package co.topl.it.util

import akka.actor.ActorSystem

import java.util.UUID
import co.topl.settings.AppSettings
import co.topl.utils.Logging
import co.topl.settings.AppSettings
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient.ListImagesParam
import com.spotify.docker.client.messages.{Container, ContainerConfig}
import com.typesafe.config.{Config, ConfigFactory}
import org.asynchttpclient.Dsl.{asyncHttpClient, config}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.asScalaBufferConverter
import scala.util.control.NonFatal
import scala.collection.JavaConverters._
class Docker()(implicit ec: ExecutionContext, system: ActorSystem) extends AutoCloseable with Logging {

  private val http = asyncHttpClient(
    config()
      .setMaxConnections(18)
      .setMaxConnectionsPerHost(3)
      .setMaxRequestRetry(1)
      .setReadTimeout(10000)
      .setKeepAlive(false)
      .setRequestTimeout(10000)
  )

  private implicit val client: DefaultDockerClient = DefaultDockerClient.fromEnv().build()
  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    log.debug("Shutdown hook")
    close()
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[Node] = {
    log.debug(s"Starting ${nodeConfigs.size} containers")
    val nodes = nodeConfigs.map(startNode)
    Await.result(
      Future.traverse(nodes)(_.waitForStartup),
      5.minutes
    )
    nodes
  }

  private def startNode(nodeConfig: Config): Node = {
    val settings = buildAppSettings(nodeConfig)
    val containerConfig: ContainerConfig = buildContainerConfig(settings)
    val containerName: String = networkName + "-" + settings.network.nodeName
    val imageName =
      Docker.bifrostImage.split('/').last
    if(client.listImages(ListImagesParam.byName(imageName)).asScala.isEmpty) {
      log.debug(s"${Docker.bifrostImage} does not exist locally.  Pulling.")
      client.pull(Docker.bifrostImage)
    }
    val containerId: String = client.createContainer(containerConfig, containerName).id
    client.startContainer(containerId)
    Node(settings, containerId, 9084, 9085)
  }

  def waitForStartup(nodes: List[Node]): Future[List[Node]] =
    Future.sequence(nodes map { _.waitForStartup })

  private def buildAppSettings(nodeConfig: Config) = {
    val actualConfig = nodeConfig
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
    AppSettings.fromConfig(actualConfig)
  }

  private def buildContainerConfig(settings: AppSettings): ContainerConfig = {
    ContainerConfig
      .builder()
      .image(Docker.bifrostImage)
      .exposedPorts(settings.network.bindAddress.getPort.toString, settings.rpcApi.bindAddress.getPort.toString)
      .build()
  }

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x =
      if (containers.isEmpty) "No"
      else
        "\n" + containers.asScala
          .map { x =>
            s"Container(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})"
          }
          .mkString("\n")

    log.debug(s"$label: $x")
  }

  override def close(): Unit = {}
}

object Docker {

  val bifrostImage: String = "bifrost:1.3.4"
  val networkNamePrefix: String = "bifrost-it"
}
