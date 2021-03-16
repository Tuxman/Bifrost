package co.topl.it

import akka.actor.ActorSystem

import java.util.concurrent.Executors
import co.topl.it.util.Docker
import co.topl.utils.Logging
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.ExecutionContext
import scala.util.Random

trait IntegrationSuite
  extends IntegrationConstants
    with BeforeAndAfterAll
    with Logging { this: Suite =>

  implicit val system: ActorSystem = ActorSystem("TestSuite")

  import system.dispatcher

  protected val localDataDir: String = s"/tmp/bifrost/it-${Random.nextInt(Int.MaxValue)}"

  protected val docker: Docker = new Docker()

  override def beforeAll(): Unit = {
    log.debug("Starting integration tests")
  }

  override def afterAll(): Unit = docker.close()
}
