// DON'T EDIT THIS FILE.
// This file is auto generated by sbt-lock 0.6.2.
// https://github.com/tkawachi/sbt-lock/
dependencyOverrides in Compile ++= {
  if (!(sbtLockHashIsUpToDate in ThisBuild).value && sbtLockIgnoreOverridesOnStaleHash.value) {
    Seq.empty
  } else {
    Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "ch.qos.logback" % "logback-core" % "1.2.3",
      "com.chuusai" % "shapeless_2.12" % "2.3.3",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.8",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.8.8",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.8",
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.8.8",
      "com.fasterxml.jackson.module" % "jackson-module-paranamer" % "2.8.8",
      "com.fasterxml.jackson.module" % "jackson-module-scala_2.12" % "2.8.8",
      "com.github.fommil.netlib" % "core" % "1.1.2",
      "com.github.julien-truffaut" % "monocle-core_2.12" % "2.0.1",
      "com.github.swagger-akka-http" % "swagger-akka-http_2.12" % "0.9.2",
      "com.github.wendykierp" % "JTransforms" % "3.1",
      "com.google.code.findbugs" % "annotations" % "2.0.1",
      "com.google.guava" % "guava" % "20.0",
      "com.google.protobuf" % "protobuf-java" % "3.5.1",
      "com.ibm.icu" % "icu4j" % "62.1",
      "com.lihaoyi" % "fastparse-utils_2.12" % "1.0.0",
      "com.lihaoyi" % "fastparse_2.12" % "1.0.0",
      "com.lihaoyi" % "sourcecode_2.12" % "0.1.4",
      "com.squareup.okhttp3" % "okhttp" % "3.12.1",
      "com.squareup.okio" % "okio" % "1.15.0",
      "com.thesamet.scalapb" % "lenses_2.12" % "0.7.0",
      "com.thesamet.scalapb" % "scalapb-json4s_2.12" % "0.7.0",
      "com.thesamet.scalapb" % "scalapb-runtime_2.12" % "0.7.4",
      "com.thoughtworks.paranamer" % "paranamer" % "2.8",
      "com.typesafe" % "config" % "1.3.3",
      "com.typesafe" % "ssl-config-core_2.12" % "0.2.1",
      "com.typesafe.akka" % "akka-actor_2.12" % "2.5.26",
      "com.typesafe.akka" % "akka-http-core_2.12" % "10.0.15",
      "com.typesafe.akka" % "akka-http_2.12" % "10.0.15",
      "com.typesafe.akka" % "akka-parsing_2.12" % "10.0.15",
      "com.typesafe.akka" % "akka-slf4j_2.12" % "2.4.17",
      "com.typesafe.akka" % "akka-stream_2.12" % "2.4.20",
      "commons-net" % "commons-net" % "3.6",
      "io.circe" % "circe-core_2.12" % "0.13.0",
      "io.circe" % "circe-generic_2.12" % "0.11.1",
      "io.circe" % "circe-jawn_2.12" % "0.11.1",
      "io.circe" % "circe-literal_2.12" % "0.11.1",
      "io.circe" % "circe-numbers_2.12" % "0.13.0",
      "io.circe" % "circe-optics_2.12" % "0.13.0",
      "io.circe" % "circe-parser_2.12" % "0.11.1",
      "io.kamon" % "kamon-bundle_2.12" % "2.0.6",
      "io.kamon" % "kamon-core_2.12" % "2.0.4",
      "io.kamon" % "kamon-influxdb_2.12" % "2.0.0",
      "io.kamon" % "kamon-zipkin_2.12" % "2.0.1",
      "io.swagger" % "swagger-annotations" % "1.5.13",
      "io.swagger" % "swagger-core" % "1.5.13",
      "io.swagger" % "swagger-jaxrs" % "1.5.13",
      "io.swagger" % "swagger-models" % "1.5.13",
      "io.swagger" % "swagger-scala-module_2.12" % "1.0.4",
      "io.zipkin.reporter2" % "zipkin-reporter" % "2.7.14",
      "io.zipkin.reporter2" % "zipkin-sender-okhttp3" % "2.7.14",
      "io.zipkin.zipkin2" % "zipkin" % "2.12.0",
      "javax.validation" % "validation-api" % "1.1.0.Final",
      "javax.ws.rs" % "jsr311-api" % "1.1.1",
      "net.jpountz.lz4" % "lz4" % "1.3.0",
      "net.sf.opencsv" % "opencsv" % "2.3",
      "net.sourceforge.f2j" % "arpack_combined_all" % "0.1",
      "org.apache.commons" % "commons-lang3" % "3.2.1",
      "org.apache.commons" % "commons-math3" % "3.5",
      "org.bitlet" % "weupnp" % "0.1.4",
      "org.bouncycastle" % "bcprov-jdk15on" % "1.54",
      "org.graalvm.js" % "js" % "19.2.0",
      "org.graalvm.regex" % "regex" % "19.2.0",
      "org.graalvm.sdk" % "graal-sdk" % "19.2.0",
      "org.graalvm.truffle" % "truffle-api" % "19.2.0",
      "org.javassist" % "javassist" % "3.18.2-GA",
      "org.json4s" % "json4s-ast_2.12" % "3.5.2",
      "org.json4s" % "json4s-core_2.12" % "3.5.2",
      "org.json4s" % "json4s-jackson_2.12" % "3.5.1",
      "org.json4s" % "json4s-native_2.12" % "3.5.2",
      "org.json4s" % "json4s-scalap_2.12" % "3.5.2",
      "org.ow2.asm" % "asm" % "6.2.1",
      "org.ow2.asm" % "asm-analysis" % "6.2.1",
      "org.ow2.asm" % "asm-commons" % "6.2.1",
      "org.ow2.asm" % "asm-tree" % "6.2.1",
      "org.ow2.asm" % "asm-util" % "6.2.1",
      "org.reactivestreams" % "reactive-streams" % "1.0.0",
      "org.reflections" % "reflections" % "0.9.10",
      "org.scala-lang.modules" % "scala-collection-compat_2.12" % "2.1.1",
      "org.scala-lang.modules" % "scala-java8-compat_2.12" % "0.8.0",
      "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.4",
      "org.scala-lang.modules" % "scala-xml_2.12" % "1.2.0",
      "org.scalanlp" % "breeze-macros_2.12" % "1.0",
      "org.scalanlp" % "breeze_2.12" % "1.0",
      "org.scorexfoundation" % "iodb_2.12" % "0.3.2",
      "org.scorexfoundation" % "scrypto_2.12" % "1.2.3",
      "org.slf4j" % "slf4j-api" % "1.7.25",
      "org.typelevel" % "algebra_2.12" % "2.0.0-M2",
      "org.typelevel" % "cats-core_2.12" % "2.1.0",
      "org.typelevel" % "cats-free_2.12" % "2.1.0",
      "org.typelevel" % "cats-kernel_2.12" % "2.1.0",
      "org.typelevel" % "cats-macros_2.12" % "2.1.0",
      "org.typelevel" % "jawn-parser_2.12" % "0.14.1",
      "org.typelevel" % "machinist_2.12" % "0.6.8",
      "org.typelevel" % "macro-compat_2.12" % "1.1.1",
      "org.typelevel" % "spire-macros_2.12" % "0.17.0-M1",
      "org.typelevel" % "spire-platform_2.12" % "0.17.0-M1",
      "org.typelevel" % "spire-util_2.12" % "0.17.0-M1",
      "org.typelevel" % "spire_2.12" % "0.17.0-M1",
      "org.whispersystems" % "curve25519-java" % "0.4.1",
      "org.yaml" % "snakeyaml" % "1.17",
      "pl.edu.icm" % "JLargeArrays" % "1.5"
    )
  }
}
// LIBRARY_DEPENDENCIES_HASH 1a3a6ef9c89eb4cbf6efe5682743d995bc326e90
