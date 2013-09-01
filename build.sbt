
// Keep in sync with the Maven pom.xml file!
// See http://www.scala-sbt.org/release/docs/Community/Using-Sonatype.html for how to publish to
// Sonatype, using sbt only.

name := "yaidom-xs"

organization := "eu.cdevreeze.yaidom"

version := "0.1.2-SNAPSHOT"

scalaVersion := "2.10.1"

// crossScalaVersions := Seq("2.10.1")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "eu.cdevreeze.yaidom" %% "yaidom" % "0.6.13"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.apache.ws.xmlschema" % "xmlschema-core" % "2.0.3"

libraryDependencies += "com.sun.xsom" % "xsom" % "20110809"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { repo => false }

pomExtra := {
  <url>https://github.com/dvreeze/yaidom-xs</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>Yaidom is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/yaidom-xs.git</connection>
    <url>https://github.com/dvreeze/yaidom-xs.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/yaidom-xs.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>
}
