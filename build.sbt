
// Keep in sync with the Maven pom.xml file!
// See http://www.scala-sbt.org/release/docs/Community/Using-Sonatype.html for how to publish to
// Sonatype, using sbt only.

name := "yaidom-xs"

organization := "eu.cdevreeze.yaidom"

version := "0.1.1-SNAPSHOT"

scalaVersion := "2.10.0"

crossScalaVersions := Seq("2.10.0", "2.9.2", "2.9.1", "2.9.0-1", "2.9.0")

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions <++= scalaBinaryVersion map { version =>
  if (version.contains("2.10")) Seq("-feature") else Seq()
}

libraryDependencies += "eu.cdevreeze.yaidom" %% "yaidom" % "0.6.7"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies <+= scalaBinaryVersion { version =>
  if (version.contains("2.10"))
    "org.scalatest" % "scalatest_2.10.0" % "1.8" % "test"
  else
    "org.scalatest" %% "scalatest" % "1.8" % "test"
}

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
