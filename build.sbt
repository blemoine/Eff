name := "scala-effects"

version := "1.0"

scalaVersion := "2.12.2"

enablePlugins(TutPlugin)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "cats" % "0.9.0"
)

tutTargetDirectory := new File("./")