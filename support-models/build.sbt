import LibraryVersions.{circeVersion, catsVersion}

name := "support-models"

description := "Scala library to provide shared step-function models to Guardian Support projects."

libraryDependencies ++= Seq(
  "com.gu" %% "support-internationalisation" % "0.12",
  "com.gu" %% "acquisition-event-producer-play26" % "4.0.18",
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion
)


