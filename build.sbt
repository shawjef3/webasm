lazy val webasm =
  project.in(file(".")).
    aggregate(ast, unsigned).
    settings(
      publish := false
    )

lazy val ast =
  project.in(file("ast")).
    dependsOn(unsigned)

lazy val unsigned =
  project.in(file("unsigned"))
