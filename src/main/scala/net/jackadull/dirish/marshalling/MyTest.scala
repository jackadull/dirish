package net.jackadull.dirish.marshalling

object MyTest extends App {
  val source = """$HOME/p: 330abe9b-7756-471d-9e23-ae970a823560 {
                 |  prv {
                 |    tools {
                 |      dirish: 9582bd4c-a4ff-4284-82a4-af9429b24757 {
                 |      	git {origin ssh:madoc@foo.bar.baz, alt ssh:mdeja@boo.faz}
                 |      }
                 |    }
                 |  }
                 |}""".stripMargin

  ProjectConfigParser.parse(ProjectConfigParser.root, source) match {
    case ProjectConfigParser.Success(result, _) ⇒ println(s"Success!\n\n${RenderProjectConfig(ProjectConfigToToken(TokenToProjectConfig(result).right.get))}")
    case somethingElse ⇒ println(s"No success: $somethingElse")
  }
}
