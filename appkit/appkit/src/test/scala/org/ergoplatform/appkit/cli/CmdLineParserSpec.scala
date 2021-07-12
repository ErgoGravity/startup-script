package org.ergoplatform.appkit.cli

import org.ergoplatform.appkit.commands.UsageException
import org.scalatest.{PropSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CmdLineParserSpec extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  import CmdLineParser._

  property("parseOptions") {
    parseOptions(Array[String]()) shouldBe (Map.empty, Seq.empty)
    parseOptions(Array[String]("param")) shouldBe (Map.empty, Seq("param"))
    parseOptions(Array[String]("p1", "p2")) shouldBe (Map.empty, Seq("p1", "p2"))
    parseOptions(Array[String]("--dry-run")) shouldBe (Map("dry-run" -> "true"), Seq())
    an[UsageException] should be thrownBy {
      parseOptions(Array[String]("--invalidOption"))
    }
    parseOptions(Array[String]("--app.conf", "app.conf.json")) shouldBe (Map("src/main/conf" -> "app.conf.json"), Seq())
    an[UsageException] should be thrownBy {
      parseOptions(Array[String]("--app.conf"))
    }
    parseOptions(Array[String]("--app.conf", "app.conf.json", "p1")) shouldBe (Map("src/main/conf" -> "app.conf.json"), Seq("p1"))
    parseOptions(Array[String]("p1", "--app.conf", "app.conf.json", "p2")) shouldBe (Map("src/main/conf" -> "app.conf.json"), Seq("p1", "p2"))
  }
}
