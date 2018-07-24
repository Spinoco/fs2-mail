package spinoco.fs2.mail.encoding

import cats.effect.IO
import fs2._
import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop._

object quotedPrintableSpec extends Properties("quotedPrintable") {

  def decodeAndEncode(quoted: Seq[String], raw: Seq[String], chunkSize:Int = Int.MaxValue): Prop = {
    val toDecode = quoted.mkString("\r\n")
    val toEncode = raw.mkString("\r\n")

    val decoded =
      Stream.chunk(Chunk.bytes(toDecode.getBytes)).covary[IO]
      .chunkLimit(chunkSize).flatMap(ch => Stream.chunk(ch))
      .through(quotedPrintable.decode)
      .through(text.utf8Decode)
      .through(text.lines)
      .compile.toVector
      .unsafeRunSync()

    val encoded =
      Stream.chunk(Chunk.bytes(toEncode.getBytes)).covary[IO]
      .chunkLimit(chunkSize).flatMap(ch => Stream.chunk(ch))
      .through(quotedPrintable.encode)
      .through(text.utf8Decode)
      .through(text.lines)
      .compile.toVector
      .unsafeRunSync()

    def dump(expect: Seq[String], result: Seq[String]): String = {
      expect.zip(result).zipWithIndex.flatMap { case ((e, r), idx) =>
       Seq(
         s"#: $idx (${e == r})"
         , "E:" + e
         , "R:" + r
       )
      }.mkString("\r\n")
    }

    ((decoded ?= raw.toVector) :| s"Failed to decode (${raw.size}, ${decoded.size}) : \r\n${dump(raw, decoded)}") &&
      ((encoded ?= quoted.toVector) :| s"Failed to encode (${quoted.size}, ${encoded.size}): \r\n${dump(quoted, encoded)} ")
  }



  property("encode-decode.sample1") = forAll(Gen.chooseNum(1, 2000)) { chunkSize =>
    decodeAndEncode(
      quoted =
        """J'interdis aux marchands de vanter trop leur marchandises. Car ils se font =
          |vite p=C3=A9dagogues et t'enseignent comme but ce qui n'est par essence qu'=
          |un moyen, et te trompant ainsi sur la route =C3=A0 suivre les voil=C3=A0 bi=
          |ent=C3=B4t qui te d=C3=A9gradent, car si leur musique est vulgaire ils te f=
          |abriquent pour te la vendre une =C3=A2me vulgaire."""
          .stripMargin.lines.toSeq
      , raw = Seq(
        """J'interdis aux marchands de vanter trop leur marchandises. Car ils se font vite pédagogues et t'enseignent comme but ce qui n'est par essence qu'un moyen, et te trompant ainsi sur la route à suivre les voilà bientôt qui te dégradent, car si leur musique est vulgaire ils te fabriquent pour te la vendre une âme vulgaire."""
      )
      , chunkSize = chunkSize
    )
  }


  property("encode-decode.sample2") = forAll(Gen.chooseNum(1, 2000)) { chunkSize =>
    decodeAndEncode(
      quoted =
        """Die Hasen und die Fr=C3=B6sche
          |
          |Die Hasen klagten einst =C3=BCber ihre mi=C3=9Fliche Lage; "wir leben", spr=
          |ach ein
          |Redner, "in steter Furcht vor Menschen und Tieren, eine Beute der Hunde, de=
          |r
          |Adler, ja fast aller Raubtiere! Unsere stete Angst ist =C3=A4rger als der T=
          |od
          |selbst. Auf, la=C3=9Ft uns ein f=C3=BCr allemal sterben."
          |
          |In einem nahen Teich wollten sie sich nun ers=C3=A4ufen; sie eilten ihm zu;
          |allein das au=C3=9Ferordentliche Get=C3=B6se und ihre wunderbare Gestalt er=
          |schreckte
          |eine Menge Fr=C3=B6sche, die am Ufer sa=C3=9Fen, so sehr, da=C3=9F sie aufs=
          | schnellste
          |untertauchten.
          |
          |"Halt", rief nun eben dieser Sprecher, "wir wollen das Ers=C3=A4ufen noch e=
          |in
          |wenig aufschieben, denn auch uns f=C3=BCrchten, wie ihr seht, einige Tiere,
          |welche also wohl noch ungl=C3=BCcklicher sein m=C3=BCssen als wir.""""
          .stripMargin.lines.toSeq
      , raw = Seq(
        "Die Hasen und die Frösche"
        , ""
        , "Die Hasen klagten einst über ihre mißliche Lage; \"wir leben\", sprach ein"
        , "Redner, \"in steter Furcht vor Menschen und Tieren, eine Beute der Hunde, der"
        , "Adler, ja fast aller Raubtiere! Unsere stete Angst ist ärger als der Tod"
        , "selbst. Auf, laßt uns ein für allemal sterben.\""
        , ""
        , "In einem nahen Teich wollten sie sich nun ersäufen; sie eilten ihm zu;"
        , "allein das außerordentliche Getöse und ihre wunderbare Gestalt erschreckte"
        , "eine Menge Frösche, die am Ufer saßen, so sehr, daß sie aufs schnellste"
        , "untertauchten."
        , ""
        , "\"Halt\", rief nun eben dieser Sprecher, \"wir wollen das Ersäufen noch ein"
        , "wenig aufschieben, denn auch uns fürchten, wie ihr seht, einige Tiere,"
        , "welche also wohl noch unglücklicher sein müssen als wir.\""
      )
      , chunkSize = chunkSize
    )


  }



}
