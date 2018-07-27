package spinoco.fs2.mail.encoding

import java.nio.charset.{Charset, StandardCharsets}

import cats.effect.IO
import fs2.Chunk.ByteVectorChunk
import fs2._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}
import scodec.bits.ByteVector



object charsetSpec extends Properties("charset") {


  case class EncodedString(s: String, chs: Charset, chunks: Int)

  val genEncodedString: Gen[EncodedString] = {

    val eightBitWordBase =
      Seq(
        """Lorem ipsum dolor sit amet, novum tollit delenit per at, tale possim nusquam qui ex, sea te unum intellegat. Cu quo audiam aliquam, id vis elitr epicuri. No timeam maiorum consulatu sea, vel ex exerci euismod constituto. Iudico labore cum et, vis etiam populo doctus no. Usu in essent aliquid docendi, vide omnis nam at. Putent scriptorem qui eu. Has in magna etiam, eu graeci periculis ius"""
      ).mkString.toCharArray.toList

    val wordBase =
      (Seq(
        """Лорем ипсум долор сит амет, сед ид фугит инермис посидониум, инермис персецути не вис. Популо сингулис ат сед, вис одио нумяуам ин, вим номинати симилияуе еу. Ат иус децоре цаусае интерессет, еа вери урбанитас вел. Десерунт сплендиде репудиандае те нам"""
        , """Λορεμ ιπσθμ δολορ σιτ αμετ, εαμ vολθπτθα ερροριβθσ νο, εθμ νο φερρι αθδιαμ αππετερε. Ιν μελ αλιqθαμ cονvενιρε τινcιδθντ, ετ δθο ειθσ νοστερ ιντελλεγαμ. Δολορεσ ιντερπρεταρισ εθ vελ, ερρεμ ελειφενδ αν vελ, λιβρισ ινιμιcθσ qθι ιν. Ηισ νολθισσε δισσεντιθντ θτ, λαβορεσ πηαεδρθμ πρι νε, vελ παθλο περιcθλισ ιν"""
        , """तकनिकल प्राधिकरन कार्यसिधान्तो डाले। कीने उदेश सादगि प्राप्त बीसबतेबोध करके(विशेष हुआआदी वैश्विक पहेला होभर सारांश असरकारक विश्लेषण मुख्य सकती हिंदी समस्याओ आंतरजाल करके सुचना वर्ष उसीएक् क्षमता। प्रतिबध बहतर बाजार परस्पर है।अभी केन्द्रिय बारे चिदंश रहारुप अंग्रेजी किके पासपाई निर्माण उपयोगकर्ता समजते दारी हिंदी अंतर्गत अधिकांश अमितकुमार मेंभटृ असरकारक परिभाषित सीमित ७हल"""
        , """気始画立詳装英天膨司島上人好購団新出定織。貨北学川速次間太千禁与貴午取場事協名写安。内虫熱開河捨西望質間質府。安級議合写禁不渉時充段逮投所予椅北。用手読険表小界私苗除誌国車現点都深情済。着科点熱子月法橋第張務企門抗事多努格倍流。及知出小町東音就加明士曲変生触供入何。月登少省約育画供帰衛説初不漁朝表図"""
        , """人ねほ打勇コツ町2発ヨ福定ほよレぽ線深 メモ発新ぞすッよ力年クネツ免激監くずぶ だ確毎問す采箱ト聞発ヱ速続イえ写敬 抗ラわとぽ。機ょほえい額社 ホチ混藤ユクオヨ視夫くやご多用 みクれえ院告マ題余へ応体関ヌ当円イサ刻種名ウヌホ職属ホリヘ昇珍望ヒ奈1立わぼ舎冒培ラ。報妨ンま上64唆2案クケ番造提食ーび順検ぶでまび初場てク論70令ぱクどね月旬ぎ童気オノ合害阪給固かも"""
        , """As Halm Himmel jeitzt rou, dé laanscht schaddreg nun. Fir vu alles d'Mier d'Welt. Frou eraus hinnen de dén. Sin fu jéngt gudden Poufank, iech wuel Fläiß fu den, et dir sinn Frot"""
      ).mkString ++ eightBitWordBase).toCharArray.toList

    val charsets = List(
      StandardCharsets.UTF_8
      , StandardCharsets.UTF_16
      , StandardCharsets.UTF_16BE
      , StandardCharsets.UTF_16LE
      , StandardCharsets.ISO_8859_1
      , StandardCharsets.US_ASCII
    )

    for {
      chs <- Gen.oneOf(charsets)
      base = if (chs == StandardCharsets.ISO_8859_1 || chs == StandardCharsets.US_ASCII) eightBitWordBase else wordBase
      length <- Gen.choose(0, 2000)
      string <- Gen.listOfN(length, Gen.oneOf(base)).map(_.mkString)
      chunkSize <- Gen.choose(1, 50)
    } yield EncodedString(string, chs, chunkSize)


  }


  property("encodes.with.charset") =  forAll(genEncodedString) { case EncodedString(s, chs, sz) =>

    val encodedExpect = ByteVector.view(s.getBytes(chs))

    val decoded =
      Stream.chunk(ByteVectorChunk(encodedExpect)).covary[IO]
        .chunkLimit(sz).flatMap(ch => Stream.chunk(ch))
        .through(charset.decode(chs))
        .through(charset.stringChunks)
        .compile.toVector.map(_.mkString)
        .unsafeRunSync()

    val encoded =
      Stream.emit(s).covary[IO]
      .through(charset.charStream)
      .chunkLimit(sz).flatMap(ch => Stream.chunk(ch))
      .through(charset.encode(chs))
      .chunks.map { ch =>
        val bs = ch.toBytes
        ByteVector.view(bs.values, bs.offset, bs.size)
      }
      .compile.toVector.map { v => ByteVector.view(new String(v.reduceOption( _ ++ _).getOrElse(ByteVector.empty).toArray, chs).getBytes(chs))  }
      .unsafeRunSync()

    (decoded ?= s) &&
    (encoded ?= encodedExpect)
  }


}
