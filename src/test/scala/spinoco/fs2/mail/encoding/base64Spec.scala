package spinoco.fs2.mail.encoding

import fs2.{Chunk, Stream, Task}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._
import scodec.bits.Bases.{Alphabets, Base64Alphabet}
import scodec.bits.ByteVector
import shapeless.the
import spinoco.fs2.mail.interop.ByteVectorChunk

object base64Spec extends Properties("base64") {


  case class EncodingSample(chunkSize:Int, text:String, alphabet: Base64Alphabet)

  implicit val encodingTestInstance : Arbitrary[EncodingSample] = Arbitrary {
    for {
      s <- the[Arbitrary[String]].arbitrary
      chunkSize <- Gen.choose(1,s.length max 1)
      alphabet <- Gen.oneOf(Seq(Alphabets.Base64Url, Alphabets.Base64))
    } yield EncodingSample(chunkSize, s, alphabet)
  }

  property("encodes.base64") = forAll { sample: EncodingSample =>
    Stream.chunk[Task,Byte](Chunk.bytes(sample.text.getBytes)).chunkLimit(sample.chunkSize).flatMap(Stream.chunk)
      .through(base64.encodeRaw(sample.alphabet))
      .chunks
      .fold(ByteVector.empty){ case (acc, n) => acc ++ ByteVectorChunk.asByteVector(n)}
      .map(_.decodeUtf8)
      .runLog.unsafeRun() ?= Vector(
      Right(ByteVector.view(sample.text.getBytes).toBase64(sample.alphabet))
    )
  }


  property("decodes.base64") = forAll { sample: EncodingSample =>
    val encoded = ByteVector.view(sample.text.getBytes).toBase64(sample.alphabet)
    Stream.chunk[Task,Byte](Chunk.bytes(encoded.getBytes))
      .chunkLimit(sample.chunkSize).flatMap(Stream.chunk)
      .through(base64.decodeRaw(sample.alphabet))
      .chunks
      .fold(ByteVector.empty){ case (acc, n) => acc ++ ByteVectorChunk.asByteVector(n)}
      .map(_.decodeUtf8)
      .runLog.unsafeRun() ?= Vector(
      Right(sample.text)
    )
  }

  property("encodes.decodes.base64") =  forAll { sample: EncodingSample =>
    val r =
      Stream.chunk[Task,Byte](Chunk.bytes(sample.text.getBytes)).chunkLimit(sample.chunkSize).flatMap(Stream.chunk)
        .through(base64.encodeRaw(sample.alphabet))
        .through(base64.decodeRaw(sample.alphabet))
        .chunks
        .fold(ByteVector.empty){ case (acc, n) => acc ++ ByteVectorChunk.asByteVector(n)}
        .runLog.unsafeRun()



    r ?= Vector(ByteVector.view(sample.text.getBytes))

  }

}
