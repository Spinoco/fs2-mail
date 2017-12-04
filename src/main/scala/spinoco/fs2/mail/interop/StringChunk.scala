package spinoco.fs2.mail.interop

import fs2.Chunk

final class StringChunk(val stringValue: String) extends Chunk[Char] {
  def size: Int = stringValue.size

  def apply(i: Int): Char = stringValue(i)

  def copyToArray[B >: Char](xs: Array[B], start: Int): Unit =
    stringValue.getChars(start, start + xs.length, xs.asInstanceOf[Array[Char]], 0)

  def drop(n: Int): Chunk[Char] =
    StringChunk(stringValue.drop(n))

  def take(n: Int): Chunk[Char] =
    StringChunk(stringValue.take(n))

  def filter(f: Char => Boolean): Chunk[Char] =
    StringChunk(stringValue.filter(f))

  def foldLeft[B](z: B)(f: (B, Char) => B): B =
    stringValue.foldLeft(z)(f)

  def foldRight[B](z: B)(f: (Char, B) => B): B =
    stringValue.foldRight(z)(f)
}


object StringChunk {

  def apply(s:String): StringChunk = new StringChunk(s)

   def asString(chunk: Chunk[Char]): String = {
     chunk match {
       case sc: StringChunk => sc.stringValue
       case _ => new java.lang.String(chunk.toArray)
     }
   }

}
