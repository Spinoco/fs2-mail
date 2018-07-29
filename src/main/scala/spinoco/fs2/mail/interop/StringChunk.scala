package spinoco.fs2.mail.interop

import fs2.Chunk

final class StringChunk(val stringValue: String) extends Chunk[Char] {
  def size: Int = stringValue.size

  def apply(i: Int): Char = stringValue(i)

  protected def splitAtChunk_(n: Int): (Chunk[Char], Chunk[Char]) = {
    val (h, t) = stringValue.splitAt(n)
    (StringChunk(h), StringChunk(t))
  }

  def copyToArray[O2 >: Char](xs: Array[O2], start: Int): Unit = {
    stringValue.iterator.copyToArray(xs, start)
  }
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
