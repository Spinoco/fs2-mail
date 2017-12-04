package spinoco.fs2.mail

import java.nio.charset.Charset

import com.beetstra.jutf7.CharsetProvider
import spinoco.protocol.common.util.attempt
import scodec.Attempt

package object encoding {

  private val ModifiedUtf7: Charset = {
    val provider = new CharsetProvider()
    provider.charsetForName("X-IMAP-MODIFIED-UTF-7")
  }

  private val ASCII: Charset = Charset.forName("ASCII")

  /**
    * Encodes UTF-7 modified according to RFC3501 5.1.3
    */
  def encodeIMAPUtf7(s: String): Attempt[String] = {
    attempt { new String(s.getBytes(ModifiedUtf7), ASCII) }
  }

  /**
    * Decodes UTF-7 modified according to RFC3501 5.1.3
    */
  def decodeIMAPUtf7(s: String): Attempt[String] = {
    attempt { new String(s.getBytes(ASCII), ModifiedUtf7) }
  }


}
