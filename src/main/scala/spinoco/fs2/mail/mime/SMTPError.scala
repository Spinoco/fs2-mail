package spinoco.fs2.mail.mime

/** Raised on SMTP Error **/
case class SMTPError(response: SMTPResponse) extends Throwable(s"SMTP Error [${response.code}] : ${response.data} ")
