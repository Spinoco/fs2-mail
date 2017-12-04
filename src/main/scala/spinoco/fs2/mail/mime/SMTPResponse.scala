package spinoco.fs2.mail.mime


case class SMTPResponse(code: SMTPResponse.Code.Value, data: String)

object SMTPResponse {

  object Code extends Enumeration {

    val SystemStatus = Value(211)       // System status, or system help reply
    val HelpMessage = Value(214)        // Help message (Information on how to use the receiver or the
                                        // meaning of a particular non-standard command; this reply is useful
                                        // only to the human user)
    val Ready = Value(220)              // <domain> Service ready
    val TxClose = Value(221)            // <domain> Service closing transmission channel
    val AuthAccepted = Value(235)       // Authentication Accepted
    val Completed = Value(250)          // Requested mail action okay, completed
    val FwdNotLocal = Value(251)        // User not local; will forward to <forward-path> (See Section 3.4)
    val CannotVerify = Value(252)       // Cannot VRFY user, but will accept message and attempt delivery
    val Continue = Value(334)           // Continue with input
    val StartMail = Value(354)          // Start mail input; end with <CRLF>.<CRLF>
    val NotAvail = Value(421)           // Service not available, closing transmission channel
                                        // (This may be a reply to any command if the service knows it must
                                        // shut down)
    val MailboxBusy = Value(450)        // Requested mail action not taken: mailbox unavailable (e.g.,
                                        // mailbox busy or temporarily blocked for policy reasons)
    val LocalError = Value(451)         // Requested action aborted: local error in processing
    val InsufficientStore = Value(452)  // Requested action not taken: insufficient system storage
    val UnsupportedParams = Value(455)  // Server unable to accommodate parameters
    val SyntaxError = Value(500)        // Syntax error, command unrecognized (This may include errors such
                                        // as command line too long)
    val InvalidParameters = Value(501)  // Syntax error in parameters or arguments
    val NotImplemented = Value(502)     // Command not implemented (see Section 4.2.4)
    val BadSequence = Value(503)        // Bad sequence of commands
    val UnknownParameter = Value(504)   // Command parameter not implemented
    val MailboxNotFound = Value(550)    // Requested action not taken: mailbox unavailable (e.g., mailbox
                                        // not found, no access, or command rejected for policy reasons)
    val NotLocal = Value(551)           // User not local; please try <forward-path> (See Section 3.4)
    val AbortedInsufficientStore = Value(552)          // Requested mail action aborted: exceeded storage allocation
    val IncorrectSyntax = Value(553)    // Requested action not taken: mailbox name not allowed (e.g.,
                                        // mailbox syntax incorrect)
    val TransactionFailed = Value(554)  //Transaction failed (Or, in the case of a connection-opening
                                        // response, "No SMTP service here")
    val InvalidAddress = Value(555)     // MAIL FROM/RCPT TO parameters not recognized or not implemented

  }

}
