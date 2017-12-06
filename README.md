# fs2-mail

Asynchronous, non-blocking IMAP/SMTP client for receiving and sending e-mails for scala.

[![Build Status](https://travis-ci.org/Spinoco/fs2-mail.svg?branch=master)](https://travis-ci.org/Spinoco/fs2-mail)
[![Gitter Chat](https://badges.gitter.im/functional-streams-for-scala/fs2.svg)](https://gitter.im/fs2-mail/Lobby)

## Overview

fs2-mail is simple, minimalistic, streaming, asynchronous and non-blocking client for receiving and sending emails 
for scala implemented in fs2 library. Traditionally sending and receiving emails in JVM world is done with 
Java's JavaMail API, that allows rich set of functionality. Albeit rich and powerful, JavaMail 
is build on top of fully blocking API. That may present limitations when working with asynchronous and 
streaming libraries, and that is exactly where fs2-mail fits in.  

Unlike JavaMail, fs2-mail is fully non-blocking, streaming allowing to process emails with minimal memory constrains 
with full utilization of modern multi-core CPUs. All processing (encoding, decoding) of emails 
is fully asynchronous and non-blocking. So for example receiving or sending emails over slower network 
connections will work perfectly fine and will automatically adjust to speed of the target/source server. That way, 
you can write clients that may send and receive hundreds or thousands emails concurrently fully saturating 
server and internet connection capacity. 

 
## Features

fs-mail currently implements: 

- IMAP to receive emails
- SMTP to send emails

Both variants may use SSL depending whether clients are created with plaintext or SSL socket. 


## SBT

Add this to your sbt build file :

for fs2 0.10.x series:

```
 // fs2-mail is currently not available for fs2 0.10.x
```

for fs2 0.9.x series:

```
libraryDependencies += "com.spinoco" %% "fs2-mail" % "0.1.0"
```


### Dependencies

version  |    scala  |   fs2  |  scodec | shapeless      
---------|-----------|--------|---------|----------- 
0.1.0    | 2.11, 2.12| 0.9.8  | 1.10.3  | 2.3.3


## Usage

fs2-mail has simple, minimalistic API, that allows straightforward usage. 

### Sending emails

To send emails, fs2-mail uses SMTP protocol. Following is the simple example of minimalistic client that 
uses single SMTP connection to send single message: 

```scala
import java.net.InetSocketAddress
import java.time.ZonedDateTime

import fs2._
import fs2.util.syntax._
import fs2.crypto.TLSEngine
import fs2.crypto.io.tcp.TLSSocket

import shapeless.tag

import spinoco.fs2.mail.smtp
import spinoco.fs2.mail.interop.StringChunk
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.EMailHeader




io.tcp.client[Task](new InetSocketAddress("imap.gmail.com", 993)).flatMap { tcpSocket =>
Stream.eval(TLSEngine[Task](clientTLS)).flatMap { tlsEngine =>
Stream.eval(TLSSocket[Task](tcpSocket, tlsEngine)).flatMap { tlsSocket =>
smtp.client(tlsSocket).evalMap { smtpClient => 

    val header: EMailHeader = EMailHeader(
      subject = "Hello from fs2-mail"
      , date = ZonedDateTime.now
      , from = EmailAddress("alice", "mail.com", None)
      , to = EmailAddress("bob", "mail.com", None)
    )
    val body: Stream[Task, Char] = Stream.chunk(StringChunk("A simple text-only emaol from fs2-mail."))

    smtpClient.connect("gmail.com") >>
    smtpClient.login("userName", "password") >>
    smtpClient.sendText(tag[EmailAddress]("alice@mail.com"), tag[EmailAddress]("bob@mail.com"), header, body)

}}}}.run.unsafeRun()

```

Apart from sending simple emails, fs2-mail also allows to send MIME encoded (inclusive multpart) data. 
To send such emails, that perhaps fecth their content not only from the strinctly defined dat but perhaps 
from data stored on filesystem or central data storage following construct (replacing the `smtp.sendText` in example above)
shall be used : 

```scala



```

### Receiving emails

To receive emails, fs2 supports IMAP v 4.1 protocol. User may search emails, perform various email actions,
download attachment (with automatical decoding according to their mime type). 

Minimalistic example of fetching ... 

```scala

import java.net.InetSocketAddress
import java.time.ZonedDateTime

import fs2._
import fs2.util.syntax._
import fs2.crypto.TLSEngine
import fs2.crypto.io.tcp.TLSSocket

import shapeless.tag

import spinoco.fs2.mail.imap 
import spinoco.fs2.mail.imap.MailboxName
import spinoco.fs2.mail.imap.IMAPSearchTerm


 io.tcp.client[Task](new InetSocketAddress("imap.gmail.com", 993)).flatMap { tcpSocket =>
  Stream.eval(TLSEngine[Task](clientTLS)) flatMap { tlsEngine =>
  Stream.eval(TLSSocket[Task](tcpSocket, tlsEngine)) flatMap { tlsSocket =>
  imap.client[Task](tlsSocket) evalMap { imapClient =>
    // login with supplied credentials
    imapClient.login("EMAIL", "PASSWORD") >>
    // check for imap server capabilities 
    imapClient.capability.flatMap { imapServerCapabilities =>
    // list mailboxes given specific criteria
    imapClient.list("", "*").flatMap { imapServerMailboxes => 
    // select email mailbox
    imapClient.select(tag[MailboxName]("INBOX")).flatMap { inboxStatus =>
    // search mailbox for mail with specified id
    imapClient.search(IMAPSearchTerm.Header("Message-ID", "your-message-id")).flatMap { searchResult =>   
    // print email headers from 42000 to 4238
    imapClient.emailHeaders(4200l to 4248l).map { hdr => println(s"HEADER: $hdr") }.run  
      

    }}}}

  }}}}
  .run.unsafeRun

```



