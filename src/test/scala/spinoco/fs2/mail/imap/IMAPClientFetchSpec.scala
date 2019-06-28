package spinoco.fs2.mail.imap

import cats.effect.IO
import org.scalacheck._
import org.scalacheck.Prop._
import fs2._
import scodec.bits.ByteVector

import spinoco.fs2.mail.interop.StringChunk
import shapeless.tag
import shapeless.tag.@@

import spinoco.fs2.mail.imap.IMAPClient.impl.{IMAPBytes, IMAPText}
import spinoco.protocol.mail.header.codec.EmailHeaderCodec

object IMAPClientFetchSpec extends Properties("IMAPClient.Fetch"){

  val emptyHeaderResponse = "* 20948 FETCH (UID 16578604 BODY[HEADER] NIL)\r\n"

  val headerSingleResponse =
    """* 4250 FETCH (UID 4268 BODY[HEADER] {5042}
      |Delivered-To: pavel.chlupacek@spinoco.com
      |Received: by 10.223.135.145 with SMTP id b17csp1326716wrb; Tue, 14 Nov 2017
      | 07:57:04 -0800 (PST)
      |X-Google-Smtp-Source: AGs4zMa7X5ziPqZHmuiRh7Rr109xcK6HN1BwSyhxfGZVxPt4n1ZH40KM8thrUDDjwsKr0/um/K7g
      |X-Received: by 10.84.138.193 with SMTP id 59mr12975759plp.446.1510675023888;
      | Tue, 14 Nov 2017 07:57:03 -0800 (PST)
      |ARC-Seal: i=1; a=rsa-sha256; t=1510675023; cv=none; d=google.com;
      | s=arc-20160816;
      | b=p9xTAI3Sff0brZExcA8LwvQwxnn+0FI+ssNGP5/gU3eHZGT4W5HVGBt2xKHyAr8oZR
      | sCl5mcTW1omcjA3NjUbVDlugpCBOvITiPJQjrRcZNIYzP52kxP0s9pMWYoKiJChpd6lx
      | hy8ecZZ7dukDPfwkGTy/RSgozeKh7B19zqrZAn/ijxbcdRWH2O7BiYvrKEqhf6tGOveh
      | NggkvNbvnMdJpph6+bOCHN5CAs5j4jBG5U5Iq9GxD07TyKe+gcmDOepoqNREpLpvp8/y
      | zpwoIyTB5i66n8Zbe24Xgc1nBTWsXqyGW1KkTSMxc9cgxxkZWUifGsY0lERKnIzJmrds iRsg==
      |ARC-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com;
      | s=arc-20160816;
      | h=require-recipient-valid-since:list-unsubscribe:date:to:mime-version
      | :subject:message-id:from:dkim-signature:dkim-signature
      | :arc-authentication-results; bh=vlIukZxbenroQc0cd2wmXkyx4+oPZT7cyYOi+0k0IiM=;
      | b=rVgk1lWigIqCbrM8FqZ8N2p8jau0BCfqrrKsSbFjiXYaBFkCGnlb8Nl1A6rmLytpfl
      | awy8l0SJ/FE+IV8Ryw6QJrHiVt3V+KAP34yW25h78Xr165F0KDf2bWeVCeVO/UkdAv7R
      | JOSE0ZJKKi5VDIYzLk8WUFcby4fAwIGAvHKTlMgU3Qm+yPXTlJDBB7seWMhrjrYMwr7/
      | s0rSuoIMSKuK4dapXSNmPUGpFg0Jod7QE4v+CcSPGYMp+VcP2QlFS9VKnBaMjlgdN/Zq
      | o8ExFVPXYNycMfg/4tW9NOwFDX7lPlwMrnOUhd6kVex5We4EXaouxhDaewCyAjvSSVKA Rafg==
      |ARC-Authentication-Results: i=1; mx.google.com; dkim=pass
      | header.i=@linkedin.com header.s=proddkim1024 header.b=B+G7gPK9; dkim=pass
      | header.i=@mailb.linkedin.com header.s=proddkim1024 header.b=bnoMHlqU;
      | spf=pass (google.com: domain of
      | s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com
      | designates 2620:109:c003:104::153 as permitted sender)
      | smtp.mailfrom=s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com;
      | dmarc=pass (p=REJECT sp=REJECT dis=NONE) header.from=linkedin.com
      |Return-Path: <s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com>
      |Received: from mailb-da.linkedin.com (mailb-da.linkedin.com.
      | [2620:109:c003:104::153]) by mx.google.com with ESMTPS id
      | 60si16147603pla.417.2017.11.14.07.57.03 for <pavel.chlupacek@spinoco.com>
      | (version=TLS1_2 cipher=ECDHE-RSA-AES128-GCM-SHA256 bits=128/128); Tue, 14 Nov
      | 2017 07:57:03 -0800 (PST)
      |Received-SPF: pass (google.com: domain of
      | s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com
      | designates 2620:109:c003:104::153 as permitted sender)
      | client-ip=2620:109:c003:104::153;
      |Authentication-Results: mx.google.com; dkim=pass header.i=@linkedin.com
      | header.s=proddkim1024 header.b=B+G7gPK9; dkim=pass
      | header.i=@mailb.linkedin.com header.s=proddkim1024 header.b=bnoMHlqU;
      | spf=pass (google.com: domain of
      | s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com
      | designates 2620:109:c003:104::153 as permitted sender)
      | smtp.mailfrom=s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com;
      | dmarc=pass (p=REJECT sp=REJECT dis=NONE) header.from=linkedin.com
      |DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=linkedin.com;
      | s=proddkim1024; t=1510675019;
      | bh=vlIukZxbenroQc0cd2wmXkyx4+oPZT7cyYOi+0k0IiM=;
      | h=From:Subject:MIME-Version:Content-Type:To:Date:X-LinkedIn-Class:
      | X-LinkedIn-Template:X-LinkedIn-fbl;
      | b=B+G7gPK96gdTZnnWeMNw60wwylbQs+HIQQX/LlWUpSLQ1aWxvzuVqKf+vx2FU9mX5
      | UNYxXmTSD6rX5/AhFzwD7UPPzEmjE4piUErbtra/AKK0GrA9qFdIyb/XA7D0PUpkBj
      | F9ypk/I3H4HCw8YxP/r2AAVyWY681JJ+C/d8VQ6M=
      |DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=mailb.linkedin.com;
      | s=proddkim1024; t=1510675019;
      | bh=vlIukZxbenroQc0cd2wmXkyx4+oPZT7cyYOi+0k0IiM=;
      | h=From:Subject:MIME-Version:Content-Type:To:Date:X-LinkedIn-Class:
      | X-LinkedIn-Template:X-LinkedIn-fbl;
      | b=bnoMHlqUL/CBUXOqB3LMPbz7Wqmdk6lQWPfPUmVdpIjP/GmT0iBAtw160+MPxVixG
      | rRLQvzHzhob6H4HgvXBiq0RTpnDTcCoRpgzbFzBTl115rnjk3DdIWaqsO3aklq2uod
      | 7GMeSmv36mngRJtaU2SamIAY867ZhZXzZlOnqchw=
      |From: LinkedIn <jobs-listings@linkedin.com>
      |Message-ID: <1994637410.2874464.1510675019013.JavaMail.app@lva1-app3759.prod.linkedin.com>
      |Subject: IP Operations
      |MIME-Version: 1.0
      |Content-Type: multipart/alternative;
      | boundary="----=_Part_2874462_211417655.1510675019006"
      |To: Pavel Chlupacek <pavel.chlupacek@spinoco.com>
      |Date: Tue, 14 Nov 2017 15:56:59 +0000 (UTC)
      |X-LinkedIn-Class: JOBS-TO-MBR
      |X-LinkedIn-Template: jobs_jymbii_digest
      |X-LinkedIn-fbl: m2-aszxtlx7udpecdghpjpk8de51nb5salzl4ctgok39kgajsudgl7t7eqorpbx1z9hdzpp89hoelzxxd851blaeioxjrdcseczfn0408
      |X-LinkedIn-Id: 1ckjt-j9zswg1k-tk
      |List-Unsubscribe: <https://www.linkedin.com/e/v2?e=1ckjt-j9zswg1k-tk&t=lun&midToken=AQHZpF-vUrxfEg&ek=jobs_jymbii_digest&li=41&m=unsub&ts=unsub&loid=AQGVV4m3M1JuUgAAAV-7P3R32K_mSezTe42jBk8xiIluEam-3NnRjWdSt48KY-0cm3WSTUu8hLWCiCax9CusW_XSKkPlpDxI048J9WsZZpnE9Q&eid=1ckjt-j9zswg1k-tk>
      |Require-Recipient-Valid-Since: pavel.chlupacek@spinoco.com; Tue, 20 Aug 2013
      | 00:43:21 +0000
      |
      |)
      |* 18617 EXISTS
      |3 OK Success
      |
      |""".stripMargin.lines.mkString("\r\n")

  val multipleHeadersResponse =
    """* 4249 FETCH (BODY[HEADER] {4774}
      |Delivered-To: pavel.chlupacek@spinoco.com
      |Received: by 10.223.135.145 with SMTP id b17csp1269216wrb; Tue, 14 Nov 2017
      | 07:02:12 -0800 (PST)
      |X-Google-Smtp-Source: AGs4zMYskldrAn2cbf3t06J3nh/CIMDbhfxrsLNQFjlw48SHgXBGTZB1pLfhP4rpiJU64NWh3WUU
      |X-Received: by 10.223.160.61 with SMTP id k58mr9553433wrk.66.1510671732535;
      | Tue, 14 Nov 2017 07:02:12 -0800 (PST)
      |ARC-Seal: i=1; a=rsa-sha256; t=1510671732; cv=none; d=google.com;
      | s=arc-20160816;
      | b=ao3WY+X1B2EvCCyUWSuLEGP0JYhYIJHod4JfImcZQD2r+/Tmyy4aORzt3tOBBrLIuS
      | xMVheyRGlP1HqFls22Pu7o60SKiniauwVKi6rzFzbgsUMjE9KtMY/VsMoDWGzR6/pPCB
      | qUxYf2Qg7CeoN2SopWIVU7rXEHzX3WkquSxCKUfvFPwU+DAtxo1O9sLLlTbzHAhdE14b
      | QVC2SH/D5MHS7PhLidDC2wlyikexR6jKJUban5HnQ9R1PtanStlWPFcp0SMMp21HmYJ8
      | BSJvQLUqjkULtAEKdyxGtlUMnuTGqJzKZtoeQNVriagE6Ntj66LdPK8D2rzcEcFdI7iP ymZA==
      |ARC-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com;
      | s=arc-20160816;
      | h=list-unsubscribe:mime-version:subject:message-id:to:reply-to:from
      | :date:dkim-signature:dkim-signature:arc-authentication-results;
      | bh=A0MbdZrmgIkt6XT6eyLPzi3uOpti0823WQHRP5YR4Pg=;
      | b=QDI1YETE7FVS8XjkZ1b7FyCFcDbUG8WYxgBjadGgA8eaWkJD7aws4FhHhaz1+/YH3Q
      | UZ3Mtck/qIVq/41OGTkVlSEZOeRIrXv0pYsCE6jUuIvXFPwo/Z9Ig4XzIPOwMX60KOxC
      | c1KzzeVKRLRccJ8beVBP4WFzgzte7eR0t7YOxhB3hpYpOwHvgg7AhBXUu3MDaclF2dMI
      | vvs4TGMJBuuVgWIzAoKPRvkONLPQHNbZJjyWufH0ZloMgzBiZBqE/9+TKHVhjXEmjmOE
      | evuEHWu1x5KlzlzFUX1/vcTMq5Sx/wNCct3rdd4HhSZly0Fc6iaf1wzZZP3rJ1Fp88IK +vRA==
      |ARC-Authentication-Results: i=1; mx.google.com; dkim=pass
      | header.i=@jetbrains.com header.s=m1 header.b=P1lPhyWc; dkim=pass
      | header.i=@mktdns.com header.s=m1 header.b=a/PCaIl+; spf=pass (google.com:
      | domain of 426-qvd-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com
      | designates 185.28.196.44 as permitted sender)
      | smtp.mailfrom=426-QVD-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com;
      | dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=jetbrains.com
      |Return-Path: <426-QVD-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com>
      |Received: from mkto-mailer.jetbrains.com (mkto-mailer.jetbrains.com.
      | [185.28.196.44]) by mx.google.com with ESMTPS id
      | n11si11354100wrh.183.2017.11.14.07.02.12 for <pavel.chlupacek@spinoco.com>
      | (version=TLS1_2 cipher=ECDHE-RSA-AES128-GCM-SHA256 bits=128/128); Tue, 14 Nov
      | 2017 07:02:12 -0800 (PST)
      |Received-SPF: pass (google.com: domain of
      | 426-qvd-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com designates
      | 185.28.196.44 as permitted sender) client-ip=185.28.196.44;
      |Authentication-Results: mx.google.com; dkim=pass header.i=@jetbrains.com
      | header.s=m1 header.b=P1lPhyWc; dkim=pass header.i=@mktdns.com header.s=m1
      | header.b=a/PCaIl+; spf=pass (google.com: domain of
      | 426-qvd-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com designates
      | 185.28.196.44 as permitted sender)
      | smtp.mailfrom=426-QVD-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com;
      | dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=jetbrains.com
      |X-MSFBL: cGF2ZWwuY2hsdXBhY2VrQHNwaW5vY28uY29tQGR2cC0xODUtMjgtMTk2LTQ0QGJn
      | LWxvbmQtMzBANDI2LVFWRC0xMTQ6MTgxOTk6MTMzMjg6NTMzMjk6MDoxOTgzODo5
      | OjIxODY1Ojc1MTUyMjE=
      |DKIM-Signature: v=1; a=rsa-sha256; q=dns/txt; c=relaxed/relaxed; t=1510671732;
      | s=m1; d=jetbrains.com; i=@jetbrains.com;
      | h=Date:From:To:Subject:MIME-Version:Content-Type;
      | bh=A0MbdZrmgIkt6XT6eyLPzi3uOpti0823WQHRP5YR4Pg=;
      | b=P1lPhyWc/IJ79qlEdPVCcGmf0clL6jS/94Q0l/z6+XBjY7rV/XKlTDBbwgSIuGUu
      | B778tYInTflql3A7AAtdO9FnV7Skcg+XCQl17JC4DSaphOTOCvPr5KbMRekBNO2Zpq7
      | Hfahz0uXt++tBLdrf9XrviqZmpy3F3VfGmoJdKRI=
      |DKIM-Signature: v=1; a=rsa-sha256; q=dns/txt; c=relaxed/relaxed; t=1510671732;
      | s=m1; d=mktdns.com; i=@mktdns.com;
      | h=Date:From:To:Subject:MIME-Version:Content-Type;
      | bh=A0MbdZrmgIkt6XT6eyLPzi3uOpti0823WQHRP5YR4Pg=;
      | b=a/PCaIl+sUdOKDk74QqXbybPjOqCJ9TW/z/VmrhdwUpEs1gRzUnJtrIAT4UbJEVw
      | 5QO4V/rbSMGVBYDs0oG2nEMvvkWkPvnLvqNV3C+zWNLUVvIKH0drreaUW+5nh0zvUJv
      | j556jZWjXPPUzUIK7ZkCcac0T99CGjmI+wjp103M=
      |Date: Tue, 14 Nov 2017 09:02:12 -0600 (CST)
      |From: JetBrains <news@jetbrains.com>
      |Reply-To: news@jetbrains.com
      |To: pavel.chlupacek@spinoco.com
      |Message-ID: <65002856.85905128.1510671732210.JavaMail.root@lon-mas2.marketo.org>
      |Subject: JetBrains Newsletter, November 2017 - ReSharper C++ 2017.2 and Kotlin
      | EduTools Plugin
      |MIME-Version: 1.0
      |Content-Type: multipart/alternative;
      | boundary="----=_Part_85905127_1257291848.1510671732210"
      |X-Binding: bg-lond-30
      |X-MarketoID: 426-QVD-114:18199:13328:53329:0:19838:9:21865:7515221
      |X-MktArchive: false
      |List-Unsubscribe: <mailto:MR2XG2TUJBGGKV22KVRXOMLEGRAWCTKDORAT2PI.21865.19838.9@unsub-lon.mktomail.com>
      |X-Mailfrom: 426-QVD-114.0.21865.0.0.19838.9.7515221@mkto-mailer.jetbrains.com
      |X-MSYS-API: {"options":{"open_tracking":false,"click_tracking":false}}
      |X-MktMailDKIM: true
      |
      | UID 4267)
      |* 4250 FETCH (UID 4268 BODY[HEADER] {5042}
      |Delivered-To: pavel.chlupacek@spinoco.com
      |Received: by 10.223.135.145 with SMTP id b17csp1326716wrb; Tue, 14 Nov 2017
      | 07:57:04 -0800 (PST)
      |X-Google-Smtp-Source: AGs4zMa7X5ziPqZHmuiRh7Rr109xcK6HN1BwSyhxfGZVxPt4n1ZH40KM8thrUDDjwsKr0/um/K7g
      |X-Received: by 10.84.138.193 with SMTP id 59mr12975759plp.446.1510675023888;
      | Tue, 14 Nov 2017 07:57:03 -0800 (PST)
      |ARC-Seal: i=1; a=rsa-sha256; t=1510675023; cv=none; d=google.com;
      | s=arc-20160816;
      | b=p9xTAI3Sff0brZExcA8LwvQwxnn+0FI+ssNGP5/gU3eHZGT4W5HVGBt2xKHyAr8oZR
      | sCl5mcTW1omcjA3NjUbVDlugpCBOvITiPJQjrRcZNIYzP52kxP0s9pMWYoKiJChpd6lx
      | hy8ecZZ7dukDPfwkGTy/RSgozeKh7B19zqrZAn/ijxbcdRWH2O7BiYvrKEqhf6tGOveh
      | NggkvNbvnMdJpph6+bOCHN5CAs5j4jBG5U5Iq9GxD07TyKe+gcmDOepoqNREpLpvp8/y
      | zpwoIyTB5i66n8Zbe24Xgc1nBTWsXqyGW1KkTSMxc9cgxxkZWUifGsY0lERKnIzJmrds iRsg==
      |ARC-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com;
      | s=arc-20160816;
      | h=require-recipient-valid-since:list-unsubscribe:date:to:mime-version
      | :subject:message-id:from:dkim-signature:dkim-signature
      | :arc-authentication-results; bh=vlIukZxbenroQc0cd2wmXkyx4+oPZT7cyYOi+0k0IiM=;
      | b=rVgk1lWigIqCbrM8FqZ8N2p8jau0BCfqrrKsSbFjiXYaBFkCGnlb8Nl1A6rmLytpfl
      | awy8l0SJ/FE+IV8Ryw6QJrHiVt3V+KAP34yW25h78Xr165F0KDf2bWeVCeVO/UkdAv7R
      | JOSE0ZJKKi5VDIYzLk8WUFcby4fAwIGAvHKTlMgU3Qm+yPXTlJDBB7seWMhrjrYMwr7/
      | s0rSuoIMSKuK4dapXSNmPUGpFg0Jod7QE4v+CcSPGYMp+VcP2QlFS9VKnBaMjlgdN/Zq
      | o8ExFVPXYNycMfg/4tW9NOwFDX7lPlwMrnOUhd6kVex5We4EXaouxhDaewCyAjvSSVKA Rafg==
      |ARC-Authentication-Results: i=1; mx.google.com; dkim=pass
      | header.i=@linkedin.com header.s=proddkim1024 header.b=B+G7gPK9; dkim=pass
      | header.i=@mailb.linkedin.com header.s=proddkim1024 header.b=bnoMHlqU;
      | spf=pass (google.com: domain of
      | s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com
      | designates 2620:109:c003:104::153 as permitted sender)
      | smtp.mailfrom=s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com;
      | dmarc=pass (p=REJECT sp=REJECT dis=NONE) header.from=linkedin.com
      |Return-Path: <s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com>
      |Received: from mailb-da.linkedin.com (mailb-da.linkedin.com.
      | [2620:109:c003:104::153]) by mx.google.com with ESMTPS id
      | 60si16147603pla.417.2017.11.14.07.57.03 for <pavel.chlupacek@spinoco.com>
      | (version=TLS1_2 cipher=ECDHE-RSA-AES128-GCM-SHA256 bits=128/128); Tue, 14 Nov
      | 2017 07:57:03 -0800 (PST)
      |Received-SPF: pass (google.com: domain of
      | s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com
      | designates 2620:109:c003:104::153 as permitted sender)
      | client-ip=2620:109:c003:104::153;
      |Authentication-Results: mx.google.com; dkim=pass header.i=@linkedin.com
      | header.s=proddkim1024 header.b=B+G7gPK9; dkim=pass
      | header.i=@mailb.linkedin.com header.s=proddkim1024 header.b=bnoMHlqU;
      | spf=pass (google.com: domain of
      | s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com
      | designates 2620:109:c003:104::153 as permitted sender)
      | smtp.mailfrom=s-4tv3u6z9tra5w76olht2p0kbpo09r2oevfhyob0kab3rd68ierluro86@bounce.linkedin.com;
      | dmarc=pass (p=REJECT sp=REJECT dis=NONE) header.from=linkedin.com
      |DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=linkedin.com;
      | s=proddkim1024; t=1510675019;
      | bh=vlIukZxbenroQc0cd2wmXkyx4+oPZT7cyYOi+0k0IiM=;
      | h=From:Subject:MIME-Version:Content-Type:To:Date:X-LinkedIn-Class:
      | X-LinkedIn-Template:X-LinkedIn-fbl;
      | b=B+G7gPK96gdTZnnWeMNw60wwylbQs+HIQQX/LlWUpSLQ1aWxvzuVqKf+vx2FU9mX5
      | UNYxXmTSD6rX5/AhFzwD7UPPzEmjE4piUErbtra/AKK0GrA9qFdIyb/XA7D0PUpkBj
      | F9ypk/I3H4HCw8YxP/r2AAVyWY681JJ+C/d8VQ6M=
      |DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=mailb.linkedin.com;
      | s=proddkim1024; t=1510675019;
      | bh=vlIukZxbenroQc0cd2wmXkyx4+oPZT7cyYOi+0k0IiM=;
      | h=From:Subject:MIME-Version:Content-Type:To:Date:X-LinkedIn-Class:
      | X-LinkedIn-Template:X-LinkedIn-fbl;
      | b=bnoMHlqUL/CBUXOqB3LMPbz7Wqmdk6lQWPfPUmVdpIjP/GmT0iBAtw160+MPxVixG
      | rRLQvzHzhob6H4HgvXBiq0RTpnDTcCoRpgzbFzBTl115rnjk3DdIWaqsO3aklq2uod
      | 7GMeSmv36mngRJtaU2SamIAY867ZhZXzZlOnqchw=
      |From: LinkedIn <jobs-listings@linkedin.com>
      |Message-ID: <1994637410.2874464.1510675019013.JavaMail.app@lva1-app3759.prod.linkedin.com>
      |Subject: IP Operations
      |MIME-Version: 1.0
      |Content-Type: multipart/alternative;
      | boundary="----=_Part_2874462_211417655.1510675019006"
      |To: Pavel Chlupacek <pavel.chlupacek@spinoco.com>
      |Date: Tue, 14 Nov 2017 15:56:59 +0000 (UTC)
      |X-LinkedIn-Class: JOBS-TO-MBR
      |X-LinkedIn-Template: jobs_jymbii_digest
      |X-LinkedIn-fbl: m2-aszxtlx7udpecdghpjpk8de51nb5salzl4ctgok39kgajsudgl7t7eqorpbx1z9hdzpp89hoelzxxd851blaeioxjrdcseczfn0408
      |X-LinkedIn-Id: 1ckjt-j9zswg1k-tk
      |List-Unsubscribe: <https://www.linkedin.com/e/v2?e=1ckjt-j9zswg1k-tk&t=lun&midToken=AQHZpF-vUrxfEg&ek=jobs_jymbii_digest&li=41&m=unsub&ts=unsub&loid=AQGVV4m3M1JuUgAAAV-7P3R32K_mSezTe42jBk8xiIluEam-3NnRjWdSt48KY-0cm3WSTUu8hLWCiCax9CusW_XSKkPlpDxI048J9WsZZpnE9Q&eid=1ckjt-j9zswg1k-tk>
      |Require-Recipient-Valid-Since: pavel.chlupacek@spinoco.com; Tue, 20 Aug 2013
      | 00:43:21 +0000
      |
      |)
      |4 OK Success
      |
      |""".stripMargin.lines.mkString("\r\n")


  def parseFetchResponse(resp: String, chunkSz: Int): Vector[Either[String, Long]] = {
    Stream.chunk(Chunk.bytes(resp.getBytes)).covary[IO]
    .chunkLimit(chunkSz).flatMap { ch => Stream.chunk(ch) }
    .through(IMAPClient.impl.lines)
    .mapAccumulate(None: Option[Long]) { case (bytesSz, next) => next match {
      case IMAPText(s) =>
        (None, bytesSz.map { sz => Stream(Right(sz), Left(s)) }.getOrElse(Stream(Left(s))))
      case IMAPBytes(bs) =>
        val sz = bytesSz.map(_ + bs.size) orElse Some(bs.size)
        (sz,  Stream.empty)
    }}
    .flatMap(_._2)
    .compile.toVector.unsafeRunSync()
  }


  property("decode-fetch-empty-header-result") = protect {
    parseFetchResponse(emptyHeaderResponse, 1024) ?= Vector(
      Left("* 20948 FETCH (UID 16578604 BODY[HEADER] NIL)")
    )
  }

  property("decode-fetch-header-result") = forAll(Gen.choose(1, headerSingleResponse.size)) { sz =>
    parseFetchResponse(headerSingleResponse, sz) ?= Vector(
      Left("* 4250 FETCH (UID 4268 BODY[HEADER] ")
      , Right(5042)
      , Left(")")
      , Left("* 18617 EXISTS")
      , Left("3 OK Success")
    )
  }

  property("decode-fetch-header-result-multi") = forAll(Gen.choose(1, multipleHeadersResponse.size))  { sz =>
    parseFetchResponse(multipleHeadersResponse, sz) ?= Vector(
      Left("* 4249 FETCH (BODY[HEADER] ")
      , Right(4774)
      , Left(" UID 4267)")
      , Left("* 4250 FETCH (UID 4268 BODY[HEADER] ")
      , Right(5042)
      , Left(")")
      , Left("4 OK Success")
    )
  }

  def parseFetchRawResponse(resp: String, chunkSz: Int): Vector[(Int, String, Either[String, Long])] = {
    IMAPClient.impl.rawContent[IO](Stream(Right(
      Stream.chunk(Chunk.bytes(resp.getBytes)).covary[IO]
      .chunkLimit(chunkSz).flatMap(ch => Stream.chunk(ch))
      .through(IMAPClient.impl.lines)
    )))
    .noneTerminate
    .mapAccumulate(None: Option[(Long, Int, String)]) {
      case (acc, Some((idx, k, next))) => next match {
        case IMAPText(s) =>
          (None, acc.map { case (sz, idx0, k0) => Stream((idx0, k0, Right(sz)), (idx, k, Left(s))) }.getOrElse(Stream((idx, k, Left(s)))))
        case IMAPBytes(bs) =>
          val acc0 = acc.map { case (sz, idx0, k0) => (sz + bs.size, idx0, k0)} orElse Some((bs.size, idx, k))
          (acc0,  Stream.empty)

      }

      case (acc, None) =>
        (None, Stream.emits(acc.toSeq.map { case (sz, idx0, k0) => (idx0, k0, Right(sz)) }))

    }
    .flatMap(_._2)
      .compile.toVector.unsafeRunSync()
  }


  property("decode-fetch-raw-content") = forAll(Gen.choose(1, headerSingleResponse.size))  { sz =>

    parseFetchRawResponse(headerSingleResponse, sz) ?= Vector(
      (0, "UID", Left("4268"))
      , (0, "BODY[HEADER]", Right(5042))
    )
  }


  property("decode-fetch-raw-content-multiple") = forAll(Gen.choose(1, multipleHeadersResponse.size))  { sz =>
    parseFetchRawResponse(multipleHeadersResponse, sz) ?= Vector(
      (0, "BODY[HEADER]", Right(4774l))
      , (0, "UID", Left("4267"))
      , (1, "UID", Left("4268"))
      , (1, "BODY[HEADER]", Right(5042l))
    )
  }

  property("decode-fetch-raw-content-empty") = protect {
    parseFetchRawResponse(emptyHeaderResponse, 1024) ?= Vector(
      (0, "UID", Left("16578604"))
      , (0, "BODY[HEADER]", Left("NIL"))
    )
  }


  def parseFetchEmailHeader(resp: String, chunkSz: Int): Vector[(Long @@ MailUID, Int)] = {
    IMAPClient.impl.rawContent[IO](Stream(Right(
      Stream.chunk(Chunk.bytes(resp.getBytes)).covary[IO]
      .chunkLimit(chunkSz).flatMap(ch => Stream.chunk(ch))
      .through(IMAPClient.impl.lines)
    )))
    .through(IMAPClient.impl.fetchLog)
    .through(IMAPClient.impl.mkEmailHeader(EmailHeaderCodec.codec(100 * 1024)))
    .compile.toVector.unsafeRunSync().map { h =>
      (h.uid, h.header.fields.size)
    }
  }

  property("decode-fetch-email-header") = forAll(Gen.choose(1, headerSingleResponse.size))  { sz =>
    parseFetchEmailHeader(headerSingleResponse, sz) ?= Vector(
      tag[MailUID](4268l) -> 26
    )
  }

  property("decode-fetch-email-header-multiple") =  forAll(Gen.choose(1, multipleHeadersResponse.size))  { sz =>
    parseFetchEmailHeader(multipleHeadersResponse, sz) ?= Vector(
      tag[MailUID](4267l) -> 29
      , tag[MailUID](4268l) -> 26
    )
  }

  property("decode-fetch-email-header-empty") = protect {
    parseFetchEmailHeader(emptyHeaderResponse, 1024) ?= Vector(
      tag[MailUID](16578604l) -> 0
    )
  }


  val fetchBodyByteResponse =
    """* 4238 FETCH (BODY[2] {328}
      |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAPFBMVEX///+1tbWwsLCtra3////5
      |+fmLi4vZ2dnT09P8/PzPz8+rq6uhoaHR0dFycnJwcHB6enp4eHiDg4OAgIBog/vRAAAADnRSTlMA
      |IiJV3e7u7u7u7u7u7rDOyYEAAABUSURBVHhepcpLDoAwCABRqkBbP9Dq/e9qLYS1ibN8GQBYWFVG
      |fQWLWyFEJG0uknGmuz+CDnjYEzDqDpF8BrV+HBRHNThjyBP42qpBufmFxOIpJ3gAPTUGaYiilrsA
      |AAAASUVORK5CYII=)
      |10 OK Success
      |
      |""".stripMargin.stripMargin.lines.mkString("\r\n")

  val fetchBodyBytes = ByteVector.fromBase64(
    """iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAPFBMVEX///+1tbWwsLCtra3////5
      |+fmLi4vZ2dnT09P8/PzPz8+rq6uhoaHR0dFycnJwcHB6enp4eHiDg4OAgIBog/vRAAAADnRSTlMA
      |IiJV3e7u7u7u7u7u7rDOyYEAAABUSURBVHhepcpLDoAwCABRqkBbP9Dq/e9qLYS1ibN8GQBYWFVG
      |fQWLWyFEJG0uknGmuz+CDnjYEzDqDpF8BrV+HBRHNThjyBP42qpBufmFxOIpJ3gAPTUGaYiilrsA
      |AAAASUVORK5CYII=
    """.stripMargin).get

  def decodeFetchBytes(content: String, chunkSz: Int, key: String = "BODY[2]", encoding: String = "BASE64"): ByteVector = {
    IMAPClient.impl.rawContent[IO](Stream(Right(
      Stream.chunk(Chunk.bytes(content.getBytes)).covary[IO]
        .chunkLimit(chunkSz).flatMap(ch => Stream.chunk(ch))
        .through(IMAPClient.impl.lines)
    )))
    .through(IMAPClient.impl.fetchBytesOf(0, key, encoding))
    .chunks.map { ch =>
      val bs = ch.toBytes
      ByteVector.view(bs.values, bs.offset, bs.size)
    }
    .compile.toVector.map(_.reduceOption(_ ++ _).getOrElse(ByteVector.empty))
    .unsafeRunSync
  }


  property("decode-fetch-bytes.base64.bytes") = forAll(Gen.choose(1, fetchBodyByteResponse.size))  { sz =>
    decodeFetchBytes(fetchBodyByteResponse, sz) ?= fetchBodyBytes
  }


  def decodeFetchText(content: String, chunkSz: Int, key: String = "BODY[1]", encoding: String = "BASE64", charset: Option[String] = Some("UTF-8")): String = {
    IMAPClient.impl.rawContent[IO](Stream(Right(
      Stream.chunk(Chunk.bytes(content.getBytes)).covary[IO]
        .chunkLimit(chunkSz).flatMap(ch => Stream.chunk(ch))
        .through(IMAPClient.impl.lines)
    )))
    .through(IMAPClient.impl.fetchTextOf(0, key, encoding, charset))
    .chunks.map(StringChunk.asString)
    .compile.toVector.map(_.reduceOption(_ ++ _).getOrElse(""))
    .unsafeRunSync()
  }

  val bodyText7BitResponse =
    """* 2002 FETCH (BODY[1] {245}
      |Thanks Pavel Chlupacek for requesting an account on Restcomm Cloud under the cloud Organization !
      |
      |Please click on the link below to create your account https://accounts.restcomm.com/verify-code?code=1709-4060452b-1b01-4991-9b7e-93e98e139e9d
      |)
      |11 OK Success
      |""".stripMargin.lines.mkString("\r\n")

  val BodyText8BitResponse =
    """* 2077 FETCH (BODY[1] {87}
      |Jestliže se vám newsletter nezobrazil správně, klikněte prosím sem
      |Červen 2017 )
      |12 OK Success
      |""".stripMargin.lines.mkString("\r\n")

  val BodyTextQuotedPrintableResponse =
    """* 4249 FETCH (BODY[1] {745}
      |<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
      |<html>
      |<head>=20
      |<meta http-equiv=3D"Content-Type" content=3D"text/html; charset=3Dutf-8">=
      |=20
      |</head>=20
      |<body><style type=3D"text/css">div#emailPreHeader{ display: none !important=
      |; }</style><div id=3D"emailPreHeader" style=3D"mso-hide:all; visibility:hid=
      |den; opacity:0; color:transparent; mso-line-height-rule:exactly; line-heigh=
      |t:0; font-size:0px; overflow:hidden; border-width:0; display:none !importan=
      |t;">ReSharper Ult. 2017.3 EAP, GoLand EAP 18, Kotlin/Native IDE Support Pre=
      |view</div>=20
      |<div style=3D"background:#ffffff;padding:0px 0px 0px 0px;margin:0px 0px 0px=
      | 0px;font-size:14px;font-family:Arial,Helvetica,sans-serif;line-height:24px=
      |">=20
      |)
      |13 OK Success
      |""".stripMargin.lines.mkString("\r\n")

  val BodyTextBase64Response =
    """* 3632 FETCH (BODY[1] {3112}
      |DQpGb3JnZXQgYWJvdXQgd2hhdCB0byBhbmFseXplIGFuZCB3aGF0IHRvIGtlZXAuIA0K
      |DQoiQ2xvdWRlcmEiIDxodHRwOi8vYXBwLmdvLmNsb3VkZXJhLmNvbS9lL2VyP3M9MTQ2
      |NTA1NDM2MSZsaWQ9MjY0NTEmZWxxVHJhY2tJZD0wMGFiYTZmMzAzMDE0YTVkYWY0YjA5
      |OWMwNjVlNzQ0MiZlbHE9MzA2ZmRjMDY5YmQ0NDhhNmEyNDExYWRlNmRmYzM4MzQmZWxx
      |YWlkPTUyMDUmZWxxYXQ9MT4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIA0K
      |DQpDb250YWN0IDxodHRwOi8vYXBwLmdvLmNsb3VkZXJhLmNvbS9lL2VyP3M9MTQ2NTA1
      |NDM2MSZsaWQ9MjY1MTMmZWxxVHJhY2tJZD0zNDk3OWQxMTQ2YWM0ZmMzOGM4ZGRmODE1
      |ZTc0YjkxNiZlbHE9MzA2ZmRjMDY5YmQ0NDhhNmEyNDExYWRlNmRmYzM4MzQmZWxxYWlk
      |PTUyMDUmZWxxYXQ9MT4NCg0KV2ViaW5hcjogU3VwZXJjaGFyZ2UgeW91ciBTSUVNIHdp
      |dGggQ2xvdWRlcmENCjNyZCBPY3RvYmVyIEAgMTBhbSBCU1QgfCBSZWdpc3RlciBub3cg
      |wrsgPGh0dHA6Ly9hcHAuZ28uY2xvdWRlcmEuY29tL2UvZXI/cz0xNDY1MDU0MzYxJmxp
      |ZD0yODAwMCZlbHFUcmFja0lkPWQ0NjEzYzUyZTg2ODQxZjhhYmNmY2YzM2IwNTk3ZThl
      |JmVscT0zMDZmZGMwNjliZDQ0OGE2YTI0MTFhZGU2ZGZjMzgzNCZlbHFhaWQ9NTIwNSZl
      |bHFhdD0xPiANCg0KIkNsb3VkZXJhIiA8aHR0cDovL2FwcC5nby5jbG91ZGVyYS5jb20v
      |ZS9lcj9zPTE0NjUwNTQzNjEmbGlkPTI4MDAwJmVscVRyYWNrSWQ9MGFlZjNhNTU1NzQ1
      |NGM5MDljNmE2NGU5YjFiY2M0OGImZWxxPTMwNmZkYzA2OWJkNDQ4YTZhMjQxMWFkZTZk
      |ZmMzODM0JmVscWFpZD01MjA1JmVscWF0PTE+ICAgICAgICAgICANCg0KSGkgUGF2ZWws
      |DQpXZSBob3BlIHlvdeKAmWxsIGpvaW4gdXMgZm9yIGEgbGl2ZSB3ZWJpbmFyIGFib3V0
      |IG9wdGltaXppbmcgeW91ciBTSUVNIGRlcGxveW1lbnRzIHdpdGggQ2xvdWRlcmEuDQpK
      |b2luIHVzIGFzIHdlIGRpc2N1c3MgaG93IENsb3VkZXJhIGVtcG93ZXJzIElUIGFuZCBj
      |eWJlcnNlY3VyaXR5IGlubm92YXRvcnMgdG8gcHJvYWN0aXZlbHkgYXNzZXNzIHJpc2sg
      |YnkgYWNjZWxlcmF0aW5nIGFub21hbHkgZGV0ZWN0aW9uLCBpbnZlc3RpZ2F0aW9uLCBh
      |bmQgcmVzcG9uc2Ugd2l0aCBtYWNoaW5lIGxlYXJuaW5nIGFuZCBjb21wbGV0ZSBlbnRl
      |cnByaXNlIHZpc2liaWxpdHkuDQoNClJlZ2lzdGVyIG5vdyDCuyA8aHR0cDovL2FwcC5n
      |by5jbG91ZGVyYS5jb20vZS9lcj9zPTE0NjUwNTQzNjEmbGlkPTI4MDAwJmVscVRyYWNr
      |SWQ9NDQxMWE2OTE2MzE2NDRjZjg0OGNjNzFjOTdmMzMxNDYmZWxxPTMwNmZkYzA2OWJk
      |NDQ4YTZhMjQxMWFkZTZkZmMzODM0JmVscWFpZD01MjA1JmVscWF0PTE+ICAgICAgICAg
      |ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
      |ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICANCg0KDQoN
      |Cg0KQ2xvdWRlcmEsIEluYy4gPGh0dHA6Ly93d3cuY2xvdWRlcmEuY29tLz9lbHFUcmFj
      |a0lkPWMwYjMzYjA0YTdlNjQ5NzJhNTBiOGVkNTdmN2ViYTBiJmVscT0zMDZmZGMwNjli
      |ZDQ0OGE2YTI0MTFhZGU2ZGZjMzgzNCZlbHFhaWQ9NTIwNSZlbHFhdD0xJmVscUNhbXBh
      |aWduSWQ9MzE1Mz4gMzk1IFBhZ2UgTWlsbCBSZCwgM3JkIEZsb29yIMK3IFBhbG8gQWx0
      |bywgQ0EgOTQzMDYgPCM+ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
      |IA0KDQrCqTIwMTcgQ2xvdWRlcmEsIEluYy4gQWxsIHJpZ2h0cyByZXNlcnZlZC4gQ2xv
      |dWRlcmEgYW5kIHRoZSBDbG91ZGVyYSBsb2dvIGFyZSB0cmFkZW1hcmtzIG9yIHJlZ2lz
      |dGVyZWQgdHJhZGVtYXJrcyBvZiBDbG91ZGVyYSwgSW5jLiBpbiB0aGUgVVNBIGFuZCBv
      |dGhlciBjb3VudHJpZXMuIEFsbCBvdGhlciB0cmFkZW1hcmtzIGFyZSB0aGUgcHJvcGVy
      |dHkgb2YgdGhlaXIgcmVzcGVjdGl2ZSBjb21wYW5pZXMuIEluZm9ybWF0aW9uIGlzIHN1
      |YmplY3QgdG8gY2hhbmdlIHdpdGhvdXQgbm90aWNlLiAgICAgICAgICAgICAgICAgICAg
      |ICAgICAgICAgICAgICAgICANCg0KVG8gdW5zdWJzY3JpYmUgZnJvbSBmdXR1cmUgZW1h
      |aWxzIG9yIHRvIHVwZGF0ZSB5b3VyIGVtYWlsIHByZWZlcmVuY2VzIGNsaWNrIGhlcmUg
      |PGh0dHA6Ly9hcHAuZ28uY2xvdWRlcmEuY29tL2Uvc2w/cz0xNDY1MDU0MzYxJmVscT0z
      |MDZmZGMwNjliZDQ0OGE2YTI0MTFhZGU2ZGZjMzgzND4gICAgICAgICAgICAgICAgICAg
      |ICAgICAgICAgICAgICAgICAgDQoNCg==)
      |13 OK Success
    """.stripMargin.lines.mkString("\r\n")

  val SP = " "

  property("decode-fetch-bytes.7bit") = forAll(Gen.choose(1, bodyText7BitResponse.size))  { sz =>
    decodeFetchText(bodyText7BitResponse, sz, encoding = "7BIT", charset=Some("US-ASCII")) ?=
      """Thanks Pavel Chlupacek for requesting an account on Restcomm Cloud under the cloud Organization !
        |
        |Please click on the link below to create your account https://accounts.restcomm.com/verify-code?code=1709-4060452b-1b01-4991-9b7e-93e98e139e9d
        |
        |""".stripMargin.lines.mkString("\r\n")
  }

  property("decode-fetch-bytes.8bit") = forAll(Gen.choose(1, BodyText8BitResponse.size))  { sz =>
    decodeFetchText(BodyText8BitResponse, sz, encoding = "8BIT", charset = Some("UTF-8")) ?=
      """Jestliže se vám newsletter nezobrazil správně, klikněte prosím sem
        |Červen 2017 """.stripMargin.lines.mkString("\r\n")
  }


  property("decode-fetch-bytes.quoted-printable") = forAll(Gen.choose(2, BodyTextQuotedPrintableResponse.size))  { sz =>


    decodeFetchText(BodyTextQuotedPrintableResponse, sz, encoding = "QUOTED-PRINTABLE", charset = Some("UTF-8")) ?=
      s"""<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
         |<html>
         |<head>$SP
         |<meta http-equiv="Content-Type" content="text/html; charset=utf-8">$SP
         |</head>$SP
         |<body><style type="text/css">div#emailPreHeader{ display: none !important; }</style><div id="emailPreHeader" style="mso-hide:all; visibility:hidden; opacity:0; color:transparent; mso-line-height-rule:exactly; line-height:0; font-size:0px; overflow:hidden; border-width:0; display:none !important;">ReSharper Ult. 2017.3 EAP, GoLand EAP 18, Kotlin/Native IDE Support Preview</div>$SP
         |<div style="background:#ffffff;padding:0px 0px 0px 0px;margin:0px 0px 0px 0px;font-size:14px;font-family:Arial,Helvetica,sans-serif;line-height:24px">$SP
         |
         |""".stripMargin.lines.mkString("\r\n")
  }


  property("decode-fetch-bytes.base64.text") = forAll(Gen.choose(1, BodyTextBase64Response.size))  { sz =>

    val r = decodeFetchText(BodyTextBase64Response, sz, encoding = "BASE64", charset = Some("UTF-8"))
      .lines.map(_.trim).mkString("\r\n")

    val e =
      s"""
         |Forget about what to analyze and what to keep.
         |
         |"Cloudera" <http://app.go.cloudera.com/e/er?s=1465054361&lid=26451&elqTrackId=00aba6f303014a5daf4b099c065e7442&elq=306fdc069bd448a6a2411ade6dfc3834&elqaid=5205&elqat=1>
         |
         |Contact <http://app.go.cloudera.com/e/er?s=1465054361&lid=26513&elqTrackId=34979d1146ac4fc38c8ddf815e74b916&elq=306fdc069bd448a6a2411ade6dfc3834&elqaid=5205&elqat=1>
         |
         |Webinar: Supercharge your SIEM with Cloudera
         |3rd October @ 10am BST | Register now » <http://app.go.cloudera.com/e/er?s=1465054361&lid=28000&elqTrackId=d4613c52e86841f8abcfcf33b0597e8e&elq=306fdc069bd448a6a2411ade6dfc3834&elqaid=5205&elqat=1>
         |
         |"Cloudera" <http://app.go.cloudera.com/e/er?s=1465054361&lid=28000&elqTrackId=0aef3a5557454c909c6a64e9b1bcc48b&elq=306fdc069bd448a6a2411ade6dfc3834&elqaid=5205&elqat=1>
         |
         |Hi Pavel,
         |We hope you’ll join us for a live webinar about optimizing your SIEM deployments with Cloudera.
         |Join us as we discuss how Cloudera empowers IT and cybersecurity innovators to proactively assess risk by accelerating anomaly detection, investigation, and response with machine learning and complete enterprise visibility.
         |
         |Register now » <http://app.go.cloudera.com/e/er?s=1465054361&lid=28000&elqTrackId=4411a691631644cf848cc71c97f33146&elq=306fdc069bd448a6a2411ade6dfc3834&elqaid=5205&elqat=1>
         |
         |
         |
         |
         |Cloudera, Inc. <http://www.cloudera.com/?elqTrackId=c0b33b04a7e64972a50b8ed57f7eba0b&elq=306fdc069bd448a6a2411ade6dfc3834&elqaid=5205&elqat=1&elqCampaignId=3153> 395 Page Mill Rd, 3rd Floor · Palo Alto, CA 94306 <#>
         |
         |©2017 Cloudera, Inc. All rights reserved. Cloudera and the Cloudera logo are trademarks or registered trademarks of Cloudera, Inc. in the USA and other countries. All other trademarks are the property of their respective companies. Information is subject to change without notice.
         |
         |To unsubscribe from future emails or to update your email preferences click here <http://app.go.cloudera.com/e/sl?s=1465054361&elq=306fdc069bd448a6a2411ade6dfc3834>
         |
         |""".stripMargin.lines.mkString("\r\n")


    r ?= e
  }



}
