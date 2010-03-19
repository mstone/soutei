(define Content
 '(html:begin
   (Header
    (title "Transport of Soutei assertions")
    (description "Discussion of various ways to bring assertions to
the Soutei engine.")
    (Date-Revision-yyyymmdd "20040723")
    (Date-Creation-yyyymmdd "20040722")
    (keywords "Binder, Soutei, Keynote, X.509, Attribute certificate")
    (AuthorAddress "oleg-at-okmij.org")
    (Author "Oleg Kiselyov")
    (rcs-id "$Id: Auth-transport.scm,v 1.2 2004/10/15 02:46:02 oleg Exp oleg $")
    (long-title "Transport of Soutei assertions")
    (Links
     (start "Authorization.html"
       (title "Authorization and Role-Based Access control"))
     (contents "Authorization.html")
     (prev "Auth-use-cases.html")
     (next "Authorization.html")
     (top "index.html")
     (home "http://soutei.sf.net/")))

  (body
   (navbar)
   (page-title)


(p "This page discusses various ways of making Soutei assertions
available to the decision engine: by reading a local configuration
file, by querying trusted databases and LDAP servers, or by delivering
the assertions in HTTP headers and in Public-key and Attribute X.509
certificates.")

   (TOC)

(Section 2 "X.509" " Certificates and Soutei")

(p "Soutei assertions may be included in a public-key X.509
certificate (PKC) " (cite "RFC3280") " or in a X.509 attribute
certificate " (cite "RFC3281") ".")

(p "There is a distinct advantage of including Soutei assertions in a
public-key certificate intended for establishing a TLS
\(HTTPS, SSL) connection with the application server. In that case, we
get certificate transport and validation ``for free'', performed as
a part of the TLS handshake. In case of HTTPS, the web server should
be configured to request a client certificate during the
handshake. The web server will challenge the validity of the
certificate, according to the TLS protocol. If the certificate is found
valid and authentic, the web server establishes the TLS connection and
accepts the HTTP request. The web server will give to the application
server (i.e., the Metcast server) the certificate in the PEM-encoded
format, as the content of the environment variable " (code
"SSL_CLIENT_CERT") ". An option " (code "ExportCertData") " must be
included in " (code "SSLOptions") " directive of the SSL engine
portion of the Apache configuration file. The Soutei assertion should
be placed, as an octet string, in the field " (code "authInfo") " of
an " (code "Attribute") " structure " (code "SvceAuthInfo") " of a
sequence " (code "SubjectDirectoryAttributes") " of the v3 certificate
extensions field. A private v3 certificate extension may also be
defined for the purpose of holding assertions. Because the assertion
is a part of a signed and validated certificate, we can trust the
assertion without any further checks.")

(p "Including Soutei assertions in a public-key certificate however
can be problematic from the logistic point of view. Certificates
issued to an end user (e.g., Common Access Card certificates)
typically have restrictions on their usage: the bit " (code "cA") " is
turned off and the certificates may be invalid for key
encipherment. Therefore, an end user may not issue his own 
certificates to delegate his privileges to applications or other users
via Soutei assertions.  The user must ask a Certifying Authority (CA)
to issue him a certificate with the appropriate Soutei assertions. The
latter is quite an involved process. Furthermore, it is argued in "
(cite "RFC3281") " that CA are wrong entities to issue authorization
statements.")

(p "The task of issuing authorization statements properly belongs
to attribute authorities, as described in " (cite "RFC3281")
". Authorization statements are placed into attribute X.509
certificates. According to " (cite "RFC3281") ", an attribute
certificate is a structure similar to public key certificates. Whereas
the latter binds an identity of a subject and his public key, an
attribute certificate serves to assign authorization attributes to the
certificate holder. The attributes may include group membership, role,
security clearance, etc.")
(blockquote
  "Some people constantly confuse public-key certificates (PKC) and
attribute certificates (AC).  An analogy may make the distinction
clear.  A PKC can be considered to be like a passport: it identifies
the holder, tends to last for a long time, and should not be trivial
to obtain.  An AC is more like an entry visa: it is typically issued
by a different authority and does not last for as long a time.  As
acquiring an entry visa typically requires presenting a passport,
getting a visa can be a simpler process " (cite "RFC3281") ".")

(p "Attribute certificates seem therefore appropriate vehicles for
Soutei assertions. An end user may issue attribute certificates for
his own applications. Attribute certificates are short-lived and ideal
for such a delegation purpose. Furthermore, the profile in " (cite
"RFC3281") " explicitly states that an attribute certificate issuer
must not be a CA: an attribute certificate issuer's public-key
certificate must not contain a " (code "basicConstraints") " extension
with the " (code "cA") " boolean set to " (code "TRUE") ". Not only
end users may issue attribute certificates -- they are the only
entities that may do so.")

(p "Before processing an assertion found in an attribute certificate,
the server must validate the certificate as described in Section 5 of
" (cite "RFC3281") ". If the field " (code "Holder") " of the
certificate identifies the holder by name or by a certificate
reference, the identity of the holder must match the identity of the
authenticated client; see Section 4.2.2 of RFC3281 for more detail.
The holder of the certificate may also be an empty sequence. The assertion
found in such a certificate is eligible for caching. Certificates with
the empty holder name are employed for delegation.")

(p "A Soutei assertion is placed into one attribute of an attribute
certificate. The certificate may include other attributes. It seems
that the most appropriate attribute for Soutei assertions is
" (code "SvceAuthInfo") ", described in Section 4.4.1 or RFC3281:
``This attribute provides information that can be presented by the AC
verifier to be interpreted and authenticated by a separate application
within the target system.''")

(verbatim
  "id-aca                     OBJECT IDENTIFIER ::= { id-pkix 10 }"
  "id-aca-authenticationInfo  OBJECT IDENTIFIER ::= { id-aca 1 }"
  "SvceAuthInfo ::= SEQUENCE {"
  "      service   GeneralName,"
  "      ident     GeneralName,"
  "      authInfo  OCTET STRING OPTIONAL }"
  )

"The Soutei verifier is such a separate application. The Soutei
assertion should be placed into the field " (code "authInfo") ". The
fields " (code "service") " and " (code "ident") " are currently
unused and should be set to the empty value of the type " (code
"directoryName") ". We should mention that a Group attribute (Section
4.4.4 of RFC3281) seems also an appropriate attribute to hold Soutei
assertions. We may in the future register an attribute object identifier
specifically for Soutei assertions."

; Place service into Soutei's application context as 'service_info'?

(p
  "When an attribute certificate is imported into Soutei, the subject
identity of the issuer (taken from the field " (code "issuerName") "
of the certificate) serves as a context identifier for the imported
assertion. The subject identity is generally a SHA-1 hash computed
from the name of the issuer or found in the " (code
"SubjectKeyIdentifier") " extension of the public-key certificate of
the issuer. The extension takes precedence, if it exists.")

(p "The problem with attribute certificates is transporting them from
a client to the server. Can we still piggy-back on TLS for
transporting and validating attribute certificates? It seems that
OpenSSL might do that for us. This issue requires further
investigation.")

(Section 2 "HTTP" " headers and Soutei")

(p "Soutei assertions may also be delivered in HTTP headers. The
headers take precedence over the TLS-based transport of 
certificates. We introduce two kinds of headers. Only one kind must be
present in a HTTP session.")

(p "Attribute-assertion header: " (code "X-X509-AC") ". The contents of
the header is an attribute certificate (described above), DER- and
Base64- encoded.")

(p "Signed-assertion-header: " (code "X-BAssertion") ". The contents of
the header is a cryptographically signed message " (cite "RFC3369") "
with the content-type " (code "text/x-bassertion") ". The signer of
the message is considered to be the issuer of the assertion.")


(hr)

(Section 2 "References")

 (bibitem "Binder" "Binder"
    "J. DeTreville. Binder, a logic-based security language. "
    "IEEE Security and Privacy, 2002. "
    (URL "http://research.microsoft.com/research/pubs/view.aspx?tr_id=545"))


(bibitem "PKI-Tutorial" "PKI-Tutorial"
  "Peter Gutmann. "
  "Everything you never wanted to know about PKI but have been forced
to find out. "
  (URL "http://www.cs.auckland.ac.nz/~pgut001/pubs/pkitutorial.pdf"))


(bibitem "RFC3280" "RFC3280"
  "R. Housley, W. Polk, W. Ford, D. Solo. "
  "Internet X.509 Public Key Infrastructure "
  "Certificate and Certificate Revocation List (CRL) Profile. "
  "RFC 3280, Standards Track. April 2002."
  (URL "http://www.rfc-editor.org/rfc/rfc3280.txt")
)

(bibitem "RFC3281" "RFC3281"
  "S. Farrell and R. Housley. "
  "An Internet Attribute Certificate Profile for Authorization. "
  "RFC 3281, Standards Track. April 2002."
  (URL "http://www.rfc-editor.org/rfc/rfc3281.txt")
)

(bibitem "RFC3369" "RFC3369"
  "R. Housley. "
  "Cryptographic Message Syntax (CMS). "
  "RFC 3369. Standards Track. August 2002. "
  (URL "http://www.rfc-editor.org/rfc/rfc3369.txt")
)

(bibitem "NIST-PKI" "NIST-PKI"
  "NIST PKI Program"
  (URL "http://csrc.nist.gov/pki/")
)

; ASN.1 References

(bibitem "ASN1-Guide" "ASN1-Guide"
  "Burton S. Kaliski Jr. "
  "A Layman's Guide to a Subset of ASN.1, BER, and DER. "
  "An RSA Laboratories Technical Note. "
  "Revised November 1, 1993. "
  (URL "http://citeseer.nj.nec.com/47302.html")
  ; http://luca.ntop.org/Teaching/Appunti/asn1.html
  )

; ASN.1 Misuse, a paper by Carl M. Ellison
; Other ASN.1 papers
; http://www.oss.com/asn1/booksintro.html
; http://www.larmouth.demon.co.uk/tutorials/tagging/index.htm
; http://www.oss.com/

(bibitem "X509-Style" "X509-Style"
  "Peter Gutmann. "
  "X.509 Style Guide. "
  "October 2000. "
  (URL "http://www.cs.auckland.ac.nz/~pgut001/pubs/x509guide.txt"))


(footer)

)))


;========================================================================
;			HTML generation

(define common-rules-here
  (generic-web-rules Content
  `(
		; A reference to a group of test cases
		; (group-ref target . title)
		; If title is given, generate a regular
		;	<a href="#target">title</a>
		; Otherwise, use target as the title
     (group-ref
      . ,(lambda (tag target . title)
	   (list "<a href=\"#" target "\">" (if (pair? title) title target)
		 "</a>" nl)))

		; A category of use cases
		; (Category key . title)
     (Category
      . ,(lambda (tag key . title)
	   (list "<hr><a name=\"" key "\">&nbsp;</a><h2>Category " 
		 (if (pair? title) title key)
		 "</h2>" nl)))

		; A section of use cases
		; (Section key title ((group) ...))
     (Scenario
      *macro*
      . ,(lambda (tag key title . groups)
	   `((a (@ (name ,key)) (n_))
	      (h3 "Scenario " ,key)
	      ,title
	      (ul ,(map (lambda (gr) `(li ,gr (br) (n_))) groups))
	      ;,groups
	      )))

		; A group of test cases
		; (group key title ((tc) ...) (check))
     (group
      *preorder*
      . ,(lambda (tag key title test-cases check)
	   (pre-post-order
	    `((a (@ (name ,key)) (n_)) 
	      ,title
	      (ul ,(map (lambda (tc) `(li ,tc)) test-cases))
	      ,check
	      )
	    `(
	      (tc			; a test case
	       . ,(lambda (tag . txt) txt))
	      (check			; Expect the result
	       . ,(lambda (tag . txt)
		    (and (not (null? txt)) (not (equal? '("") txt))
			 (list "Expect: " txt))))
	      ,@common-rules-here))))

     ; Grammatical terms
     (nonterm		; Non-terminal of a grammar
      *macro*
      . ,(lambda (tag term)
	   (list "<" term ">")))

     (term-lit		; terminal that is a Scheme id
      *macro*
      . ,(lambda (tag term)
	   term))

     (term-str		; terminal that is a Scheme string
      *macro*
      . ,(lambda (tag term)
	   (list "\"" term "\"")))

     (term-lit		; a literal Scheme symbol
      *macro*
      . ,(lambda (tag term)
	   `(em ,term)))

     (ebnf-opt		; An optional term
      . ,(lambda (tag term)
	   (list term "?")))

     (ebnf-*		; Zero or more repetitions
      . ,(lambda (tag term)
	   (list term "*")))

     (ebnf-+		; One or more repetitions
      . ,(lambda (tag term)
	   (list term "+")))

     (ebnf-choice	; Choice of terms
      . ,(lambda (tag . terms)
	   (list-intersperse terms " | ")))

     (ebnf-group	; Group of terms
      *macro*
      . ,(lambda (tag . terms)
	   `((strong "(") " " ,(list-intersperse terms " ") " "
	     (strong ")"))))

     (production
      *macro*
      . ,(lambda (tag number lhs rhs . comment)
	   (define local-ss
	     `((quote *preorder* 
		 . ,(lambda (tag elem) `(term-str ,elem)))
	       (*default* . ,(lambda x x))
	       (*text*
		 . ,(lambda (tag str) `(nonterm , str)))))
	   (let ((lhs (pre-post-order lhs local-ss))
		 (rhs (pre-post-order rhs local-ss)))
	     ;(cerr "lhs: " lhs nl "rhs: " rhs nl)
	     `(tr (@ (valign top))
		(td (@ (align right))
		  (a (@ (name ("prod-" ,number))) "[" ,number "]") (n_))
		(td (@ (align right))
		  (code ,lhs))
		(td (@ (align center))
		  (code " ::= "))
		(td (@ (align left))
		  (code
		    ,(if (and (pair? rhs) (pair? (car rhs)))
		       (list-intersperse rhs " ")
		       rhs))
		  " " ,comment)))))

     (productions
      *macro*
      . ,(lambda (tag . prods)
	   `(table (@ (border 0) (bgcolor "#f5dcb3")) ,prods)))


     )))

; Generating HTML


(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content common-rules-here)))

(generate-HTML Content)


; LocalWords:  Datalog KeyNote RTC SDSI SPKI PolicyMaker SAML
; LocalWords:  Herbrand blockquote bibitem ebnf HTTPS SSL PKC TLS authInfo
; LocalWords:  SubjectDirectoryAttributes SvceAuthInfo GeneralName issuerName
; LocalWords:  directoryName SubjectKeyIdentifier OpenSSL issuer's IMG pubkey
; LocalWords:  DeTreville's ExportCertData SSLOptions Soutei Binder's pAtom
; LocalWords:  KANREN DeTreville
