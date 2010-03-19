(define Content
 '(html:begin
   (Header
    (title "Authorization and Role-Based Access control")
    (description "Notes on and the survey of Authorization and
Role-Based Access control systems.")
    (Date-Revision-yyyymmdd "20040723")
    (Date-Creation-yyyymmdd "20031216")
    (keywords "Binder, Keynote, Datalog, Authorization, logic system.")
    (AuthorAddress "oleg-at-okmij.org")
    (Author "Oleg Kiselyov")
    (rcs-id "$Id: Authorization.scm,v 1.5 2004/07/24 01:38:54 oleg Exp oleg $")
    (long-title "Authorization and Role-Based Access control. Survey and notes.")
    (Links
     (start "Authorization.html" 
       (title "Authorization and Role-Based Access control"))
     (contents "Authorization.html")
     (prev "Authorization.html")
     (next "Auth-use-cases.html")
     (top "index.html")
     (home "http://soutei.sf.net/")))

  (body
   (navbar)
   (page-title)
   (TOC)

;----------------------------------------------------------------------

(Section 2 "Trust-management" " systems")

(p "Most of the access control systems discussed below belong to a
class of trust-management systems. RFC 2704 succinctly describes these
systems as follows " (cite "RFC2704") ":")

(blockquote
  (p
    "A trust-management system provides standard, general-purpose
mechanisms for specifying application security policies and
credentials.  Trust-management credentials describe a specific
delegation of trust and subsume the role of public key certificates;
unlike traditional certificates, which bind keys to names, credentials
can bind keys directly to the authorization to perform specific
tasks." (br) "...")
  (p
    "Trust management unifies the notions of security policy,
credentials, access control, and authorization.  An application that
uses a trust-management system can simply ask the compliance checker
whether a requested action should be allowed.  Furthermore, policies
and credentials are written in standard languages that are shared by
all trust-managed applications; the security configuration mechanism
for one application carries exactly the same syntactic and semantic
structure as that of another, even when the semantics of the
applications themselves are quite different.")
  (p
    "Trust-management policies are easy to distribute across networks,
helping to avoid the need for application-specific distributed policy
configuration mechanisms, access control lists, and certificate
parsers and interpreters.")
)


;----------------------------------------------------------------------
(Section 2 "KeyNote")

(p
  "KeyNote " (cite "KeyNote") " " (cite "RFC2704") " is one particular
framework and a language to build trust-management systems. In
KeyNote, principals are identified by names, which can be
cryptographic keys. Policies and credentials are called `assertions':
typically cryptographically signed statements describing trusted
actions and conditions that yield a policy compliance value. The
latter is often a binary value (e.g., " (code "grant") " and " (code
"deny") "); a range of restricted access permissions may also be
specified. A principal may issue an assertion delegating authorization
to perform (a subset) of actions to other principals. Top-level assertions
are usually stored locally. Others are fetched from remote authorities
or delivered by clients. In the latter cases, the assertions should be
signed.")

(p
  "Particularly attractive properties of KeyNote are an ability of
principals to delegate a subset of their privileges to other
principals, and an ability to express authorization conditions as
logical formulas. A condition is a logical proposition over attributes
whose values can be strings, integers, and floating-point numbers. The
values of the attributes are provided by an application that requests
an authorization advice. The conditions can express, for example, that
a particular file is accessible for reading only within a specific
time window and only if the request is vouched for by at least two
trusted administrators.  Examples at the end of RFC2704 are quite
illustrative.")
(p
  "KeyNote is a mature system. There is a reference implementation and
several others. KeyNote is a part of a secure distributed file system
" (cite "DisCFS") " and of OpenBSD's IPSEC stack. Apache-SSL can also
use KeyNote. The KeyNote page
" (cite "KeyNote") " lists other real-world applications of the
system. Google search for " (code "KeyNote trust management") " yields
quite a few links.")
(p
  "One of the important properties of the KeyNote system is its
monotonicity: access permissions never decrease as more security
assertions are made available to the system. That is, KeyNote will
never authorize an action only because some crucial assertion was not
delivered to the system in time. We should note however that the
monotonicity property, however beneficial, precludes using KeyNote
assertions for revocation. Revocation of privileges must be handled in
some other way (e.g., through expiration of certificates).")
(p
  "The monotonicity property (adding certificates may only increase
the trust value) seems to be sound: it is guaranteed by the fact that
the " (code "Licensees:") " field of an assertion uses monotone
operators (" (code "&&") ", " (code "||") " and " (code "k-of") "),
and the " (code "Conditions:") " field cannot refer to other
certificates.")
(p
  "The notion of an application scope provides some kind of scoping of
attributes. The calling application is responsible for dereferencing
attributes -- either by passing a dictionary or providing a look-up
function (call-back). To the KeyNote system, the values of attributes
and the bindings themselves are immutable. KeyNote provides for
indirect references (e.g., " (code "$foo") " refers to an attribute
whose name is in the attribute " (code "foo") ").")

(Section 3 "Keynote problems")

(p "The KeyNote system is not without problems.")

(ol
  (li "RFC2704 says, ``Attribute names may be given literally or
calculated from string expressions and may be recursively
dereferenced.'' It is not clear if self-references or cyclical
references are expressible. If they are, a non-termination of a
policy decision becomes an issue.")

  (li
    "Type conversion seems quite sloppy. Attribute values are
strings; a user may request a conversion of a string to an integer or
an IEEE floating-point number. If the conversion fails, no error is
reported but the conversion result is assumed to be 0. Likewise,
dereferencing an unbound attribute reports no error but yields the
empty string instead.")

  (li
    "Local attributes (defined in an assertion itself) override
application-supplied attributes. However, if the name of a local
attribute is mis-spelled (or mis-cased -- names are case-sensitive),
trouble ensues. No error is reported but the overriding fails. The
error becomes especially insidious if a mis-spelled name is used as an
indirect attribute name. The latter fact may cause a wrong value used
in a condition formula, and consequently, authorizing an action that
should have been denied.")

  (li
    "A design decision making a numeric conversion failure yield 0 is a
security concern. Here's the example from RFC2704 itself:"
    (verbatim
      "Conditions:"
      "  @user_id == 0 -> \"full_access\";             # clause (1)"
      "  @user_id < 1000 -> \"user_access\";           # clause (2)"
      "  @user_id < 10000 -> \"guest_access\";         # clause (3)"
      "  user_name == \"root\" -> \"full_access\";       # clause (4)"
      )

    "Here " (code "@") " is a string-to-integer conversion
operator. Let us suppose that " (code "user_id") " was meant to be
" (code "65535") " but by mistake was " (code "65535-") ". The
conversion fails, " (code "@user_id") " yields the value of
zero, which triggers the answer " (code "full_access") ". A client
would be given an authorization for the full access when no access
should have been granted. This security concern becomes especially
serious if the values of the attributes are accepted from clients,
without exhaustive checking.")
  (li "It has been proven " (cite "RTC") " that an analysis of all
requests authorized by a set of assertions is undecidable. In fact,
we cannot in generally tell if a policy with a single assertion
authorizes any request at all. It is therefore impossible in general
to analyze the effect of security assertions, e.g., to verify global
policy constraints.")  )


; That isn't a problem: the whole test fails on the conversion error,
; rather than a single relation
; <blockquote>
;    A runtime error occurring in the evaluation of a test, such as
;    division by zero or an invalid regular expression, causes the test to
;    be considered false.  For example:

;       foo == "bar" -> {
;                         @a == 1/0 -> "oneval";    # subclause 1
;                         @a == 2 -> "anotherval";  # subclause 2
;                       };
;    Here, subclause 1 triggers a runtime error.  Subclause 1 is therefore
;    false (and has the value _MIN_TRUST).  Subclause 2, however, would be
;    evaluated normally.
; </blockquote>

; Note however that the language of a condition formulas includes
; negation! Therefore, failing a test may actually increase the
; permissions. Thus if a requesting principal manages to induce an
; application to set an attribute to a bad value, the principal may get 
; extra permissions. 


;----------------------------------------------------------------------
(hr)
(Section 2 "Binder system")

(p "Binder is a logic-based security language: an extension of
Datalog. Binder was introduced in a paper " (cite "Binder") ". Google
search for ``Binder security language'' offers many links to that
paper -- but no real applications or implementations. In that respect,
KeyNote is more developed. On the other hand, Binder is developed by
an experienced security researcher, has a clean design and sound 
logical foundations " (cite "Logic-AC") ".")
(p
  "A security statement in Binder is a logical program written in a
subset of Prolog without function symbols (i.e., Datalog). Binder
extends Datalog " (cite "Datalog") " with the notion of a context and
a distinguished relation " (code "says") ". A statement in Binder can
be a simple fact, e.g., " (code "can(john_smith,read,resource_r)") "
or a rule, e.g., " (code "can(X,read,resource_r) :-
employee(X,bigco). ") " One rule like that replaces a great number
of conventional access control list items. Security statements in Binder
are therefore concise. Binder can easily express role-based access
control, delegation, and quite complex security policies, for example
" (cite "Binder") ":")

  ; Use a complex condition: vouched by two managers
(verbatim
  "can(X, read, resource_r) :-"
  "   employee(X, bigco),"
  "   boss(Y, X),"
  "   approves(Y, X, read, resource_r)."
  "employee(john_smith, bigco)."
  "boss(fred_jones, john_smith)."
  "approves(fred_jones, john_smith, read, resource_r)."
  )

"The first statement in the above certificate is a rule stating that
any employee of a BigCo may read " (code "resource_r") " provided such an
action is approved by his boss. The other three statements are facts
about employees of BigCo and the approval action. More examples along
with their detailed descriptions can be found in the Binder paper "
(cite "Binder") "."

(p "Granting access to a resource in Binder is " (em "deriving") "
an atom that asserts such a permission, e.g., an atom " (code
"can(john_smith,read,resource_r)") " in the example. The
derivation constitutes a " (em "logical proof") " of the
permission. The proof can be generated by a service, in polynomial
time. Alternatively, a client can generate a proof and submit it with
the request. The service needs merely to check the proof. The latter
approach distributes the load of authorization computations and helps
prevent denial-of-service attacks.")
(p "Binder programs do not contain negation. Therefore, Binder is
monotonic: adding more statements can only make more atoms
provable. In other words, we cannot cause elevated access permissions
by withholding statements.")

(p
  "Binder is specifically designed for a distributed computing
environment. Each authorization service has its own Binder context. A
context with a set of facts and rules can be exported into a signed
certificate and transmitted to another service. Statements in an
exported context are marked with the identity of the exporting service
using the quotation form " (code "says") ". A service can import a
context and use the context's statements in local proofs if the local
service trusts the remote one. The trust relationship is itself
expressed as a set of Binder statements.")
(p
  "Identities of Binder principals -- for instance, identities of the
exporting services -- are represented by cryptographic keys. The latter
may be encoded in the format described in " (cite "RFC2792") ". One may
bind a local name to a cryptographic key for easy reference, e.g.,
" (cite "Binder") ":")

(verbatim
  "employee(X, bigco, full_time) :-"
  "  Y says employee(X, bigco, full_time),"
  "  bound(bigco_hr, Y)."
  "bound(bigco_hr, rsa:3:c1ebab5d)."
  )
"The local context with its name " (code "bigco_hr") " can be exported
in turn.  This feature lets Binder simulate the linked name spaces of
SDSI/SPKI, but without built-in language support."

(p "The paper " (cite "Binder") " states the following distinguished
features of the system:")
(ol
  (li "A statement in Binder can be translated into a declarative, 
stand-alone English sentence.")
  (li "Binder programs can explicitly define new, application-specific
predicates, which can act as lemmas in proofs. Predicates can be
defined recursively. Rich proofs are allowed.")
  (li "Certificates may contain arbitrary facts and rules, over local,
application-specific -- or remote and quoted predicates. Certificates
can be safely interpreted outside their exporting context.")
  (li "Binder statements can appear in certificates, in policies, in
ACLs, and elsewhere, and these statements can inter-operate freely.")
  (li "Binder queries are decidable in polynomial time.")
  )

(p "Section 7 of the paper " (cite "Binder") " compares Binder with
X.509 Certificates, SDSI and SPKI, PolicyMaker, KeyNote, SD3 and
similar logic-based security languages, and digital rights management
languages. The paper shows that none of those systems possesses all five
key Binder properties.")


;----------------------------------------------------------------------
(hr)
(Section 2 "SAML")
(p
  "SAML is a Security Assertion Markup Language " (cite "SAML") ".
SAML seems to be more a certificate format and a certificate transport
format than a trust management language.")
(p
  "It seems that DecisionType of a SAML assertion only specifies
Permit, Deny and Indeterminate. KeyNote provides for far more variety
of decisions.  The conditions on the assertion are also far less
expressive: NotBefore, NotOnOrAfter, <AudienceRestrictionCondition>,
<DoNotCacheCondition>.")


;----------------------------------------------------------------------
(Section 2 "RTC:" " Datalog with constraints")
(p
  "Ninghui Li and John C. Mitchell have proposed a family of
declarative trust-management languages based on Datalog with
constraints " (cite "RTC") ".")



;----------------------------------------------------------------------
(Section 2 "Policy checking")

(p
  "An access control system advises an application if an action
requested by a particular principal is consistent with a security
policy. We may also need to check if the security policy itself is
consistent, that is, if it actually protects valuable resources. In a
policy with many rules, the overall effect may be difficult to
see. Unpleasant surprises do happen in practice:")

(blockquote
  "Firewalls that rely on chained rule sets are vulnerable to
cascade failures -- a change in one rule can have an effect on every
rule that follows it. I've seen systems that relied on a firewall to
block services that were only supposed to be available on the local
network, but which were made available to the entire Internet due to
the unforeseen results of a firewall rule change. " (cite "Firewalls") 
  " (p. 35)")

(p "The first quantitative study of firewall configuration errors
" (cite "Firewall-errors") " found the results dismal. ``Only one of the
37 firewalls exhibited just a single misconfiguration. All the others
could have been easily penetrated by both unsophisticated attackers
and mindless automatic worms.''")

(p "To prevent such unforeseen results we need to check policy's
invariants and consistency. Unfortunately, many access control systems
have quite low expressivity, which results in a large set of
rules. For example, SELinux policy has around 50,000 statements. We
need automated tools to verify policies. The tools must be built on
firm logical foundations. Because the policy check is an off-line
process (executed only when the policy is updated), the performance of
the tools is not of prime importance.")

(p "Unfortunately, some of the access control systems such as KeyNote
have not been designed with policy checking in mind: in general,
policy checking in KeyNote is undecidable " (cite "RTC") ".")


(p "One real-life example of policy checking is testing that SELinux
policies are consistent with the trusted computer base requirements:
`Analyzing Integrity Protection in the SELinux Example Policy' by
Trent Jaeger, Reiner Sailer, Xiaolan Zhang presented at USENIX
Security Symposium 2003 " (cite "VALI") ". The authors have developed
a Gokyo policy analysis tool, which seems to rely on a human-aided
exhaustive search. No inference seem to be present. In fact, the words
`infer' and `formal' are not even mentioned, and the word `logic'
occurs only in the title of two referenced papers. It is not clear how
the tool itself was verified -- if it was at all. Perhaps the stress
must be on rigor rather than on the development of visual tools.")


;----------------------------------------------------------------------
(hr)
(Section 2 "Datalog")

(p "Datalog seems to be the foundation for many logic-based access
control languages and systems.")

(p "Introduction to datalog, top-down and bottom-up strategies,
and Herbrand interpretation:")
(blockquote
  "Computational Intelligence, a logical approach. D. Poole,
A. Mackworth and R. Goebel.Oxford University Press, 1998.  ISBN
0-19-510270-3. Chapter 2")
"The handouts are available at "
(URL "http://www.cs.kuleuven.ac.be/~gerda/hb43/")

(p "A more advanced comparison of top-down and bottom-up strategies:")
(blockquote
  "Datalog Bottom-up is the Trend in the Deductive Database Evaluation
Strategy. Yurek K. Hinz"
  (URL "http://faculty.ed.umuc.edu/~meinkej/inss690/hinz.pdf"))


(p "Even more advanced, and more detailed papers:")
(blockquote
  "Greedy Algorithms In Datalog. Sergio Greco and Carlo Zaniolo."
  (URL "http://www.cs.ucla.edu/~zaniolo/papers/tplp01.pdf"))
(blockquote
  "Top-Down vs. Bottom-Up Revisited. "
  "Ramakrishnan, R., Srivastava, D., & Sudarshan, S. (2000)."
  (URL "http://citeseer.nj.nec.com/374977.html"))
(blockquote
  "Magic Sets and Other Strange Ways to Implement Logic Programs. "
  "Francois Bancilhon, David Maier, Yehoshua Sagiv, Jeffrey D. Ullman. "
  "PODS 1986: 1-16"
  (URL
    "http://www.informatik.uni-trier.de/~ley/db/conf/pods/BancilhonMSU86.html")
  )
  
(blockquote "Voronkov, A. (1999). Deductive Database.
Computing Science Department Uppsala University, Uppsala, Sweden."
  (URL "http://www.csd.uu.se/~voronkov/ddb.htm"))


(dl
  (dt "Answer-set programming")
  (dd (URL "http://www.cs.utexas.edu/users/vl/tag/")
    (URL "http://www.cs.utexas.edu/users/tag/")))



(dl 
  (dt "XSB: An efficient logical system with top-down and bottom-up
strategies.  XSB can evaluate according to Well-Founded Semantics 
through full SLG resolution.")
  (dd (URL "http://www.cs.sunysb.edu/~sbprolog/manual1/index.html")))


(h5 "Disjunctive Datalog")

(p "A disjunctive Datalog system DLV is the very first system
supporting full disjunctive logic programming with answer set
semantics.  It supports answer set semantics for full disjunctive
logic programs with negation, integrity constraints, queries, and
arithmetic built-ins.")
(dl
  (dd "DLV Home page:"
    (URL "http://www.dbai.tuwien.ac.at/proj/dlv/"))
  (dd "DLV Tutorial"
    (URL "http://home.cern.ch/~chkoch/dlv/dlv_tutorial.html")))

(p "A graph coloring problem in the tutorial illustrates the
advantages of DLV. The problem is to find out if a given map of
countries can be colored with three colors. No two neighbor countries
should have the same color. The map, of Mid-Western U.S. states in the
example, as represented as a set of nodes and arcs. Arcs connect
neighboring states.")

(verbatim
  "node(minnesota). node(wisconsin). node(illinois). node(iowa). ..."
  "arc(minnesota, wisconsin). arc(illinois, iowa)."
  )

(p "The problem is solved by the following DLV program with only two
statements:")
(verbatim
  "% guess coloring"
  "col(Country, red) v col(Country, green) v col(Country, blue) :- node(Country)."
  ""
  "% check coloring"
  ":- arc(Country1, Country2), col(Country1, CommonColor), col(Country2, CommonColor)."
  )

"The first statement is a disjunctive rule that guesses a
coloring. The second statement expresses the strong constraint that
deletes all those colorings that do not satisfy our requirements (that
there may be no arc between two nodes of equal color). DLV solves the
problem rather efficiently."



; (Slide "Proposal"
;   (p "KeyNote is the most mature but has problems. Its problems are our
; opportunities")

;   (p "Implement KeyNote:")
;   (ul
;     (li "In a safe language, this time")
;     (li "Explicit declaration of attributes. Type inference?")
;     (li "Prevent self-references"))
;   (p "Implement Binder or Datalog with Constraints (RT)?")
; )

; (Slide "Off-line checking of policies"

;   (p
;     "No conversion error can cause
; the elevation of the trust value.")
;   (p "A  particular resource will always be protected"))

; (Slide "Logic system"
;   (ul
;     (li "Logic system with first-class relations, lexically-scoped logical variables, embedded in Scheme")
;     (li "Has true relation union, iterative deepening strategy, deterministic evaluation, declarative cuts")
;     (li "Purely declarative subset of Scheme")
;     (li "Easy to reason about: only " (greek "beta"))
;     (li "Performance (interpreter): 3-4 times slower than SWI-Prolog")
; ))

(hr)

(References
    
  (bibitem "RFC2704" "RFC2704"
    "M. Blaze, J. Feigenbaum, J. Ioannidis, A. Keromytis. "
    "The KeyNote Trust-Management System Version 2. "
    "RFC 2704. September 1999."
    (URL "http://www.rfc-editor.org/rfc/rfc2704.txt"))

  (bibitem "KeyNote" "KeyNote"
    (URL "http://www.crypto.com/trustmgt/kn.html"))

  (bibitem "RFC2792" "RFC2792"
    "M. Blaze, J. Ioannidis, A. Keromytis. "
    "DSA and RSA Key and Signature Encoding for the 
KeyNote Trust Management System."
    "RFC 2792. March 2000."
    (URL "http://www.rfc-editor.org/rfc/rfc2792.txt"))

  (bibitem "DisCFS" "DisCFS"
    "S. Miltchev, V. Prevelakis, S. Ioannidis, J. Ioannidis,
A.D. Keromytis, J.M. Smith. "
    "Secure and Flexible Global File Sharing. "
    "Proc. USENIX 2003, FREENIX Track, pp. 165-178."
    (URL 
      "http://www.usenix.org/events/usenix03/tech/freenix03/miltchev.html"))

  (bibitem "Binder" "Binder"
    "J. DeTreville. Binder, a logic-based security language. "
    "IEEE Security and Privacy, 2002. "
    (URL "http://research.microsoft.com/research/pubs/view.aspx?tr_id=545")
    )


  (bibitem "RTC" "RTC"
    "Ninghui Li, J.C. Mitchell. "
    "Datalog with constraints: a foundation for trust management languages. "
    "Proc. PADL2003: Practical Aspects of Declarative Languages. "
    "LNCS 2562, pp. 58-73.")

  (bibitem "Logic-AC" "Logic-AC"
    "M. Abadi. Logic in Access Control. "
    "Proc. of the Eighteenth Annual IEEE Symposium on
Logic in Computer Science (June 2003), 228-233."
    (URL "http://www.cse.ucsc.edu/~abadi/Papers/lics2003.pdf"))


  (bibitem "SD3" "SD3"
    "T. Jim. SD3: A trust management system with certified evaluation. "
    "Proc. 2001 IEEE Symposium on Security and Privacy, pp. 106-115.")

  (bibitem "SAML" "SAML"
    "Security Assertion Markup Language (SAML).  Version 1.1. September 2003. "
    (URL "http://xml.coverpages.org/saml.html")
    (URL "http://www.oasis-open.org/committees/tc_home.php?wg_abbrev=security")
    )

  (bibitem "Firewalls" "Firewalls"
    "A. Singer, San Diego Supercomputing Center. Life without firewalls. "
    "USENIX ;login:, Dec 2003, v28, N6, pp. 34-41.")

  (bibitem "Firewall-errors" "Firewall-errors"
    "Avishai Wool. "
    "A Quantitative Study of Firewall Configuration Errors. "
    "IEEE Computer, June 2004, pp. 62-67")

  (bibitem "VALI" "VALI"
    "IBM T. J. Watson Research Center. Linux Security Analysis Tools. "
    (URL "http://www.research.ibm.com/vali/"))

  )
(footer)

)))

;========================================================================
;			HTML generation

; IMPORT
; SXML-to-HTML-ext.scm and all of its imports


; Generating HTML

(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content
   (generic-web-rules Content 
     `((greek *macro* . ,(lambda (tag elem) `(code ,elem)))
       (math *macro* . ,(lambda (tag elem) `(code ,elem)))
       (References *macro* . 
	 ,(lambda (tag . elems) `((Section 2 "References") ,@elems)))
       (abstract *macro* .
	 ,(lambda (tag . elems)
	    `(div (@ (align center)) ,@elems))))))))

(generate-HTML Content)

; LocalWords:  Datalog KeyNote Google RTC SDSI SPKI PolicyMaker SAML SELinux
; LocalWords:  Herbrand blockquote DLV bibitem
