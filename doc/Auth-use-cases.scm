(define Content
 '(html:begin
   (Header
    (title "Use cases for the Soutei trust-management system")
    (description "Use cases and details for Soutei.")
    (Date-Revision-yyyymmdd "20051207")
    (Date-Creation-yyyymmdd "20040107")
    (keywords "Binder, Soutei, Keynote, Datalog, Authorization, logic system.")
    (AuthorAddress "oleg-at-okmij.org")
    (Author "Oleg Kiselyov")
    (rcs-id "$Id: Auth-use-cases.scm,v 2.9 2005/12/08 03:01:57 oleg Exp oleg $")
    (long-title "Soutei: syntax, semantics, and use cases")
    (Links
     (start "Authorization.html"
       (title "Authorization and Role-Based Access control"))
     (contents "Authorization.html")
     (prev "Authorization.html")
     (next "Auth-transport.html")
     (top "index.html")
     (home "http://soutei.sf.net/")))

  (body
   (navbar)
   (page-title)

   (TOC)

; Alan Karp from HP, in his talk Oct 19 2005, advocated `authorizations'
; They are something like `capabilities'. Although they are better viewed
; as `promissory notes'.
; Traditional Scheme: client contacts a server (aka, `Application')
; and submits its credentials. The server relays the credentials to
; an authorization engine (e.g., Soutei) and asks for advice. If the engine
; recommends the action, the server performs it.

; Alan Karp Scheme's: a server prepares `authorizations' (which, in
; the simplest case, are just long random numbers) and gives them to
; the authorization system. A client contacts an authorization system
; and tells it the client's credentials as well as a desired
; action. The system gives the client an authorization for an action, if
; found advizable. The client then contacts the server, passing its
; requests along with the authorization. The server verifies the authorization
; (e.g., via a lookup in its tables), and, if successful, performs the
; action.
; In Alan Karp's scheme, a server never contacts the authorization engine
; in the course of fulfilling a request. An authorization can be
; considered a `shortcut' of such a communication. The authorization system
; dispenses authorizations but it doesn't know their meaning. Only the
; server that prepared an authorization understands what it means; for
; everybody else, an authorization is just an opaque bytestring.
;
; An authorization may just as well be a Soutei assertion encrypted
; with the server secret key. Noone but the server can read that
; assertion let alone modify it. The assertion may enumerate
; conditions (time of the day, etc.) under which the server promises to
; perform the action. So, a server may still contact Soutei engine when
; fulfilling a request. That engine however will use a local policy
; and a local vocabulary. Nobody but the server needs to know
; teh meaning of terms used in formulating the authorization: nobody
; can even read (decrypt) the authorization but the server itself.



; From KeyNote:
;    Trusted actions to be evaluated by KeyNote are described by a
;    collection of name-value pairs called the `action attribute set'.
;    Action attributes are the mechanism by which applications communicate
;    requests to KeyNote and are the primary objects on which KeyNote
;    assertions operate.  An action attribute set is passed to the KeyNote
;    compliance checker with each query.
;   Each action attribute consists of a name and a value.  The semantics
;    of the names and values are not interpreted by KeyNote itself; they
;    vary from application to application and must be agreed upon by the
;    writers of applications and the writers of the policies and
;    credentials that will be used by them.

; Our application namespace is action attr set, in parts


;
; Logic without function symbols is equivalent to logic with function
; symbols, _provided_ we have the infinite supply of constants.
; For example, Foo(f(x),g(x)) can be represented as
;	exists y y'. F(x,y) & G(x,y') & Foo(y,y')
; So, each function symbol f of arity n is represented as a predicate
; F of arity n+1. That predicate is a functional predicate: 
; for each combination of the first n arguments, there exists exactly one
; object (for the last argument) that makes the predicate hold.
; Since terms in logic are finite, the infinitely countable supply
; of constants is enough.
;
; In Datalog however, the extensional database is finite. 
; There is a finite number of facts and hence the finite number of
; constants.  That breaks the above correspondence, and so terms like
; f(f(f(... x))) can no longer be emulated as the corresponding F
; predicate is finite (its domain is finite). That's why in pure
; Datalog (without negation or with stratified negation), termination
; is assured. This is not the case in pure Prolog because of the possibility
; to construct longer and longer terms like succ(succ(x)).
;
; The modes inference below outlaws programs like
; foo(?x) :- foo(?x).
; So, the properly moded Datalog program (where all variables are
; safe, that is, range limited) is terminating in a (fair)
; top-down evaluator-- just as is the case in simply-typed lambda-calculus.
; So, if we do whole-program mode checking, we can assume termination
; and so need to impose no time limits on reaching the decision.
; However, we don't (and can't, in general) do the full program analysis.
; The mode system below will admit
;	Assertion A: foo(?x) :- B says bar(?x).
; because the mode system assumes the predicate `bar' in assertion B
; is well-moded. Likewise, the system accepts
;	Assertion B: bar(?x) :- A says foo(?x).

; So, non-termination in the top-down evaluator may happen. To even
; find all assertions that a particular assertion may refer to, we
; need abstract interpretation: indeed, we may have assertions like
;  Baz says baz(?x), ?x say quux(?z)



; Introduce predicates of zero arity.

; How to create assertions for newly created objects?


; I read it
; this afternoon and feel that at a minimum, you need some kind of
; figure tying down the names of the people and the various
; certificates.  Imagine that you are in a room trying to explain this
; to someone with only a blackboard and a piece of chalk.  What kinds of
; pictures would you draw?  Also, I did not see anything resembling a
; conclusion.

; The problem is that I did not see anything in your
; original writings about a "high-level configuration file" nor do I remember
; seeing anything about namespaces and the fact that rules in the namespace
; hierarchy would be controlled by different people. Also, the point about
; signatures on rules is not very clear. I think a separate brief document
; that talks about these issues, as distinct from a paper that discusses
; syntax, would be very useful.


; - Identity is a name meaningful within the domain of application
; (from SPKI)
; - Owner identities don't work very well as certificate ID's
; - Owner identity is rarely of security interest (authorization/
;    capabilities are what count)
; Identity
;    - Use a locally meaningful identifier
;       ­ User name
;       ­ email address
;       ­ Account number
;    - Don't try and do anything meaningful with DNs
;       ­ Treat them as meaningless blobs
; - SPKI
;    ­ Binds a key to an authorization
; We do to an assertion.

; TLS Extension mechanism
; http://www.gnutls.org/ especially KSBA library
; http://www.post1.com/home/ngps/m2/howto.ca.html
; http://www.mozilla.org/projects/security/pki/nss/
; cmsutil from the above project
; or use
; http://www.cs.auckland.ac.nz/~pgut001/cryptlib/smime.html
; libksba port, libtasn1
; srm, secure rm


;----------------------------------------------------------------------

(Section 2 "Introduction")
(p
  "This page introduces a trust management system " (em "Soutei")
". The system is essentially based on Binder. However we have
specified many aspects left undefined in the original DeTreville's
paper " (cite "Binder") ", and added several extensions.  The most
notable extension is the disequality predicate and a type system to
guarantee its static safety and the preservation of monotonicity.  Many
of the extensions -- such as the interaction between an application
and Soutei, communicating the context of the request to the decision
engine -- are notably influenced by the KeyNote system. We have
specified the protocol for sending Soutei assertions in HTTP requests
and in various certificates. We have also clarified the logical
meaning of the distinguished Binder/Soutei relation
" (code "says") ".")
(p
  "In the following, we first describe Soutei's syntax, which differs
from that of Binder. We added numeric and IP address literals. We
changed the syntax of variables and constants to inhibit user errors
that may have serious security consequences. We added a disequality
predicate, and a type system that statically ensures that disequality
and other restricted-mode predicates are safe and do not
destroy the overall monotonicity. A Soutei assertion that is found
unsafe is rejected by the compiler.  We specify semantics of
Soutei. In particular, we concentrate on the " (code "application")
" context and on the logical and operational interpretations of
" (code "says") ". We describe the use of Soutei for role-based or
capability-based access control, and the revocation of privileges.")
(p
  "Finally, we introduce a number of use cases. The cases are meant to
illustrate notable aspects of the system -- in particular, several
levels of trust delegation. We test both granting and rejecting user
requests. The test cases are meant to be executable, and so to
constitute an acceptance test of an early prototype and the regression
test of the finished system.")

(p "A separate page " (cite "Transport") " discusses various ways of
making Soutei assertions available to the decision engine: by querying
trusted databases, LDAP servers, or by delivering the assertions in
HTTP headers and in Public-key and Attribute X.509 certificates.")


; Need a conclusion?


(Section 2 "Syntax" " of Soutei")

; Term-lit are spelled as '"string"
; nonterms are just strings

(div (@ (align center))
 (productions
  (production 1
    "Assertion"  (ebnf-* "Statement") "; also called clause")
  (production 2
    "Statement"
    (ebnf-choice "Rule" "Fact"))
  (production 3
    "Rule"
    ("Head" '":-" "Body" '"."))
  (production 4
    "Head" "pAtom")
  (production 5
    "Fact" ("Head" '"."))
  (production 6
    "Body"
    ("Atom" (ebnf-* (ebnf-group '"," "Atom"))))
  (production 7
    "Atom"  (ebnf-choice "cAtom" "pAtom"))
  (production 8
    "pAtom"
    ("Predicate" '"(" "Args" '")"))
  (production 9
    "cAtom"
    ("Context" '"says" "pAtom"))
  (production 10
    "Predicate" "symbol-constant")
  (production 11
    "Args" 
    ("Term" (ebnf-* (ebnf-group '","  "Term"))))
  (production 12
    "Term"    (ebnf-choice  "constant" "variable"))
  (production 13
    "Context"  "Term")
  (production 14
    "variable" (ebnf-choice '"?" ('"?" (ebnf-+ "id-symbol"))))
  (production 15
    "constant"
    (ebnf-choice
      "string-constant"
      "numeric-constant"
      "symbol-constant"
      "ip-constant"
      "ipnetwork-constant"
      ;"pubkey-constant"
      ))
  ))

(p "Clauses with the same predicate must be consecutive.")
; Predicates at the head are not qualified by a context.  

(p "Lexical entities:")
(div (@ (align center))
 (productions
  (production L1
    "string-constant"
    (n_)
    "A quoted Scheme string")
  (production L2
    "numeric-constant"
    (n_)
    "A real Scheme number")
  (production L3
    "symbol-constant"
    (n_)
    "A Scheme symbol")
  (production L4
    "ip-constant"
    (('"#p" "IP4-quad or IP6-address")))
  (production L5
    "ipnetwork-constant"
    (('"#n" "IP4-quad or IP6-address" '"/" "number of significant bits")))
  ; Add a clarification or example for an IP address
  ; <pubkey-constant> : A Scheme string in the format described in RFC2792
  (production L5
    "id-symbol"
    (n_)
    "A lower- or upper-case character or digit that can appear in a
Scheme symbol")
   ))

(p 
  "As usual, non-terminals of the grammar are separated by
whitespace. Whitespace between two non-terminals may be elided if the
non-terminals can still be parsed separately. A semi-colon introduces
a comment that spans through the closest line terminator: a line-feed
character, a carriage-return character, or a combination of line-feed
followed by carriage-return. A comment is considered whitespace.")

(p "The syntax differs in insignificant details from the language
proposed in DeTreville's paper, and also from Prolog/Datalog. The main
difference is the syntax of variables and constants. In the original
Binder, as in Prolog on which it is based, capitalized symbols denote
variables. The other symbols are constants. In a security language,
symbols and variables stand for the names of people, programs, and
computers. These names have different capitalization
conventions. We must keep in mind that the main users of Soutei
are not Prolog programmers. Rather, they are system and security
administrators. Therefore, we should avoid glaring inconsistencies
between the lexical conventions of the language and the domain.")
(p "There is a notably important reason to make the names of Soutei
variables stand out. In the original Binder, an assertion")

(verbatim
  "can(Pubkey,resource_r,read) :- pubkey(john,Pubkey)."
  "pubkey(john,\"0123436\")."
  "pubkey(doug,\"abcde\"). ..."
  )

"allows the user " (code "john") " to read " (code "resource_r")
". However, if by mistake the administrator spelled the name of the
user in the conventional capitalization:"

(verbatim
  "can(Pubkey,resource_r,read) :- pubkey(John,Pubkey).")

(p "then he unwittingly allowed access to " (code "resource_r") " to
" (em "every registered user!") " The confusion between the domain
notation and the language notation can have grave
consequences. Therefore, we choose to prefix the names of all variables
with the question-mark character, to make the variable names really
stand out. That convention hopefully inhibits the errors of great
security consequence -- but it cannot assuredly eliminate them. We
must subject every Soutei assertion to a policy check, described later
on this page.")

(p "Thus the main syntactic difference from Prolog and the original Binder is
that in our version, the names of all variables begin with the
question mark. A single question mark stands for the anonymous
variable. Other symbols -- capitalized or not -- are constants. Quoted
strings are also considered constants. In fact, we do not draw any
semantic distinction between symbolic constants and quoted strings. If
the name of a constant starts with a question mark or contains spaces,
parentheses, commas and other characters that are not permitted in
Scheme identifiers, the name of the constant must be enclosed in
double quotes. Otherwise, the name may be enclosed in double
quotes. All names are case-sensitive.")

(p "We also extend the original Binder with numerical literals and
literals for IP addresses. Internally, IP addresses are represented as
32 or 128-bit exact integers.")


(hr)
;----------------------------------------------------------------------
(Section 3 "Safety" " of assertions")





(p "A Soutei assertion must satisfy safety constraints as described
below.  An assertion that fails to satisfy the safety property is
rejected by the compiler and cannot be added to the system.")

; A Datalog-program is said to be safe if its bottom-up
; processing terminates on all valid inputs.

(p "Our notion of safety corresponds to well-moded logical programs,
as described in " (cite "Mercury-Modes") ".  However, the treatment of
modes in Mercury is too operational, relying on notions such as
`predicate call' and the instantiatedness of a variable `before' and
`after' the call. In a bottom-up evaluation, there are no notions of
`operator call' at all.")

(p "We are concerned about safety because of standard predicates such
as " (code "neq") ", the disequality predicate. If used without
restriction, the predicate destroys the formal properties of a logical
program, e.g., the existence of a minimal model -- let alone
monotonicity. Even the relation " (code "says") " is problematic unless
restricted, as in " (code "?X says may(read)") " where " (code "?X") "
is an uninstantiated variable. Should we try to enumerate all possible
namespaces and try " (code "may(read)") " in each of them?  The
problems with " (code "neq") " and " (code "says") " can be eliminated
with run-time checks.  However, that is not satisfactory: the monotonicity
property is compromised, seemingly unrelated changes to assertions may
lead to run-time errors, the ability of the system to optimize
assertions is restricted. Therefore, we would like to statically reject
assertions that are unsound or can cause run-time instantiatedness
failures. We would like to admit only those assertions that
can assuredly be evaluated without the above problems.")

; Classical logic is monotone. Negation as finite failure is different from
; NOT because it adds non-monotonicity.
; So, our neq is an ability to define _classical negation_: neq(X,Y)
; holds if we can positively derive that X and Y are different.

(p "Thus we would like to introduce static safety and
monotonicity-preservation guarantees for mode-limited predicates and
especially for the limited form of negation -- disequality. Not only
we have to ensure that disequality can be evaluated in the context where
its arguments are fully instantiated. We also have to guarantee that
that the domain of " (code "neq") " arguments is invariant of remote
assertions. The latter preserves monotonicity: adding more
assertions to the system does not reduce the set of allowable security
actions. Our notion of monotonicity is identical to that of KeyNote: 
`` An important principle in KeyNote's design is `assertion
monotonicity'; the policy compliance value of an action is always
positively derived from assertions made by trusted principals.
Removing an assertion never results in increasing the compliance
value returned by KeyNote for a given query.'' [RFC2704] " (cite "KeyNote"))

(p "Like in Datalog, to make sure " (code "neq") " and similar
predicates are safe, we introduce the notion of range-limited
variables: variables whose set of possible values is known to be
finite. However, the set of values may be defined by statements in
remote namespaces (assertions), which may need to be fetched and may
be unavailable due to network failures, etc. It is imperative to avoid
the situation where the system grants a request that would have been
denied had the network had operated normally.")

; So the safety rule:
; a fact is safe if it contains no variables.
; a rule is safe if all of its variables are safe
; a variable is safe if it appears in at least one (positive)
; atom in the body of the rule whose
; predicate is safe

(p "Therefore, we introduce " (em "statically range limited
variables") ": whose range of values is fully known at the time the
query begins evaluation. For example, given " (code "application says
ipaddress(?IP)") ", we know that the variable " (code "?IP") " is
statically range limited, because the set of its possible values --
which is one, the IP address of the requesting client -- is fully known
at the time query begins evaluation. Similarly, if a namespace
defines a predicate " (code "access_mode") " by the two clauses:
" (code "access_mode(read). access_mode(write).") " then the variable
" (code "?X") " in an atom " (code "access_mode(?X)") " is statically
range limited because the set of possible values for " (code "?X") "
is known and does not depend on any remote assertions.")

(p "To formulate statically verifiable safety conditions, we introduce
the following type system.")

(p "Terms are associated with one of the three types:")
(ul
  (li (strong (code "S")) " -- Statically range limited")
  (li (strong (code "L")) " -- Range limited")
  (li (strong (code "A")) " -- Any, including uninstantiated"))

(p "The types are ordered: " (code "A < L < S") ". We let the
meta-variable " (code "ttype") " range over these term types.")

(p "Every argument of a predicate and the first argument of the
relation " (code "says") " are assigned one of the following types:
" (strong (code "PS")) ", " (strong (code "PL")) ", " (strong (code
"RS")) ", " (strong (code "RL")) ", " (strong (code "A")) ". Here the
letter P stands for `provides' and the letter R stands for
`required'. The types are ordered: " (code "RS < RL < A < PL < PS")
". We let the meta-variable " (code "ptype") " range over these types.")

(p "The first argument of the relation " (code "says") " has the type
" (code "RL") ". Predicates in the " (code "application") " namespace
have the types assigned to them.  The types of other predicates are
inferred.")

(p "Informally, an assertion is safe if all the following three
conditions hold:")
(ul
  (li "the type of every argument of every predicate defined in the
assertion is inferred to be " (code "PL") " or " (code "PS") ",")
  (li "in every atom, if a predicate argument type is " (code "RS") ", the 
corresponding argument term has the type " (code "S") ",")
  (li "in every atom, if a predicate argument type is " (code "RL") ", the 
corresponding argument term has the type " (code "L") " or " (code "S") ".")
)

(p "The following is a formal system to determine the safety of an
assertion.  The results can also be used to reorder conjunctions.")

(p "Meta-variables:")
(ul
  (li (code "term") " -- term")
  (li (code "atom") " -- atom")
  (li (code "atom*") " -- perhaps empty sequence of atoms")
  (li (code "atom+") " -- a non-empty sequence of atoms")
  (li (code "var") " -- variable")
  (li (code "ttype") " -- term type")
  (li (code "ptype") " -- predicate argument type")
  (li (code "pred/n") " -- a predicate, local or context-qualified, of
arity " (code "n") ". The arity may be omitted if understood from the
context.")
  (li (code "i") " -- " (code "1..n") ", where " (code "n") " is the
arity of the predicate in question.")
  (li (code "c") " -- ranges over all clauses that define a predicate
in question.")  )

(p "The meta-constant " (code "[]") " stands for an empty sequence.")

(p "The notation " (code (P pred/n i)) " specifies the argument " (code "i")
" of the predicate " (code "pred/n") ", whereas " (code (P pred/n c i)) "
specifies the argument " (code "i") " of the predicate " (code
"pred/n") " that appears in the head of the clause " (code "c") ".")


(p "Meta-functions:")
(ul
  (li (code (F Clauses pred/n)) 
    " -- all clauses that define the local predicate " (code "pred/n"))
  (li (code (F FV atom)) " -- variables that appear in " (code "atom"))
  (li (code (F Dom Gamma)) " -- domain of a type environment")
  (li (code (F GExt Gamma var ttype)) " -- extension of the type environment,
defined below") 
)

(p "Predicate typing judgments:")


(p (code (Jpi pred/n i ptype)) " asserts that the argument " (code "i") " of
the predicate " (code "pred/n") " has the type " (code "ptype") ".")


(p "Axioms for the types of predicates")

(pc (A (Jpi says 1 RL)))
(pc (A (Jpi "application says pred" i ptype))
  " See the section about the application namespace.")
;??||-pi system says pred_i : ptype -- as inferred when typechecking system ns
(pc (A (Jpi "term says pred" i PL)))
(pc (R ((forall (c (F Clauses pred/n)) (Jpi (P pred/n c i) ptype_c))
       "ptype is lwb ptype_c")
      (Jpi pred/n i ptype)))

(pc (R "Clause c of pred/n is a fact"
     (forall (i n) (Jpi (P pred/n c i) PS))))

(pc (R "Clause c of pred/n is a rule"
     (forall (i n) (Jpi (P pred/n c i) PL))))



(p "Type environment")

(p "The type environment Gamma associates variables within a clause
with their " (code "ttype") "s.")

(productions
  (production Gamma
    "Gamma"    (ebnf-choice  '"[]" (ebnf-group "var : ttype" '"," "Gamma"))))

(p "The extension function of the environment " (code (F GExt Gamma var ttype))
  " is defined as follows:")

(pc (R ("var not in " (F Dom Gamma))
     ("var : ttype, Gamma")))
(pc (R (("var : ttype1 in Gamma")
       ("ttype2 = max ttype ttype1"))
     ("var : ttype2, Gamma\\var")))

(p "Typing of terms")

(p (code (Jt Gamma term ttype)) " asserts that the term has a " (code
"ttype") " in the typing environment Gamma.")

(pc (A (Jt Gamma constant S)))
(pc (R "ttype1 <= ttype"
     (Jt "x:ttype, Gamma" x ttype1)))


(p "Typing of the body of the rule: a sequence of atoms")

(p (code (Jb Gamma atom*)) " asserts that the sequence of atoms is well-typed
in the type environment Gamma.")

(pc (A (Jb "[]" "[]")))
(pc (R (
	(Jb Gamma atom*)
	"ti is a var"
	((n_) (Jpi pred/n i PL) ", " ("Gamma' = " (F GExt Gamma ti L)))
	"OR"
	((n_) (Jpi pred/n i PS) ", " ("Gamma' = " (F GExt Gamma ti S)))
	)
     (Jb "Gamma'" "pred/n(t1,...tn), atom*")))


(h4 "Safety conditions of clauses")

(p (code (Js1 clause)) " asserts that the clause satisfies the safety
condition 1.  This is the safety condition in the Datalog sense: a
" (code (nonterm "Head")) " must not contain variables that are not
used in its " (code (nonterm "Body")) ". In particular, a
" (code (nonterm "Fact")) " may not contain any variables at all. Another way
to look at this safety condition is as the justification of the
inductive assumption in the typing of predicates.")

(pc (R ("c is head :- []"
	((F FV head) " is []"))
      (Js1 c)))

(pc (R ("c is head :- atom+"
	 (Jb Gamma atom+)
	 (forall (v (F FV head)) (Jt Gamma x L)))
      (Js1 c)))


(p (code (Js2 clause)) " and " (code (Js3 clause)) " assert that the clause
satisfies safety conditions 2 resp. 3.")

(pc (R ("c is head :- atom*"
       (Jb Gamma atom+)
       (forall ("pred/n(t1,...tn)" atom*)
	 (forall (ti "(t1,...tn)")
	   ((Jpi pred/n i RS) " --> "
	    (Jt Gamma ti S)))))
     (Js2 c)))

(pc (R ("c is head :- atom*"
       (Jb Gamma atom+)
       (forall ("pred/n(t1,...tn)" atom*)
	 (forall (ti "(t1,...tn)")
	   ((Jpi pred/n i RL) " --> "
	     (Jt Gamma ti L)))))
     (Js3 c)))

(p "The whole Soutei assertion is safe if every clause of it satisfies
all three safety conditions.")

(hr)
;----------------------------------------------------------------------

(Section 2 "Semantics" " of Soutei assertions")

(p "A Soutei engine acts as a compliance checker that provides advice
to applications regarding specific actions. Actions and accompanying
data are described in terms of constants, atoms and
assertions. Assertions are grouped into named " (code (nonterm
"Context")) "s, which are also called namespaces. To be more precise,
each " (code (nonterm "Atom")) " is associated with a particular
" (code (nonterm "Context")) ", via the distinguished predicate
" (code "says") ". There is also a local context for each
assertion. The local context is imputed to each atom without an
explicit context association. A context is identified by its name,
which is related to the issuer of the corresponding security
assertion. For example, the context identifier can be issuer's
distinguished name in a LDAP database. Alternatively, the context
identifier may be the issuer's public key or a hexadecimal string
encoding a SHA-1 hash of issuer's subject name. Such a hash is used in
OpenSSL as an index in a directory of certificates. The Soutei engine
treats context identifiers as opaque strings, with the exception of
two distinguished context names: "(code "system") " and " (code
"application") ".")

(p "The Soutei engine starts up with an initial assertion, which has a
distinguished name " (code "system") ". The initial assertion is
normally written by the server security administrator and is placed
into a trusted configuration file.")

(p "An application invokes the Soutei engine by issuing a query. The
application gives the engine the context with a distinguished
name " (code "application") ". Rules and facts in the application
context describe the client and the requested action. The application
context also includes built-in predicates such as " (code
"ip_of/2") ", and the distinguished predicate " (code "says/2")
".")

(p "The application asks the Soutei engine to prove an atom such
as")
  (verbatim "system says may(X,Y,Z)")

(p "where X,Y,Z are the constants or variables that represent the
resource, the action, or the principal. See the use cases below for
more detail. The Soutei engine replies if the atom can be derived from
the assertions at hand. If the atom can be proved, the engine returns
a set of substitutions for the variables in the atom. If more than one
set of substitutions prove the atom, only the first found set is
returned.")

(p "We should stress that the predicate " (code "may") " is not
built-in. It is just the agreement between a particular application
and a policy writer. An application may choose to ask for
authorization advice using a different predicate with a different
number of parameters.")



  (Section 3 "Application Context")
  (p "The application context defines facts and rules describing the
authorization query. The context also defines built-in predicates. All
statements in the application context are considered to be
non-recursive. Hence the application context is implicitly an
extensional database.")

  (p "Statements of the application context describing the action and
the request:")
  (dl
    (dt (code "ipaddress/1"))
    (dd "A predicate for the IP address of the client requesting the
authorization decision.")
    (dd (code (Jpi "ipaddress/1" 1 PS)))

    (dt (code "pubkey_fingerprint/1"))
    (dd "A predicate for the public key fingerprint (i.e., the subject
identity) of the requesting client.")
    (dd (code (Jpi "pubkey_fingerprint/1" 1 PS)))

    (dt (code "access_mode/1"))
    (dd "Access modes permitted by the application")
    (dd (code (Jpi "access_mode/1" 1 PS)))
)

(p "Additional predicates such as client's distinguished name or
client's voice fingerprint can easily be added if a particular policy
requires that.")

  (Section 3 "Built-in" " predicates")

  (p "The application context may provide built-in utility predicates.")
  (dl
    (dt (code "ip_of/2"))
    (dd "An atom " (code "ip_of(ip,ipnet)") " is provable if the IP
address " (code "ip") " is a part of the network " (code "ipnet") ",
specified as an IP network literal.")
    (dd (code (Jpi "ip_of/2" 1 RL)))
    (dd (code (Jpi "ip_of/2" 2 RS)))

    (dt (code "neq/2"))
    (dd "The disequality predicate. It asserts that its two arguments
are provably not equal. The equality is structural.")
    (dd (code (Jpi "neq" 1 RS)))
    (dd (code (Jpi "neq" 2 RS)))
)


  (Section 3 "says" " predicate")
  (p "The application context also contains a distinguished predicate
" (code "says/2") ". An atom " (code "says(context_id,pAtom)") " is
provable if and only if the atom " (code "pAtom") " is provable in the
context " (code "context_id") ".")
  (pc (Jpi "says" 1 RL))

  (p
  "When the application receives a Soutei assertion the application
determines the issuer and derives the context identifier.  If the
assertion was signed, the application checks the signature. The
application then compiles the assertion and, barring any errors,
extends the predicate "(code "says/2") " with the compiled assertion
and " (code
"context_id.") " Normally the application will cache received
assertions for some time (not exceeding the validity interval of the
certificate used to sign or carry the assertion). The predicate
" (code "says/2") " therefore is an abstraction of that cache. The
application uses the signature bits of the signed certificate to
quickly determine if the received certificate is already in the
cache. If the received assertion is a part of an attribute certificate
with a non-empty holder, the assertion may not be cached. The
assertion can only be used for authorization decisions regarding the
holder.")

  (Section 3 "Logical" " interpretation of " (code "says"))
  (p "The original Binder paper " (cite "Binder") " treats " (code
"says") " as a quotation mark for imported assertions. In that
interpretation, ``context says'' is a modality of an atom. We however
interpret " (code "says") " in a different way. In our view, " (code
"A says B") " means " (code "A |- B") ". The context identifier
becomes the denotation for a set of assumptions in a
natural-deduction-like proof. This view appears to be closely related
to a definitional logic of D.Miller, R.McDowell and
A.Tiu. Interestingly, this interpretation of " (code "says") " does
not seem to be mentioned in any of the papers related to Binder.")

; The export rule of Binder:
;         context q: application says A |- B
;         context p: C, q says B |- may(...).
; ==>
;         context p: C, application says A -> may(...)
; then looks precisely like cut elimination!

  (p "Treating " (code "says") " as the entailment relation lends
itself to a simple realization of Soutei in any (meta-)logic system
with first-class relations, such as " (cite "KANREN") ". The examples
of induction proofs in KANREN illustrate the first-class treatment of
rules and facts. The Binder paper evaluates a set of assertions by
importing them all into one context, quoting their rules
correspondingly. We, on the other hand, refer to other contexts but do
not import them. We search for a proof of a formula in the context of
a particular assertion.")


; declarative semantics: well-founded semantics or minimal semantics?
; It is set based, the order of clauses and conjunctions
; doesn't matter.
; need reordering of clauses during the evaluation.

(hr)
;----------------------------------------------------------------------

(Section 2 "Using" " Soutei")

(Section 3 "Role-based" " access control and Soutei")

(p "Role-based access control is a popular authorization technique. A
trust management system " (cite "KeyNote") " is a far more general
mechanism that can trivially implement the most complex role-based
authorizations. The Soutei engine, for example, bases its decisions
entirely on the client identifiers provided by the requesting
application. These identifiers may correspond to a particular user --
or to a particular role of that user. Soutei supports delegation by a
principal of a (subset) of its privileges to another principal. A user
therefore can delegate subsets of his permissions to various
applications, agents, or representations of himself. Each such agent
corresponds therefore to a particular role played by the user. Such a
delegation is especially trivial if we chose Attribute certificates as
the transport mechanism for Soutei assertions. A user can sign a
number of attribute certificates based off his public key certificate
-- each attribute certificate for a particular role and for a particular
agent.")

(p "The Soutei engine lets the policy author specify hierarchies of
roles and impose side conditions, e.g., a particular role can be used
only if the request comes from a particular network or designated
hosts.")


(Section 3 "Capabilities" " and Soutei")

(p "The client identifier and the context identifier are treated by
Soutei as opaque strings. The identifiers may be meaningful
descriptions of real people, e.g., their e-mail or postal
addresses. On the other hand, the identifiers may be meaningless
unique strings that are given by an administrator to a client in a
certificate or in some other assured way. The administrator can keep
in a special context the record of these unique strings associated
with human readable names of his choice:")

(verbatim
  "capability(\"Dennis\",\"RGVubmlzSnVsMjM=\")."
  "capability(\"John from the second floor, for a printer\","
  "           \"Sm9obiBmcm9tIHRoZSBzZWNvbmQgZmxvb3IsIGZvciBhIHByaW50ZXI=\")."
  )

"If that context has the name " (code "capabilities") ", the policy
writer can use the human readable identifiers in his policies, for
example:"

(verbatim
  "may(printer,?id) :- capabilities says "
  "                        capability(\"John from the second floor, for a printer\","
  "                                   ?id), ..."
  )

"or, if the fact the capability exists is sufficient,"

(verbatim
  "may(printer,?id) :- capabilities says capability(?,?id), ..."
  )

(p "The re-direction afforded by the context such as " (code
"capabilities") " can solve the ``stale resource name problem'': when
a resource is deleted, all access permissions to it should be
terminated. If another resource is created later with the same name,
the users of the old resource should not automatically have any access
rights to the new resource. In Soutei, resource names have no
particular significance and treated as opaque strings.  The naming
conventions and the vocabulary of resources are defined exclusively by
requesting applications. Some applications may name the resource after
the SHA-1 hash of its content. Such names are effectively unique. We
can use translation contexts such as " (code "capabilities") " to
associate the unique names with some other attributes to help write
policies. The context names are themselves first-class and can be
associated with attributes in other contexts. Soutei lets policy
writers implement as extensive chains of reference and redirection as
necessary.")

(Section 3 "Policies predicated on time")

(p "Soutei readily admits policies predicated on time: for example,
the following statement permits write access to a file only during
business hours, subject to supervisor's approval: ")

(verbatim
  "may(\"untitled.doc\",write) :-"
  "  application says this-period(business-hours),"
  "  supervisor says may(\"untitled.doc\",write)."
  )

(p "In contrast, the following statement permits write access outside
business hours:")

(verbatim
  "may(\"untitled.doc\",write) :-"
  "  application says this-period(?period),"
  "  application says neq(?period,business-hours),"
  "  supervisor says may(\"untitled.doc\",write)."
  )

(p "It is of course the job of the application to define what
constitutes business hours, and to determine if the time of the
request falls within that definition. The above examples assumed that
the application communicates its determination to Soutei via the
predicate " (code "this-period/1") ". An application programmer and a
policy writer could have chosen a different predicate and a different
set of terms for time intervals. The Soutei engine
could care less: the engine does not determine time. The engine merely
checks if application-provided facts about application-introduced terms
" (em "syntactically") " conform to the rules of the policy.")

(p "The next section explains how to introduce temporal order among
the time intervals and to use it in policies.")


(Section 3 "Lists, trees, organizational charts, and partial orders")

(p "So far, the policies were relating terms -- e.g., file names, access
rights -- based solely on their equality or disequality. Some
application domains however, such as time intervals, clearances,
organizational charts, are ordered. We would like therefore to
write policies that take the order into account, e.g., to permit
access to a resource only if the user has clearance ``confidential''
" (em "and above") ".")

; In the regular Datalog, which has numbers and arithmetics,
; lists are emulated by a pair of predicates
; list_len(5).
; list_elem(0,el0). list_elem(1,el1)....

(p "One may doubt at first if such policies are at all expressible in
Soutei: after all, terms in Datalog lack any structure and so can be
compared only for equality. However, it is the fact that logic without
function symbols is equivalent to logic with function symbols, " (em
"provided") " the unlimited supply of constants. For example, the
following assertion named " (code "user-list") " represents the list of
three elements, " (code "user1") ", " (code "user2") ", " (code
"user3") ", in that order:")
(verbatim
  "list(l1)."
  "list-el(l1,user1,l2)."
  "list-el(l2,user2,l3)."
  "list-el(l3,user3,nil)."
  )

"The distinguished constant nil represents the empty list. Now we can
write"

(verbatim
  "user-list says list(?l),"
  "user-list says list-el(?l,?,?ln),"
  "user-list says list-el(?ln,?user,?)"
  )
"to find out the name of the second user."

(p "The adjacency predicates such as " (code "list-el") " let us
specify arbitrary trees, lattices and partial orders. For example, we
can represent an organizational chart as the following assertion: ")

(verbatim
  "reports-to(VP-sales,CEO)."
  "reports-to(VP-development, CEO)."
  "reports-to(CFO,CEO)."
  "reports-to(dept-sales-Japan,VP-sales)."
  "reports-to(dept-sales-US,VP-sales)."
  "reports-to(QA,VP-development)."
  "reports-to(OS-division,VP-development)."
  "reports-to(filesystem-group,OS-division)."
  )

"If the above assertion is named " (code "org-chart") ", we can use it
to write the policy that permits access to a document named
``development milestones'' only to those employees that belong to the
development branch, from VP-development downwards:"

(verbatim
  "may(\"development milestones\",?access) :-"
  "  known-access(?access),"
  "  application says this-user-div(?user,?ou),"
  "  path(?ou,VP-development)."
  ""
  "path(?x,?x) :- org-chart says reports-to(?x,?). ; reflectivity"
  "path(?x,?x) :- org-chart says reports-to(?,?x). ; reflectivity"
  "path(?x,?y) :- org-chart says reports-to(?x,?y)."
  "path(?x,?y) :- path(?x,?z), org-chart says reports-to(?z,?y)."
  )

"Incidentally, the last clause of the path predicate exhibits left
recursion. Soutei can deal with it. The path predicate will work even
if the organizational chart is not a tree (e.g., two VPs share the
responsibility for same department) and has cycles.  The example
assumed that the application tells the engine about the organizational
position of the requester via the predicate " (code
"this-user-div/2") ". That data could be extracted from the employee's
PKI certificate, for example."

(p "We can also write a policy that permits access to a document
``proposed reorg'' to employees in the OS-division and " (em "all") " their
managers up the chain:")

(verbatim
  "may(\"proposed reorg\",read) :- "
  "  application says this-user-div(?user,?ou),"
  "  path(OS-division,?ou)."
  )

(p "The assertion " (code "org-chart") " can be managed by a dedicated
person in an HR department, for example, to reflect changes in the
organizational structure. Unless the OS division is renamed, the above
policy needs no modifications. For example, a restructuring in sales 
has no effect on the above policy, as it should not. If a new
position of COO is introduced to whom VP-development reports, the COO
could immediately read the ``proposed reorg'' document under the
policy.")

(p "It remains an interesting question the extent to which Soutei can
emulate authorization systems such as constrained RT and others that
explicitly deal with hierarchies, intervals and structured domains.")


(Section 3 "Revocation")

(p "Just like KeyNote and Binder, the Soutei engine has the property
of monotonicity: making more assertions known to the engine may
potentially cause more authorization queries to be answered
affirmatively -- but never fewer. Therefore, withholding an assertion
from the decision engine can never cause the engine to advise an
action that the engine will not advise otherwise.")

(p "The monotonicity property however makes the revocation of
previously granted privileges challenging (although the disequality
predicate " (code "neq/2") " can greatly help here). We must first
remark that handling of revocations greatly depends on the chosen
mechanism of delivery of assertions to the engine.")

(p "If we use LDAP or other trusted database to store all assertions,
and configure the Soutei engine to consult the database on each
request or frequently enough, revocation becomes trivial. Removing
from the database the assertion that originally granted the privilege
to a particular client automatically revokes the privilege.")

(p "If assertions are delivered to the engine in signed messages or
certificates, the validity of the certificate determines the validity
of the assertion. If the certificate is expired, the corresponding
assertion is effectively revoked. It is argued " (cite "Transport") "
that X.509 Attribute certificates are the most appropriate kind of
certificates for Soutei assertions. Such certificates correspond to
roles of the user or his agent, and are supposed to be issued for a
very short validity period.")

(p "Hybrid assertion transport mechanisms lend themselves to flexible
revocation strategies. For example, a policy can include the
statement:")

(verbatim
  "may(resource,?user,read) :- Admin says may(resource,?user,read),"
  "                            LDAP says valid_user(?user)."
  )

"That is, access to the resource is granted if " (em "both") "
statements can be proved: that Admin has granted the access right to the
user and that LDAP says the user is valid. The administrator may
grant the user privileges by giving him the corresponding
certificate. The user must present the certificate with his
requests. Proving " (code "LDAP says valid_user(?user)") " on the other
hand may involve an " (em "on-line") " check for the user in a trusted LDAP
database. Removing the user identifier from that database instantly
and effectively revokes the certificate Admin gave to the user."

(p "We should also point out that if a client receives access to a
resource via a delegation chain, removing the delegating assertion
from anywhere in the chain terminates the access. At least one
assertion in the chain -- the top one -- must come from a trusted
source, a local configuration file. Assertions that come from such
sources are easy to revoke.")

(p "Finally, the disequality predicate makes it easy to revoke access
for a specific user (a client on a specific network, etc). We should
modify the top, " (code "system") " assertion or other such assertion
by inserting an atom like " (code "neq(?IPAddress,#p10.10.1.127)")
". No other assertion in the system and in delegation chains has to
be modified. The revocation takes effect immediately and we are sure of
that.")

(hr)
;----------------------------------------------------------------------
(Section 2 "Use" " cases")

(p
  "The slides from the June 7, 2005 demo of Soutei and its integration
with Navy Enterprise Single Sign-On (NESSO) and GIG-Lite (formerly,
Metcast) Channels can be found in the
" (code "demo/metcast-channels/doc/") " directory of the
Soutei distribution." (br) "The demo included examples of
delegation and Risk Adaptable Access Control (RAdAC).")

(p (n_))


(p "To formulate the following executable use cases, we need to have a
particular installation of Soutei in mind. We have chosen Metcast
Channels "(cite
"Metcast-Channels") ". For the cases below, we have also chosen the
assertion delivery mechanism: signed messages or X.509 certificates.")


(p "We start with the Metcast Channel database containing two empty
test channels " (code "MEMO") " and " (code "DEMO-IMG") ". We wish to
let any FNMOC client read and write " (code "MEMO") ". We let only
one remote user, named Joe, access the channel. We assign the user
Dean to be the administrator of DEMO-IMG. Dean shall control the
access to that channel.")

(p "To control the access to Metcast databases, we introduce a predicate
" (code "may/3") ". The atom " (code "may(database, resource, right)")
" is provable if the requesting client has " (code "right") " to access
a particular Metcast " (code "database") " named " (code "resource")
".")

(p
  "The Initial assertion:")

(verbatim
  "may(channel,MEMO,?a) :- application says ipaddress(?IP),"
  "                        internal(?IP), access(?a)."
  "may(channel,MEMO,?a) :- known_user(Joe), access(?a)."
  ""
  "may(channel,\"DEMO-IMG\", ?Access) :-"
  "            pubkey(Dean,?Dean_key),"
  "            ?Dean_key says may(channel,\"DEMO-IMG\", ?Access)."
  ""
  "internal(#p10.10.1.1)."
  "internal(?IP) :- application says ip_of(?IP,#n192.168.0.0/16)."
  ""
  "known_user(?user) :- pubkey(?user,?key), pubkey_fingerprint(?key)."
  ""
  "pubkey(Dean,\"abcdef\")."
  "pubkey(Joe,\"0123456789\")."
  "  ; Convenient abbreviations"
  "pubkey_fingerprint(?x) :- application says pubkey_fingerprint(?x)."
  "access(?a)             :- application says access_mode(?a)."
  )


   (Scenario "Local-MEMO"
     "Local access to MEMO"

     (group "init"
       ""
       ((tc "Start the Soutei engine and load the initial assertions"))
       (check "")
       )

     (group "local-MEMO-1"
       ""
       ((tc "Write the document into MEMO from within FNMOC")
	(tc "Read the document back")
	(tc "Write another document from a metnet host")
	(tc "Read both documents"))
       (check "All operations must succeed"))
     )


   (Scenario "Remote-MEMO"
     "Remote access to MEMO"

     (group "r-memo-init"
       ""
       ((tc "See Scenario Local-MEMO through the point "
	    (group-ref "local-MEMO-1")))
       (check "")
       )

     (group "MEMO-Basic-authentication-checks"
       ""
       ((tc "An attempt to access the MEMO channel in plain HTTP")
	(tc "Read the MEMO channel via HTTPS, but supplying no client
public-key certificate")
	(tc "Read the MEMO channel via HTTPS, supplying an invalid
client certificate (untrusted CA)")
	(tc "Read the MEMO channel via HTTPS, supplying an expired
certificate")
	(tc "Read the MEMO channel via HTTPS, using a valid certificate,
issued to the user Dean")
	 )
       (check "All attempts should be denied and logged."))

     (group "MEMO-remote-OK"
       ""
       ((tc "Write a document into the MEMO channel using HTTPS and a valid client public-key certificate issued to the user Joe")
	(tc "Read from the MEMO channel using HTTPS and the same
certificate"))
       (check "The channel should contain three documents. The remote
user should obtain these three documents."))
     )


   (Scenario "DEMO-IMG-simple"
     "Access to DEMO-IMG by its administrator. Simple authorizations."

     (group "init"
       ""
       ((tc "Start the Soutei engine and load the initial assertions"))
       (check "")
       )

     (group "init-tests-DEMO-IMG"
       ""
       ((tc "Try to write into DEMO-IMG from within FNMOC")
	(tc "Try to read from DEMO-IMG via HTTP")
	(tc "Try to read from DEMO-IMG via HTTPS without providing any client certificate")
	(tc "User Joe tries to read DEMO-IMG with his certificate")
	(tc "User Dean tries to read DEMO-IMG with his certificate")
	)
       (check "All attempts should be denied and logged."))

     (group "Dean-allows-himself"
       ("User Dean allows the full access to DEMO-IMG for himself."
	 (br)
	 "Dean issues an attribute certificate with the assertion " 
	 (code "may(channel,DEMO-IMG,?a) :- application says access_mode(?a).") ". Dean specifies himself as the holder of the certificate.")
       ((tc "Dean sends his attribute certificate and his public-key certificate over HTTPS (in the header?) and writes a sample image into DEMO-IMG")
	 (tc "The same but attempting to read the channel")
	 )
       (check "Channel should have the sample image.  Dean was able to
read it. Because the certificate has a non-empty holder name, the
assertion is not added to the assertion cache."))

     (group "DEMO-IMG-check-rejections"
       ""
       ((tc "User Joe tries to read DEMO-IMG over HTTPS, with authentication")
	(tc "Dean sends his attribute certificate and his public-key
certificate over HTTPS and attempts to read the channel MEMO")
	(tc "Joe tries to use Dean's attribute certificate from " 
	  (group-ref "Dean-allows-himself")
	  " to read DEMO-IMG. User Joe sends the certificate in an
HTTP header and or? TLS")
	(tc "User Dean tries to read DEMO-IMG without providing the
attribute certificate"))
       (check "All attempts should be denied and logged.")
       )

     (group "DEMO-IMG-simple-authorization"
       ("Dean directly authorizes a user to access the channel." (br)
	 "Dean issues an attribute certificate with the assertion " 
	 (code "may(channel,DEMO-IMG,?a) :- application says access_mode(?a).") " for the user Aaron.")
       ((tc "Aaron sends the attribute certificate and his public-key certificate over HTTPS (in the header?) and reads the sample image from DEMO-IMG")
	 )
       (check "Aaron should get the image. The certificate is not cached."))
     )


   (Scenario "DEMO-IMG-delegation-1"
     "Access delegation via assertions."

     (group "demo-img-init"
       ""
       ((tc "Run Scenario DEMO-IMG-simple through the point "
	    (group-ref "init-tests-DEMO-IMG")))
       (check "")
       )

     (group "demo-img-delegation-1"
       ("Dean allows Eric to read from any channel he can read."
	 (br)
	 "Dean issues an attribute certificate with the assertion "
; may(channel,?c,read) :- system says a_channel(?c), ...
	 (verbatim
	   "may(channel,DEMO-IMG,read) :- known_user(Eric)."
	   "known_user(Eric) :- 
               application says pubkey_fingerprint(\"dddddd\").")
	 " and the empty holder field and a validity period of one hour.")
       ((tc "Eric sends the attribute certificate and his public-key
certificate over HTTPS (in the header?) and reads a sample image from
DEMO-IMG")
	(tc "Eric attempts to read the channel again but without
providing the attribute certificate"))
       (check "Eric reads the image both times. Dean's assertion is
added to the assertion cache."))

     (group "demo-img-delegation-1-rej"
       ""
       ((tc "Joe attempts to read the channel DEMO-IMG.")
	(tc "Eric sends the attribute certificate and his public-key
certificate over HTTPS (in the header?) and attempts to write another
sample image into DEMO-IMG")
	(tc "Eric sends the attribute certificate and his public-key
certificate over HTTPS (in the header?) and attempts to read MEMO")
	(tc "Eric attempts to read DEMO-IMG after one hour has passed"))
       (check "All attempts should be denied and logged. Dean's
assertion should no longer be in the assertion cache."))

     (group "demo-img-delegation-1-rej-2"
       ""
       ((tc "Joe attempts to read the channel DEMO-IMG providing the
renewed Dean's certificate"))
       (check "All attempts should be denied and logged. Dean's assertion is added to the assertion cache."))
     )

   (Scenario "DEMO-IMG-delegation-2"
     "Chained delegation via assertions."

     (group "demo-img-init"
       ""
       ((tc "Run Scenario DEMO-IMG-simple through the point "
	    (group-ref "init-tests-DEMO-IMG")))
       (check "")
       )

     (group "demo-img-delegation-2"
       ("Dean allows Ryan to delegate reading from any channel."
	 (br)
	 "Dean issues an attribute certificate with the assertion "
	 (verbatim
	   "may(?c,?n,read) :- \"eeeeee\" says may(?c,?n,?).")
	 " and the empty holder field. Ryan issues an attribute
certificate to the user Greg:" (code "may(channel,DEMO-IMG,?a) :- application says access_mode(?a).") " Greg is the
holder of the latter certificate.")
       ((tc "Greg sends Dean's and Ryan's attribute certificates and
his public-key certificate over HTTPS (in the header?) and reads a
sample image from DEMO-IMG")
	(tc "Greg attempts to read the channel again but without
providing Dean's attribute certificate"))
       (check "Greg reads the image both times. Dean's assertion is
added to the assertion cache."))

     (group "demo-img-delegation-2-rej"
       ""
       ((tc "Joe attempts to read the channel DEMO-IMG")
	(tc "Greg sends both attribute certificates and his public-key
certificate over HTTPS (in the header?) and attempts to write another
sample image into DEMO-IMG")
	(tc "Greg sends both attribute certificates and his public-key
certificate over HTTPS (in the header?) and attempts to read from MEMO")
	 )
       (check "All attempts should be denied and logged. Dean's
assertion should be in the assertion cache."))
     )


; need use cases for the revocation
; need use cases for the typecheck failures
; 1. Can transitivity be defined safely? That is, would the following
; statement be allowed?

; 	larger(X,Z):- larger(X,Y), larger(Y,Z)

   (Scenario "Policy-Check"
     "Off-line policy check of Soutei assertions"

     (group "policy-check-init-assertion"
       "Checking the initial assertion"
       ((tc "Check that there is a Metcast channel and a (remote or
local) user that cannot access this channel")
	(tc "Check that there is a channel that is not writable by
everybody")
	(tc "Check that there is no channel that can be accessed by
every known user") )
       (check "")
       )

     (group "policy-check-init-assertion-var-mistake-1"
       ("Introduce a mistake into the initial assertion:" (br)
	 (code "may(channel,MEMO,?a) :- known_user(?Joe), access(?a).") (br)
	 "The spurious question mark before the user name turns the
constant into a variable.")
       ((tc "Run checks of " (group-ref "policy-check-init-assertion")))
       (check "Policy check should fail: every known user can read and write
the channel MEMO.")  )

     (group "policy-check-delegation"
       ("Check that the user Dean is in full control over DEMO-IMG.")
       ((tc "Check that no one can access DEMO-IMG unless Dean allows
him to. That is, the explicit denial of access by Dean disallows all
access to the channel.")
       )
       (check ""))

; that should be checked by a typechecker!
     (group "policy-check-delegation-var-mistake"
       ("Introduce a mistake into the initial assertion:" (br)
	 (code "may(channel,?MEMO,?a) :- known_user(Joe), access(?a)") (br)
	 "The spurious question mark before the name of the channel turns the constant into a variable.")
       ((tc "Run checks of " (group-ref "policy-check-delegation"))
       )
       (check "Policy check should fail: the user Joe is now allowed
full access to the channel DEMO-IMG, regardless of Dean's wishes.")
       )
     )


(hr)

(Section 2 "References")

 (bibitem "Binder" "Binder"
    "J. DeTreville. Binder, a logic-based security language. "
    "IEEE Security and Privacy, 2002. "
    (URL "http://research.microsoft.com/research/pubs/view.aspx?tr_id=545")
    )

  (bibitem "KeyNote" "KeyNote"
    (URL "http://www.crypto.com/trustmgt/kn.html"))

(bibitem "Transport" "Transport"
  (a (@ (href "Auth-transport.html")) "Transport of Soutei assertions")
)

;; (bibitem "Metcast-Channels" "Metcast-Channels" 
;;   (a (@ (href "Metcast-Channels.html")) "Metcast Channels"))

(bibitem "Metcast-Channels" "Metcast-Channels" 
  "Metcast Channels (see the Metnet site).")

(bibitem "KANREN" "KANREN"
  "KANREN: A declarative applicative logic programming system"
  (URL "http://kanren.sourceforge.net/")
)


(bibitem "Mercury-Modes" "Mercury-Modes"
  "The Mercury Language Reference Manual. Version 0.11.0. Modes. "
  (URL "http://www.cs.mu.oz.au/research/mercury/information/doc-release/reference_manual_4.html")
  )

(footer)

)))


;========================================================================
;			HTML generation

(define (->str x) (if (symbol? x) (symbol->string x) x))
 
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

     ; Typing things: meta-language

     (pc *macro*
       . ,(lambda (tag . args) `(p (code ,@args))))

     ; (forall (v domain) body)
     (forall *macro*
       . ,(lambda (tag arg body)
	    (let ((v (car arg)))
	      `("forall " ,v " in " ,(->str (cadr arg)) "." ,(->str body)))))

     ; Axiom
     (A *macro*
       . ,(lambda (tag body)
	    `("===> " ,body)))

     ; Rule
     (R *macro*
       . ,(lambda (tag precond concl)
	    (cons
	      (list-intersperse (if (pair? precond) precond (list precond))
		'(br))
	      `((br) "===> " ,concl))))

     ; Meta-functions
     ; (F Clauses pred/n)
     ; (F FV atom)
     ; (F GExt Gamma var ttype)
     (F *macro*
       . ,(lambda (tag name . args)
	    (list (symbol->string name) "(" 
	      (list-intersperse
		(map ->str args) ",") ")")))


     ; (P pred/n i)
     ; (P pred/n c i)
     (P *macro*
       . ,(lambda (tag pred . args)
	    (let*-values
	      (((c i)
		 (if (null? (cdr args)) (values #f (car args))
		   (apply values args))))
	      (list (->str pred) (and c `(sup ,c)) `(sub ,i)))))

     ; Judgements
     (Js1 *macro*
       . ,(lambda (tag clause)
	    `("|-" (sub s1) " " ,clause)))
     (Js2 *macro*
       . ,(lambda (tag clause)
	    `("|-" (sub s2) " " ,clause)))
     (Js3 *macro*
       . ,(lambda (tag clause)
	    `("|-" (sub s3) " " ,clause)))

     ; (Jpi pred/n i ptype)
     ; (Jpi (P pred/n c i) ptype)
     (Jpi *macro*
       . ,(lambda (tag pred . args)
	    (let*-values
	      (((pred type)
		 (if (null? (cdr args)) (values (->str pred) (car args))
		   (values `(P ,pred ,(car args)) (cadr args)))))
	      `("|-" (sub pi) " " ,pred ":" ,type))))

     (Jt *macro*
       . ,(lambda (tag gamma term ttype)
	    `(,(->str gamma) "|-" (sub t) " " ,term ":" ,ttype)))

     (Jb *macro*
       . ,(lambda (tag gamma atom*)
	    `(,(->str gamma) "|-" (sub ok) " " ,(->str atom*))))

     (trace *macro* . ,(lambda (tag . args) (apply cerr args) #f))

     (sub . ,(lambda (tag el) (list "<sub>" el "</sub>")))
     (sup . ,(lambda (tag el) (list "<sup>" el "</sup>")))

     )))

; Generating HTML


(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content common-rules-here)))

(generate-HTML Content)


; LocalWords:  Datalog KeyNote RTC SDSI SPKI PolicyMaker SAML Soutei's LDAP PS
; LocalWords:  Herbrand blockquote bibitem ebnf HTTPS SSL PKC TLS authInfo RS
; LocalWords:  SubjectDirectoryAttributes SvceAuthInfo GeneralName issuerName
; LocalWords:  directoryName SubjectKeyIdentifier OpenSSL issuer's IMG pubkey
; LocalWords:  DeTreville's ExportCertData SSLOptions Soutei Binder's pAtom RL
; LocalWords:  KANREN DeTreville disequality ptype ttype pred arity forall neq
