; Evaluation of this file yields an HTML document
; $Id: soutei-sourceforge.scm,v 4.57 2009/03/11 04:12:37 oleg Exp $

(define Content
'(html:begin
  (Header
   (title "Soutei: a logic-based trust-management system")
   (description "A distributed trust-management system")
   (Date-Revision-yyyymmdd "20100319")
   (Date-Creation-yyyymmdd "20100319")
   (keywords "Logic Programming, Keynote, Binder, authorization, access control, Haskell")
   (AuthorAddress "oleg-at-okmij.org")
   (long-title "Soutei: a logic-based trust-management system")
   )

  (body
   (navbar
     ("Home" "http://soutei.sourceforge.net")
     ("Docs" "#Documentation")
;     ("Sample" "#Sample")
     ("Availability" "#Availability")
     ("SVN" "http://soutei.svn.sourceforge.net/viewvc/soutei")
     ("Summary" "http://sourceforge.net/projects/soutei/")
     ;("Discussion" "http://sourceforge.net/mail/?group_id=99654")
     ;("News" "http://sourceforge.net/news/?group_id=99654")
     ;("Related" "#SOUTEI-related")
     )



   (page-title)

   (a (@ (href "http://sourceforge.net/projects/soutei")) " "
      (img (@ (src 
	    "http://sflogo.sourceforge.net/sflogo.php?group_id=308653&type=3")
	      (width "80") (height "15") (border "0")
	      (alt "SourceForge.net"))))

   (p (em "SOUTEI") " is a trust-management system, a dialect of
Binder, for access control in distributed systems. Soutei policies and
credentials are written in a declarative logic-based security language
and thus constitute distributed logic programs. Soutei policies are
modular, concise, and readable. They support policy verification, and,
despite the simplicity of the language, express role- and
attribute-based access control lists, and conditional delegation.")
   (p
     (em "SOUTEI") " is designed to be an authorization decision
system: it is meant to give " (em "advice") " whether a particular
action should be permitted.  A policy-enforcement point such as a web
server, after receiving a request to fetch a web page, consults
Soutei. Soutei receives the details of the request and replies
with the `yes' or `no' answer, possibly qualified with attributes.  It is
up to the policy-enforcement point to follow the given advice.  To
reach a decision, Soutei consults the attributes of the request (such
as the users and resources involved), information about the world, and
policies.")
   (p
     (em "SOUTEI") " can be either embedded into applications or run
as a stand-alone network service. In the latter case, Soutei can work with
applications written in any language or running on any operating system.
Originally, Soutei is a Haskell library. Soutei server is an application
built on the top of the library.")

    (ul
;      (li (local-ref "Sample"))
;      (li (local-ref "mini"))
      (li (local-ref "Documentation"))
      (li (local-ref "Availability")
	(ul (li (local-ref "SVN"))
	    (li (local-ref "Distributions"))
	  ))
;       (li (local-ref "Related")
;       (li 
; 	(a (@ (href "http://lists.sourceforge.net/lists/listinfo/ssax-sxml"))
; 	  "SOUTEI Mailing list"))
      (li (a (@ (href "http://sourceforge.net/projects/soutei"))
	    "SOUTEI project summary page at SourceForge"))
      )

; Add the news section

;;    (Section 3 "Sample" " applications")

;;    (dl
;;      (dt (cvs-ref "mini/type-inference.scm"))
;;      (dd
;;        "Hindley-Milner type inference " (em "relation") ", which
;; relates a term in a lambda-calculus with fixpoint, polymorphic let,
;; sums and products -- and its type. The relation can be used for type
;; inference (determining the type for a term), type checking (making
;; sure that a term is of the given type), and term 
;; reconstruction (constructing a term that has the desired type).  We may
;; also specify a part of the term and a part of the type, and ask the
;; system to fill in
;; the rest. In the latter applications, this code acts as a theorem
;; prover in intuitionistic logic.")

;;      (dt (cvs-ref "benchmarks/"))
;;      (dd "Standard Prolog benchmarks: nrev, query, qsort, queens, etc. --
;; re-written for SOUTEI.")
;;      )

   (Section 3 "Documentation" " and design notes")

  (p (em "Soutei, a logic-based trust-management system (system description)") 
    (br)
    "Andrew Pimlott and Oleg Kiselyov. Proceedings of FLOPS 2006, "
    "8th International Symposium on Functional and Logic Programming. "
    "Fuji-Susono, Japan, April 24-26, 2006." (br)
    "The paper is published in Springer's Lecture Notes in "
    "Computer Science 3945, pp. 130-145, 2006." (br)
    (URL "http://dx.doi.org/10.1007/11737414") (br)
    (URL "http://okmij.org/ftp/papers/Soutei.pdf"))

 (p
   (a (@ (href "doc/"))
     "Specification, use cases and design notes"))

   (Section 3 "Availability")
   (p "The current version of SOUTEI is 2.1. SOUTEI is OpenSource,
distributed under the GPL 2 license.")
   (p
     "SOUTEI has been tested on the following Haskell systems:"
     (br)
     "GHC 6.10.4, The Haskell Platform, on Linux and FreeBSD. It is known
     to work on Microsoft Windows.")

   (Section 3 "Distributions")
   (p "SOUTEI download site at SourceForge:"
      (URL "http://sourceforge.net/projects/soutei/files"))

  (Section 3 "SVN" " Tree")
  (p (a (@ (href "http://soutei.svn.sourceforge.net/viewvc/soutei"))
	"The SVN Tree"))

   (footer)

)))

;(pp Content)

;========================================================================
;			HTML generation

; IMPORT
; SXML-to-HTML-ext.scm and all of its imports


; Generating HTML

(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content
   (generic-web-rules Content 
     `((who *preorder*
	 . ,(lambda (tag . elems)
	      (pre-post-order `((br) . ,elems) universal-conversion-rules)))

		; A reference to an anchor in the present file
		; (local-ref target . title)
		; If title is given, generate a regular
		;	<a href="#target">title</a>
		; Otherwise, transform the content so that a
		; construct that may generate an anchor 'target' (such
		; as Section) is re-written to the
		; title SXML. All other constructs re-write to
		; nothing.
     (local-ref
      *preorder*
      . ,(lambda (tag target . title)
	   (let
	       ((title
		 (if (pair? title) title	; it is given explicitly
		     (pre-post-order Content
		       `((*text* . ,(lambda (trigger str) '()))
			 (*default*
			  . ,(lambda (tag . elems)
			       (let ((first-sign (signif-tail elems)))
				 (if first-sign
				     (let ((second-sign
					    (signif-tail (cdr first-sign))))
				       (assert (not second-sign))
				       (car first-sign))
				     '()))))
			 (Section
			  *preorder*
			  . ,(lambda (tag level key . elems)
			       (if (equal? key target)
				   (list key elems)
				   '()))))))))
	     (assert (pair? title) report: target)
	     (cerr "title: " title nl)
	     (post-order 
	      `(a (@ (href #\# ,target)) ,title)
	      universal-conversion-rules))))

       ; cvs-ref SOUTEI-relative path
       (cvs-ref
	 *macro*
	 . ,(lambda (tag path)
	      `(a (@ (href 
		       "http://soutei.cvs.sourceforge.net/soutei/soutei/"
		       ,path))
		 (code ,path))))

       ; (navbar (title url) ...)
       (navbar
	*preorder*
	. ,(lambda (tag . elems)
	     (post-order
	       `(p (hr (@ (size 1) (noshade)))
		  (div (@ (align "center"))
		    ,(map
		     (lambda (title-url)
		       `((a (@ (href ,(cadr title-url))) ,(car title-url))
			 (n_) "|" (n_)))
		     elems))
		  (hr (@ (size 1) (noshade))) (br))
	       universal-conversion-rules)))

       )))))

(generate-HTML Content)

; LocalWords:  href cvs dd typecheck dt OpenSource Chez
