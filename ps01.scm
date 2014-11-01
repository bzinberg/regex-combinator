;;; Ben Zinberg
;;; Regex combinators
;;; (Assignment for 6.945 Advanced Symbolic Programming)

;;; Write-up.  (This file contains answers to the accompanying questions in
;;; ps.txt.  Most of the actual code is found in regexp.scm and its includes.)

(load "regexp.scm")

;;; PROBLEM 1
(define (r:+ expr)
  (r:repeat 1 #f expr))

(define (r:* expr)
  (r:repeat 0 #f expr))

#|
(pp (r:grep* (r:seq (r:quote "I ")
                    (r:* (r:quote "really "))
                    (r:quote "like Scheme"))
             "tests.txt"))

("[16]. I like Scheme"
 "[17]. I really like Scheme"
 "[18]. I really really really like Scheme"
 "[20]. I really really really really really like Scheme"
 "[22]. I really really really really really really really really really like Scheme")

(pp (r:grep* (r:seq (r:quote "I ")
                    (r:+ (r:quote "really "))
                    (r:quote "like Scheme"))
             "tests.txt"))

("[17]. I really like Scheme"
 "[18]. I really really really like Scheme"
 "[20]. I really really really really really like Scheme"
 "[22]. I really really really really really really really really really like Scheme")

(pp (r:grep* (r:seq (r:quote "I ")
                    (r:+ (r:alt (r:quote "really ")
                                (r:quote "truly ")))
                    (r:quote "like Scheme"))
             "tests.txt"))

("[17]. I really like Scheme"
 "[18]. I really really really like Scheme"
 "[20]. I really really really really really like Scheme"
 "[21]. I really truly really like Scheme"
 "[22]. I really really really really really really really really really like Scheme")
|#

;;; PROBLEM 2
;;; 
;;; a. The body of (r:repeat 0 1 expr) will itself call (r:repeat 0 1 expr),
;;; resulting in an infinite loop.
;;; 
;;; b. Bonnie's code will produce much shorter regular expressions, and they
;;; will be more human readable mostly for that reason.  We should also note
;;; that having extremely large regular expressions may cause efficiency
;;; problems, both for storage and for matching.  For Bonnie's suggestion, less
;;; new code will be required: we can just substitute (r:? expr) for (r:alt
;;; expr "") directly, whereas implementing Alyssa's suggestion would likely
;;; involve explicitly the dirty details of copying the base string several
;;; times, and tacking on expr to the end of these copies various numbers of
;;; times.  The code might need comments explaining this convoluted strategy.
;;; 
;;; c. Ben's code uses BRE syntax, whereas Alyssa's and Bonnie's use ERE syntax.
;;; (BRE is supposedly more basic and should be supported by more programs.
;;; This might become important if we try to unmarry ourselves from using grep
;;; for string matching.)  The regular expressions coming out of Ben's method
;;; are shorter than all the others.  A human can actually see what the meaning
;;; is immediately when reading an interval expression, rather than reading a
;;; complicated alternation construct and saying "Oh, this is their way of
;;; conveying intervals."  Also, the number of additional characters tacked on
;;; by a repeat is small, whereas it was on the order of (max-min) * length of
;;; expr for Bonnie's suggestion, and even bigger for Alyssa's suggestion.

(define (r:repeat min max expr)
  (string-append "\\("
                 expr
                 "\\{"
                 (number->string min)
                 ","
                 (if max
                   (number->string max)
                   "")
                 "\\}"
                 "\\)"))

#|
(pp (r:grep* (r:seq (r:quote "I ")
                    (r:repeat 2 5 (r:quote "really "))
                    (r:quote "like Scheme"))
             "tests.txt"))

("[18]. I really really really like Scheme"
 "[20]. I really really really really really like Scheme")

(pp (r:grep* (r:seq (r:quote "I ")
                    (r:repeat 2 #f (r:quote "really "))
                    (r:quote "like Scheme"))
             "tests.txt"))

("[18]. I really really really like Scheme"
 "[20]. I really really really really really like Scheme"
 "[22]. I really really really really really really really really really like Scheme")

(pp (r:grep* (r:seq (r:quote "I ")
                    (r:repeat 2 5 (r:alt (r:quote "really ")
                                         (r:quote "truly ")
                                         (r:quote "don't ")))
                    (r:quote "like Scheme"))
             "tests.txt"))

("[18]. I really really really like Scheme"
 "[19]. I really really don't really like Scheme"
 "[20]. I really really really really really like Scheme"
 "[21]. I really truly really like Scheme")
|#

;;; PROBLEM 3
;;; 
;;; My strategy is basically an object-oriented or data-directed one.
;;; Following the tag system used in chapter 2 of SICP, I created tagged data
;;; types for the various basic forms and means of combination found in regular
;;; expressions, and define a "render" method for each, as well as a
;;; "render-atomic" method which places parentheses around the object if such
;;; parentheses would be needed for grouping in the context of a larger
;;; expression.  There is then a generic "render" procedure (as well as
;;; "render-atomic") which dispatches to the appropriate method, and this
;;; generic procedure is used in some of the methods to render "child elements"
;;; as subexpressions of the parent (e.g., alternation relies on being able to
;;; generically render each of the subexpressions being alternated between).
;;;
;;; Now the task of eliminating unnecessary parentheses comes down to making
;;; sure the renderer knows whether parentheses are necessary.  This is the
;;; purpose of having both render and render-atomic: when grouping is
;;; necessary, render-atomic is called, and parentheses are only added if the
;;; grouping needs additional parentheses.  So for example, the atomic
;;; rendering of c\{2,3\} is (c\{2,3\}), but the atomic rendering of [ab] is
;;; simply [ab].  Subtle cases such as this one and many more can be seen in
;;; the output of regexptest.scm, which shows all generated regular expressions
;;; and matches for the test cases.  See for example the test case "Repeated
;;; repeat."

;;; PROBLEM 4
;;; 
;;; My strategy for implementing backreferences is to allow names (symbols) to
;;; be attached to the objects representing regular expressions.  When the
;;; regular expression is rendered, the result is a "labelled string", in which
;;; the first character of each parenthesized chunk is labelled with the name
;;; given to the object which produced the chunk.  (We ensure that chunks
;;; generated by named objects are parenthesized.)  If backreferences occur
;;; within the regular expression to be rendered, they are rendered as
;;; placeholders which record (in their labels, remember this is a labelled
;;; string) the name of the object to be backreferenced.
;;;
;;; After this initial rendering, we wish to resolve backreferences.  Doing so
;;; will not introduce any new parentheses, so we can now start counting
;;; parentheses.  We create a table which maps names to backreference numbers
;;; (i.e. the number which would be used in a backreference to the chunk havin
;;; that name).  Then we use this table to replace backreference placeholders
;;; with the appropriate backreference number and preceding backslash.
;;;
;;; This strategy requires the notion of a "labelled string," which is fleshed
;;; out in labelled-string.scm.  Note that in the above, the labels associated
;;; to placeholders are not symbols, and so they can be distinguished from the
;;; labels associated to named chunks.  Since each character in a labelled
;;; string can have at most one label, this implies that backreferences
;;; themselves cannot have names.  This is reasonable, since backreferences to
;;; backreferences can (and probably should) always be simplified away.
;;;
;;; I should note that the way in which I deal with labelled strings is sloppy
;;; and, if I planned to extend this software further, should be reworked to
;;; have have more careful type checking.  It is important that each render
;;; method return a labelled string (not just a string), but to keep the code
;;; short and readable, I did this by having the labelled-string-append and
;;; labelled-string->string procedures take in either strings or labelled
;;; strings as arguments, and always return labelled-strings.  So, it's
;;; important that each render method does something that eventually converts
;;; the result into a labelled string, but this is not explicitly stated in the
;;; code and the place at which conversion happens is not always obvious if you
;;; don't read carefully.  It's probably okay for programs of this length, but
;;; if the program got bigger I'd definitely want to fix this up a bit.

;;; PROBLEM 5
;;; 
;;; ISSUES
;;;
;;; The main weirdness is that a lot of the special characters, which in BREs
;;; are treated literally when unescaped and are treated specially when
;;; escaped, are treated precisely the reverse way in EREs.  So for example \(
;;; \) are grouping symbols in BREs whereas they're literal parentheses in
;;; EREs, and () are grouping symbols in ERES whereas they're literal
;;; parentheses in BREs.  Of course, this only holds outside of bracketed
;;; expressions [].  And by the way, [] are considered special without escaping
;;; in both BREs and EREs.  To test the new (and old) backends, there should be
;;; some test cases with lots of literal parentheses, braces and brackets, as
;;; well as grouping, backreferences, repetition, and char-froms that result in
;;; the rendering of syntactic prantheses, braces and brackts.
;;; 
;;; Also, there are a bunch of new additions to the lineup of characters that
;;; must be escaped in EREs, such as ?, +, |.  We should include test cases
;;; that have literal versions of these characters in addition to the syntactic
;;; constructs requiring them.  (Actually, that will only be possible for |.
;;; Since we are building a language on top of REs, we do not have to use the
;;; syntactic sugar ?, +, *; we can use interval expressions instead.)
;;;
;;; It's worth noting that the implementation of backreferences has to change
;;; in order to accommodate an ERE backend, since the same expression may have
;;; different backreference numbers as a BRE vs. as an ERE.  This is likely to
;;; cause problems for an incorrect implementation if there are backreferences
;;; in an RE that has lots of grouping and literal parentheses.  I will attempt
;;; to increase the level of abstraction so that the code differences between
;;; BREs and EREs are small and transparent, without having so much abstraction
;;; that it becomes hard to read and understand the code.
;;; 
;;; SOLUTIONS
;;;
;;; In this problem I created my third draft of the code.  In this draft, a new
;;; boolean constant USE_ERE is defined, and the constants
;;; chars-needing-quoting and chars-needing-quoting-in-brackets are re-branded
;;; to procedures which check the value of USE_ERE and return the appropriate
;;; constant.  Such a procedure is also created for pipe |, as well as open and
;;; close parentheses and braces.  The procedure surround-with-braces and the
;;; new version of parenthesize use these refactored constant-giving
;;; procedures, and would work even if the future values of these constants
;;; changed.
;;; 
;;; The procedure make-backreference-table needed surprisingly few changes to
;;; work for EREs.  I mostly just had to change has-initial-open-paren? to work
;;; with the new constant-giving procedures.  However the new version of
;;; make-backreference-table is not completely general: The notion of
;;; backslash, and the fact that it escapes exactly one character, is
;;; hard-coded in.  Thus, only two front two characters of the labelled string
;;; in question are remembered at a given time, since only one character is to
;;; be skipped.  I decided not to go more general because this meaning of
;;; backslash holds for all REs and there is never a time when more than two
;;; characters would together be treated as a single token for the purposes of
;;; an RE.  Generalizing would just make the code longer and harder to read.

