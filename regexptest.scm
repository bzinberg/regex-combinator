;;; Ben Zinberg
;;; Regex combinators
;;; (Assignment for 6.945 Advanced Symbolic Programming)
;;; Test suite

(load "regexp.scm")

(define (list-add . lists)
  (apply map + lists))

(define (display-with-newline string)
  (display string)
  (newline))

(define (display-indented string)
  (display "  ")
  (display-with-newline string))

(define (make-test-case name object)
  (list name object))

(define test-case-name car)
(define test-case-object cadr)

(define test-cases '())

(define (add-test-case name object)
  (let ((test-case (make-test-case name object)))
    (set! test-cases (append test-cases
                             (list test-case)))))

(define (run-tests)
  ; Returns (1 0) on success, (0 1) on failure
  (define (run-test test-case)
    (let* ((name (test-case-name test-case))
           (object (test-case-object test-case))
           (bre (fluid-let ((USE_ERE #f))
                           (r:compile object)))
           (ere (fluid-let ((USE_ERE #t))
                           (r:compile object)))
           (grep-matches (r:grep* bre "tests.txt"))
           (egrep-matches (r:egrep* ere "tests.txt")))
      (display "Test case: ")
      (display name)
      (newline)
      (display-with-newline "Compiled BRE:")
      (display-indented bre)
      (display-with-newline "Compiled ERE:")
      (display-indented ere)
      (if (equal? grep-matches egrep-matches)
        (begin
          (display-with-newline "grep and egrep results match:")
          (for-each display-indented grep-matches)
          (newline)
          '(1 0))
        (begin
          (display-with-newline "grep and egrep results don't match.")
          (display-with-newline "grep results:")
          (for-each display-indented grep-matches)
          (display-with-newline "egrep results:")
          (for-each display-indented egrep-matches)
          (newline)
          '(0 1)))))

  (let* ((results
          (apply list-add
                 (map run-test test-cases)))
        (num-successes (car results))
        (num-failures (cadr results)))
    (display (string-append "Testing complete: "
                            (number->string num-successes)
                            " passed, "
                            (number->string num-failures)
                            " failed (as far as I can tell)"))
    (newline)))

(add-test-case "Literal brackets and parentheses"
               (r:quote "()--[]"))

(add-test-case "Alternating literal brackets and parentheses with a backreference"
               (r:seq
                 (r:attach-name 'left
                                (r:alt (r:quote "()")
                                       (r:quote "[]")))
                 (r:quote "--")
                 (r:backref 'left)))

(add-test-case "Alternation with a pipe"
               (r:repeat 5
                         5
                         (r:alt
                           (r:quote "|")
                           (r:quote "e"))))

(add-test-case "Repeated literal left brace"
               (r:repeat 1
                         2
                         (r:quote "{")))

(add-test-case "Repeated literal right brace"
               (r:repeat 1
                         2
                         (r:quote "}")))

(add-test-case "Repeated repeat"
               (r:repeat 3
                         #f
                         (r:repeat 2
                                   3
                                   (r:quote "c"))))

(add-test-case "Repeated literal expression that itself looks like a repeat designation, with a backreference"
               (r:seq
                 (r:attach-name 'fake-repeat
                                (r:repeat 1
                                          2
                                          (r:quote "{1,2}")))
                 (r:quote " and ")
                 (r:backref 'fake-repeat)))

(add-test-case "Possibly mismatched literal parentheses, brackets and braces, because hey, why not"
               (r:seq
               (r:repeat 3
                         3
                         (r:alt (r:quote "(")
                                (r:quote "[")
                                (r:quote "{")))
               (r:repeat 3
                         3
                         (r:alt (r:quote ")")
                                (r:quote "]")
                                (r:quote "}")))))

(add-test-case "char-from \"a]\""
               (r:seq (r:quote "CHARFROM ")
                      (r:char-from "a]")))

(add-test-case "char-not-from \"^\""
               (r:seq (r:quote "CHARFROM ")
                      (r:char-not-from "^")))

(add-test-case "char-from \"^-\" (the weird exception case)"
               (r:seq (r:quote "CHARFROM ")
                      (r:char-from "^-")))

(add-test-case "char-from \"a-z^\" (note that 'b' should not be matched here)"
               (r:seq (r:quote "CHARFROM ")
                      (r:char-from "a-z^")))


(add-test-case "BOL and EOL with literal ^ and $"
               (r:seq
                 (r:bol)
                 (r:quote "^")
                 (r:quote "$")
                 (r:eol)))

(add-test-case "Empty line"
               (r:seq
                 (r:bol)
                 (r:eol)))

(add-test-case "Lines beginning with $"
               (r:seq
                 (r:bol)
                 (r:quote "$")))

(add-test-case "Lines ending with ^"
               (r:seq
                 (r:quote "^")
                 (r:eol)))

(add-test-case "Backreference with nested parentheses, both syntactic and literal"
               (r:seq
                 (r:attach-name 'first-sentence
                                (r:seq
                                  (r:quote "I'm (I am) going to ")
                                  (r:alt (r:quote "run to")
                                         (r:quote "urinate in"))
                                  (r:quote " the ")
                                  (r:attach-name 'room-type
                                                 (r:repeat 1
                                                           #f
                                                           (r:char-not-from " ")))
                                  (r:quote "room.")))
                 (r:quote "  I need to use the ")
                 (r:backref 'room-type)
                 (r:quote ".  ")
                 (r:backref 'first-sentence)))

(add-test-case "Combining a bunch of small terms with alternation and concatenation, backreferences, and unused names"
               (r:seq
                 (r:attach-name
                   'chunk1
                   (r:alt
                     (r:repeat 1
                               2
                               (r:attach-name 'chunk2
                                              (r:quote "{1,2}(((")))
                     (r:seq (r:char-from "a]")
                            (r:char-not-from "^"))))
                 (r:attach-name 'laughter
                                (r:quote "haha"))
                 (r:quote "HAHA")
                 (r:backref 'laughter)
                 (r:backref 'chunk1)))

(run-tests)
