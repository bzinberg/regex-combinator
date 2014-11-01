;;; Ben Zinberg
;;; Regex combinators
;;; (Assignment for 6.945 Advanced Symbolic Programming)

(load "labelled-string.scm")

;; Use EREs rather than BREs?  (For definitions of BRE and ERE, see official
;; POSIX documentation for regular expressions)
(define USE_ERE #f)

(define (r:const:open-paren)
  (if USE_ERE
    "("
    "\\("))
(define (r:const:close-paren)
  (if USE_ERE
    ")"
    "\\)"))

(define (parenthesize string)
  (let ((open (r:const:open-paren))
        (close (r:const:close-paren)))
    (labelled-string-append open
                            string
                            close)))

(define (has-initial-open-paren? labelled-string)
  (let* ((string (labelled-string->string labelled-string))
         (open (labelled-string->string (r:const:open-paren)))
         (open-length (string-length open)))
    (and (>= (string-length string)
             open-length)
         (equal? (string-head string open-length)
                 open))))

(define (interpose x L)
  (if (< (length L) 2)
    L
    (append (list (car L) x)
            (interpose x (cdr L)))))
; (interpose "and" '(1 2 3 4))    -> (1 "and" 2 "and" 3 "and" 4)
; (interpose "and" '())           -> ()
; (interpose "and" '(1))          -> (1)

(define (r:attach-tag tag data)
  (list tag data))

(define (r:read-tag object)
  (car object))

(define (r:read-data object)
  (cadr object))

(define (r:attach-name name already-tagged-object)
  (if (not (symbol? name))
    (error "Names for r: objects must be symbols"))
  (if (eq? (r:read-tag already-tagged-object)
           'r:backref)
    (error
      "Cannot attach a name to a backreference.  Besides, there is no reason to do this."))
  (case (length already-tagged-object)
    ((3)
     (error "Tried to attach a name to an already named object"))
    ((2)
     (append already-tagged-object
             (list name)))
    (else
      (error "Tried to attach a name to an invalid object"))))

(define (r:has-name? object)
  (= (length object) 3))

(define (r:get-name object)
  (list-ref object 2))

(define (r:dot)
  (r:attach-tag 'r:dot '()))
(define (r:bol)
  (r:attach-tag 'r:bol '()))
(define (r:eol)
  (r:attach-tag 'r:eol '()))

(define (r:dot:render data)
  (make-labelled-string "."))
(define (r:dot:render-atomic data)
  (r:dot:render data))
(define (r:bol:render data)
  (make-labelled-string "^"))
(define (r:bol:render-atomic data)
  (r:bol:render data))
(define (r:eol:render data)
  (make-labelled-string "$"))
(define (r:eol:render-atomic data)
  (r:eol:render data))

(define (r:char char)
  (r:attach-tag 'r:char char))

(define (r:quote string)
  (case (string-length string)
    ((0) (error "Tried to quote an empty string"))
    ((1) (r:char (car (string->list string))))
    (else
      (apply r:seq
             (map r:char (string->list string))))))

(define (r:char:render data)
  (let ((char data)
        (chars-needing-quoting (r:const:chars-needing-quoting)))
    (if (memv char chars-needing-quoting)
      (labelled-string-append "\\" (char->string char))
      (make-labelled-string (char->string char)))))

(define (r:char:render-atomic data)
  (r:char:render data))

(define (r:const:chars-needing-quoting)
  (if USE_ERE
    '(#\. #\[ #\\ #\^ #\$ #\* #\? #\+ #\( #\) #\{ #\} #\|)
    '(#\. #\[ #\\ #\^ #\$ #\*)))

(define (r:char-from string)
  (case (string-length string)
    ((0) (error "Tried to build a regex that matches nothing (char-from the empty set)"))
    ((1) (r:quote string))
    (else
      (let ((chars (r:reorder-chars-for-quoting-in-brackets (string->list string))))
        (r:attach-tag 'r:char-from chars)))))

(define (r:char-not-from string)
  (if (equal? string "")
    ; Which characters do not belong to the empty set?  All of them.
    (r:dot)
    (let ((chars (r:reorder-chars-for-quoting-in-brackets (string->list string))))
      (r:attach-tag 'r:char-not-from chars))))

(define (r:reorder-chars-for-quoting-in-brackets chars)
  (if (lset= eqv? chars '(#\- #\^))
    '(#\- #\^)
    (let* ((chars-needing-quoting-in-brackets (r:const:chars-needing-quoting-in-brackets))
           (optional
             (lambda (c)
               (if (memv c chars)
                 (list c)
                 '())))
           (easy-chars
             (remove (lambda (c)
                       (memv c chars-needing-quoting-in-brackets))
                     chars)))
      (append (optional #\])
              easy-chars
              (optional #\^)
              (optional #\-)))))

(define (r:char-from:render data)
  ; data should be a list of length at least 2
  (let ((chars data))
   (labelled-string-append "["
                  (list->string chars)
                  "]")))

(define (r:char-from:render-atomic data)
  (r:char-from:render data))

(define (r:char-not-from:render data)
  ; data should be a list of length at least 1
  (let ((chars data))
   (labelled-string-append "[^"
                  (list->string data)
                  "]")))

(define (r:char-not-from:render-atomic data)
  (r:char-not-from:render data))

(define (r:const:chars-needing-quoting-in-brackets)
  '(#\] #\^ #\-))

;;; Means of combination for patterns
(define (r:seq . objects)
  (r:attach-tag 'r:seq objects))

(define (r:seq:render data)
  (apply labelled-string-append
         (map r:render data)))

(define (r:seq:render-atomic data)
  (parenthesize (r:seq:render data)))

(define (r:repeat min max object)
  (r:attach-tag 'r:repeat
                (list min max object)))

(define (r:repeat:get-min data) (car data))
(define (r:repeat:get-max data) (cadr data))
(define (r:repeat:get-object data) (caddr data))

(define (r:const:open-brace)
  (if USE_ERE
    "{"
    "\\{"))

(define (r:const:close-brace)
  (if USE_ERE
    "}"
    "\\}"))

(define (r:surround-with-braces string)
  (let ((open (r:const:open-brace))
        (close (r:const:close-brace)))
    (labelled-string-append open
                            string
                            close)))

(define (r:repeat:render data)
  (let ((min (r:repeat:get-min data))
        (max (r:repeat:get-max data))
        (object (r:repeat:get-object data)))
    (labelled-string-append
      (r:render-atomic object)
      (r:surround-with-braces
        (labelled-string-append
          (number->string min)
          ","
          (if max
            (number->string max)
            ""))))))

(define (r:repeat:render-atomic data)
  (parenthesize (r:repeat:render data)))

(define (r:backref ref-name)
  (if (not (symbol? ref-name))
    (error "The name referenced by a backreference must be a symbol"))
  (r:attach-tag 'r:backref ref-name))

(define (r:backref:render data)
  (let ((ref-name data))
   (ls:attach-label (list 'r:backref ref-name)
                    ; Here "B" is an arbitrary character; it will be replaced
                    ; when backreferences are resolved.
                    (make-labelled-string "B"))))

(define (r:backref:render-atomic data)
  (r:backref:render data))

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.
(define (r:alt . objects)
  (case (length objects)
    ((0) (error "Tried to create an alternation of zero objects"))
    ((1) (car objects))
    (else
      (r:attach-tag 'r:alt objects))))

(define (r:const:pipe)
  (if USE_ERE
    "|"
    "\\|"))

(define (r:alt:render data)
  (let ((objects data))
   (parenthesize (apply labelled-string-append
                        (interpose (r:const:pipe)
                                   ; Note that we use r:render, not r:render-atomic,
                                   ; because we assume alternation has the lowest
                                   ; precedence of all operations.  (Of course,
                                   ; alternation is not officially part of BREs, but we
                                   ; infer from the fact that alternation has lowest
                                   ; precedence in EREs.  Of course, in EREs the
                                   ; character for alternation is | rather than \|...)
                                   (map r:render objects))))))

(define (r:alt:render-atomic data)
  (r:alt:render data))

;;; Generic rendering procedure.  Dispatches on type using type tags.
(define (r:render object)
  (let ((tag (r:read-tag object))
        (data (r:read-data object)))
    (let ((unlabelled-version
            (case tag
              ('r:dot (r:dot:render data))
              ('r:bol (r:bol:render data))
              ('r:eol (r:eol:render data))
              ('r:char (r:char:render data))
              ('r:char-from (r:char-from:render data))
              ('r:char-not-from (r:char-not-from:render data))
              ('r:seq (r:seq:render data))
              ('r:alt (r:alt:render data))
              ('r:repeat (r:repeat:render data))
              ('r:backref (r:backref:render data))
              (else (error "Tried to render unknown type of object" tag)))))
      (if (r:has-name? object)
        ; The strings generated by objects with names must be parenthesized and
        ; labelled accordingly
        (let ((name (r:get-name object)))
         ; The name of the object becomes the label associated with its labelled
         ; string representation
         (if (has-initial-open-paren? unlabelled-version)
           (ls:attach-label name unlabelled-version)
           (ls:attach-label name (parenthesize unlabelled-version))))
         ; If object has no name, labelled string has no label (at least not on
         ; the first character).
         unlabelled-version))))
        

(define (r:render-atomic object)
  (let ((tag (r:read-tag object))
        (data (r:read-data object)))
    (let ((unlabelled-version
            (case tag
              ('r:dot (r:dot:render-atomic data))
              ('r:bol (r:bol:render-atomic data))
              ('r:eol (r:eol:render-atomic data))
              ('r:char (r:char:render-atomic data))
              ('r:char-from (r:char-from:render-atomic data))
              ('r:char-not-from (r:char-not-from:render-atomic data))
              ('r:seq (r:seq:render-atomic data))
              ('r:alt (r:alt:render-atomic data))
              ('r:repeat (r:repeat:render-atomic data))
              ('r:backref (r:backref:render-atomic data))
              (else (error "Tried to render unknown type of object" tag)))))
      (if (r:has-name? object)
        ; The strings generated by objects with names must be parenthesized and
        ; labelled accordingly
        (let ((name (r:get-name object)))
         ; The name of the object becomes the label associated with its labelled
         ; string representation
         (if (has-initial-open-paren? unlabelled-version)
           (ls:attach-label name unlabelled-version)
           (ls:attach-label name (parenthesize unlabelled-version))))
         ; If object has no name, labelled string has no label (at least not on
         ; the first character).
         unlabelled-version))))

;;; Resolving backreferences

;;; Given a labelled string, creates a hash table whose keys are the names
;;; occuring in the string (i.e., the labels that are symbols) and whose values
;;; are the numbers which should be used in a backreference to that object (as
;;; discovered by counting parentheses).
(define (r:make-backreference-table labelled-string)
  (let ((labelled-chars (labelled-string->list-of-chars-and-lists labelled-string)))
   (define table (make-strong-eq-hash-table))
   (define (traverse the-list paren-count)
     (if (or (null? the-list)
             (null? (cdr the-list)))
       ; This is the base case.  We don't care about return value.
       #!unspecific
       (let* ((c (car the-list))
              (char (if (pair? c)
                      (car c)
                      c))
              (label (if (pair? c)
                       (cadr c)
                       '()))
              (next-char (if (pair? (cadr the-list))
                           (caadr the-list)
                           (cadr the-list))))
         (let ((tail (if (eq? char #\\)
                       (cddr the-list)
                       (cdr the-list)))
               (updated-paren-count
                 (if (has-initial-open-paren?
                       (list->string (list char next-char)))
                   (begin
                     (if (not (null? label))
                       (hash-table/put! table label (+ paren-count 1)))
                     (+ paren-count 1))
                   paren-count)))

           (traverse tail updated-paren-count)))))

   (traverse labelled-chars 0)
   ; Return the hash table we have built up
   table))

;;; Given a hash table whose keys are symbols and whose values are integers
;;; between 1 and 9, and given a labelled string, makes the appropriate
;;; backreference substitutions.  Namely, each character whose label is a pair,
;;; is replaced by a character sequence indicating a backreference (such as
;;; \1), with the reference number gotten from the hash table.  This procedure
;;; was made for the use case in which labelled-string is the return value of
;;; r:render and table is the return value of r:make-backreference-table.
(define (r:apply-backreference-table table labelled-string)
  ; Here c is either a character or a (character, label) pair
  (define (replace-as-appropriate c)
    (cond ((char? c)
           (char->string c))
          ((list? c)
           (let ((char (car c))
                 (label (cadr c)))
             (if (pair? label)
               ; label should be a two element list whose car is 'r:backref and
               ; whose cadr is the name associated with the backreferred object.
               ; This name should be one of the keys in our backreference table.
               (let* ((key (cadr label))
                      (value (hash-table/get table key #f)))
                 (if value
                   (if (> value 9)
                     (error "Tried to create a back reference whose numerical index was more than 9")
                     (make-labelled-string (string-append "\\" (number->string value))))
                   (error "Invalid label supplied for backreference: " key)))
               ; If the label is not a list, then we are just looking at an
               ; ordinary character (which happens to have a label), rather than
               ; a backreference which must be resolved.
               (char->string char))))))

  (apply labelled-string-append
         (map replace-as-appropriate
              (labelled-string->list-of-chars-and-lists labelled-string))))

;;; Renders the given object and resolves all backreferences.
(define (r:compile object)
  (let* ((rend (r:render object))
         (table (r:make-backreference-table rend))
         (resolved (r:apply-backreference-table table rend)))
    (labelled-string->string resolved)))


;;; Code for interfacing with grep.  This code was provided in the problem set.

;;; Using system's grep.
(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

;;; Works for any string without newlines.
(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))


;;; This is MIT/Scheme specific and compatible with grep for the
;;; purposes of this code.

(load-option 'synchronous-subprocess)

(define (r:grep expr filename)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-grep-command-string expr filename)
             'output port)
            0)
	 (r:split-lines (get-output-string port)))))

(define (r:grep* expr filename)
  (let ((port (open-output-string)))
    (run-synchronous-subprocess "grep"
                                (list expr filename)
                                'output
                                port)
    (r:split-lines (get-output-string port))))

(define (r:egrep* expr filename)
  (let ((port (open-output-string)))
    (run-synchronous-subprocess "egrep"
                                (list expr filename)
                                'output
                                port)
    (r:split-lines (get-output-string port))))


(define (r:split-lines string)
  (reverse
   (let ((end (string-length string)))
     (let loop ((i 0) (lines '()))
       (if (< i end)
	   (let ((j
		  (substring-find-next-char string i end #\newline)))
	     (if j
		 (loop (+ j 1)
		       (cons (substring string i j) lines))
		 (cons (substring string i end) lines)))
	   lines)))))

