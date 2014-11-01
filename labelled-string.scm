;;; Ben Zinberg
;;; Regex combinators
;;; (Assignment for 6.945 Advanced Symbolic Programming)

;;; LABELLED STRINGS
;;; 
;;; A "labelled string" is a sequence of characters, each of which is allowed
;;; to have a "label" (an arbitrary object).  In this way, it is more properly
;;; the characters of our string that are labelled.  However, we stipulate that
;;; the label of a string is simply the label of its first character, if that
;;; exists.
;;; 
;;; Note that in some cases a "labelled string" may not itself have a label.
;;; Note also that characters other than the first character of a labelled
;;; string can be labelled.  In this case we think of our labelled string as
;;; having labelled substrings.  For example, the following diagram represents
;;; a labelled string with no label, which contains the labelled substring
;;; "ELL" which has label 'a.
;;; 
;;; H E L L O
;;;   ^ ^
;;;   a b
;;;

;;; Here s is either a labelled string (in which case no conversion happens) or
;;; a string.  This is an assumption; we do not check for it or make any
;;; promises about the return value for invalid inputs.
(define (make-labelled-string s)
  (cond
    ((string? s) (string->list s))
    ((pair? s) s)
    (else
      (error "Cannot convert object to labelled string: " s))))

;;; Returns a representation of labelled-string as a list of elements, each of
;;; which is either a character or a 2-element list whose car is a character
;;; and whose cadr is the label of that character.
(define (labelled-string->list-of-chars-and-lists labelled-string)
  ; As of right now, this coincides with our internal representation
  labelled-string)

;;; item is either a string or a labelled string.  A labelled string is
;;; returned.
(define (ls:attach-label label item)
  (let* ((item (make-labelled-string item)))
   (cond ((null? item)
          (error "Tried to attach a label to the empty string"))
         ((pair? item)
          (let ((first (car item))
                (rest (cdr item)))
            ; Since we are adding a label, there should not already be a label
            ; there
            (if (char? first)
              (cons (list first label)
                    rest)
              (error
                "Tried to label a character that was already labelled"))))
         (else
           (error
             "Tried to attach a label to data of an unsupported type: "
             item)))))

;;; Takes in either strings or labelled strings as arguments
(define (labelled-string-append . items)
  (apply append (map make-labelled-string items)))

;;; Drops the label data and returns a string.
(define (labelled-string->string labelled-string)
  (if (string? labelled-string)
    labelled-string
    (list->string
      (map (lambda (c)
             (if (char? c)
               c
               (car c)))
           labelled-string))))

;;; Lists all labels occurring in the given labelled string.
(define (ls:label-list labelled-string)
  (apply append
         (map (lambda (c)
                (if (pair? c)
                  (list (cadr c))
                  '()))
              labelled-string)))

