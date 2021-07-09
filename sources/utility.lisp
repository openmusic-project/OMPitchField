(in-package :om)


;;;;; ======================================================================
;;;;; OMTP 2.0
;;;;;
;;;;; UTILITY

;;;;; In this file:
; mc->p
; p->mc
; p->pc
; parse-incl-classreps
; parse-prog-classreps
; flatten2chordlist

;============================================================================
; mod12 arithmetic

(defun 12+ (num &rest more-nums)
  (mod (apply #'+ num more-nums) 12))

(defun 12- (num &rest more-nums)
  (mod (apply #'- num more-nums) 12))


; ===================================================================
; BETWEEN MIDICENTS (fixed @ 100/semitone) AND PITCH INTEGERS (variable)

(defparameter *mc/octave* 1200)

(defmethod! mc->p (midicents &optional (n 12))
  :icon '(141)
  :doc "
Converts from midicents to pitch space.

   midicents: middle C = 6000, semitone = 100

   pitch space: middle C = 0, octave/N = 1

The modulus N defaults to 12 but is an optional parameter
that can be set to other values."
  :indoc '("number or list")
  (om-round (om- midicents 6000)
            0
            (/ *mc/octave* n)))

;-----

(defmethod! p->mc (pitch &optional (n 12))
  :icon '(141)
  :doc "
Converts from pitch space to midicents.

   pitch space: middle C = 0, octave/N = 1

   midicents: middle C = 6000, semitone = 100

The modulus N defaults to 12 but is an optional parameter
that can be set to other values."
  :indoc '("number or list")
  (om+ (om* pitch
            (/ *mc/octave* n))
       6000))

; ===================================================================
; FROM PITCH TO PITCH CLASS

(defmethod! p->pc (pitch &optional (n 12))
  :icon '(141)
  :doc "
Converts from pitch number to mod-N pitch class number.

The modulus N defaults to 12 but is an optional parameter
that can be set to other values."
  :indoc '("number or list" "modulus of the pc space")
  (do-modular-reduction pitch n))

; ===================================================================
; NESTING->TIMING


(defun flatten (lst times)
  (let* ((m (max-depth lst))
         (times1 (* (signum times)
                    (min m (abs times))))
         (times2 (if (plusp times1)
                   times1
                   (+ m times1))))
    (if (null lst)
      (values nil nil)
      (rec-flatten lst times2 (make-list (length lst) :initial-element 1)))))


(defun rec-flatten (lst times nest-code)
  (if (zerop times)
    (values lst nest-code)
    (multiple-value-bind (new-lst new-nest-code)
                         (flatten-once lst nest-code)
      (rec-flatten new-lst (1- times) new-nest-code))))


(defun flatten-once (lst nest-code)
    (do ((lst-result nil)
         (nest-code-result nil)
         (lst-work lst
                   (cdr lst-work))
         (nest-code-work nest-code
                         (cdr nest-code-work)))
        ((null lst-work) (values lst-result nest-code-result))
      (let ((l (car lst-work)))
        (if (listp l)
          (setf lst-result (append lst-result l)
                nest-code-result (append nest-code-result
                                         (when l
                                           (append (make-list (1- (length l))
                                                              :initial-element 1)
                                                   (list (1+ (car nest-code-work)))))))
          (setf lst-result (append lst-result (list l))
                nest-code-result (append nest-code-result
                                         (list (1+ (car nest-code-work)))))))))


(defun max-depth (lst)
  (if (null lst)
    0
    (if (atom (car lst))
      (max-depth (cdr lst))
      (max (1+ (max-depth (car lst)))
           (max-depth (cdr lst))))))

(defmethod! nesting->timing ((lst list) (levels number) (time-unit number))
  :icon '(141)
  :doc
"output1 is <lst> flattened by a specified number of <levels>.
Or if <levels> is nonpositive, its absolute value determines the
number of nested levels to remain after flattening.

output2 is list with same length as output1, containing onset
times separated by <time-unit>, or by a multiple of this amount
when the separation corresponds to a boundary between sublists
of <lst> that have been joined by flattening.

The intended use is to flatten hierarchic pitch data into a form
acceptable to a chord-seq factory, and to recover the hierarchy
lost to flattening in the form of timing data acceptable to the
factory. Usual wiring: nesting->timing OUT-1 to chord-seq IN-2
and nesting->timing OUT-2 to chord-seq IN-3"

  :initvals '(nil -1 500)
  :indoc '("nested list of numbers"
           "number of nested levels to flatten [or if negative, abs val is number of nested levels to remain after flattening]"
           "time unit separating members of same sublist (multiplied at sublist boundaries)")
  :numouts 2

  (multiple-value-bind (l n)
      (flatten lst levels)
    (values l
            (dx->x 0
                   (mapcar (lambda (j) (* j time-unit))
                           n)))))


; ===================================================================
; HELP WITH -CLASSREP EXPRESSIONS

(defmethod! parse-incl-classreps (incl-classreps)
  :icon *PARSER_ICON*
  :doc "
Prints a description of each item in a list of incl-classrep expressions,
to assist in the construction of parameters that use this format.  This
function returns the input list, so it can be used 'inline'.

INCL-CLASSREP EXPRESSION SYNTAX

1.  undirected pc interval, possibly 0; or aggregation of them
-- integer in [0, N/2]; or list of them

2.  undirected pitch interval, possibly 0, or range of them; or
aggregation of intervals and/or ranges
-- (:p a-0 a-1 ...)
     a-i a nonnegative integer or pair (LO-i HI-i) representing the range
     [LO-i, HI-i]; a single interval or range takes the same form, and is
     therefore a list with head :p and tail of length 1

3.  t pcset class
-- (:t k-0 k-1 ...), k-i a mod-N integer

4.  ti pcset class
-- (:ti k-0 k-1 ...), k-i a mod-N integer

5.  t pitch set class
-- (:tp k-0 k-1 ...), k-i an integer

6.  t pitch set class
-- (:tip k-0 k-1 ...), k-i an integer"

  :indoc '("list of incl-classrep expressions")
  (loop for expression in incl-classreps do
        (cond ((numberp expression)
               (format t "~&ic ~A"
                       expression))
              ((listp expression)
               (let ((head (first expression)) (tail (rest expression)))
                 (cond ((numberp head)
                        (format t "~&ics ~A"
                                expression))
                       ((eq head :p)
                        (format t "~&undirected pitch interval(s) ~A"
                                (expand-range-expressions tail)))
                       ((eq head :t)
                        (format t "~&pcset ~A and its transpositions"
                                tail))
                       ((eq head :ti)
                        (format t "~&pcset ~A and its transpositions & inversions"
                                tail))
                       ((eq head :tp)
                        (format t "~&pitch set ~A and its transpositions"
                                tail))
                       ((eq head :tip)
                        (format t "~&pitch set ~A and its transpositions & inversions"
                                tail))
                       (t (format t "~&UNRECOGNIZED EXPRESSION: ~A" expression))))
               )))
  (fresh-line)
  incl-classreps)



;-----

(defmethod! parse-prog-classreps (prog-classreps)
  :icon *PARSER_ICON*
  :doc "
Prints a description of each item in a list of prog-classrep expressions,
to assist in the construction of parameters that use this format. This
function returns the input list, so it can be used 'inline'.

PROG-CLASSREP EXPRESSION SYNTAX

1.  directed pc interval, possibly 0; or aggregation of them
-- mod-N integer; or list of them

2.  directed pitch interval or range of them; or aggregation
of intervals and/or ranges
-- (:p a-0 a-1 ...)
     a-i a nonnegative integer or pair (LO-i HI-i) representing the range
     [LO-i, HI-i]; a single interval or range takes the same form, and is
     therefore a list with head :p and tail of length 1"
  :indoc '("list of prog-classrep expressions")
  (loop for expression in prog-classreps do
        (cond ((numberp expression)
               (format t "~&directed pc interval ~A"
                       expression))
              ((listp expression)
               (let ((head (first expression)) (tail (rest expression)))
                 (cond ((numberp head)
                        (format t "~&directed pc intervals ~A"
                                expression))
                       ((eq head :p)
                        (format t "~&directed pitch interval(s) ~A"
                                (expand-range-expressions tail)))
                       (t (format t "~&UNRECOGNIZED EXPRESSION: ~A" expression))))
               )))
  (fresh-line)
  prog-classreps)

;-----

(defun expand-range-expressions (x)
  (loop for expr in x
        append (if (numberp expr)
                 (list expr)
                 (let ((1st (first expr)) (2nd (second expr)))
                   (if (> 1st 2nd)
                     (loop for n from 2nd to 1st
                           collect n)
                     (loop for n from 1st to 2nd
                           collect n))))))

; ===================================================================
; FLATTENING NESTED LIST OF CHORDS

(defun atoms? (lst)
  (or (null lst)
      (and (atom (first lst))
           (atoms? (rest lst)))))

(defun do-flatten2chordlist (lst)
  (when lst
    (if (atoms? lst) (list lst)
        (append (do-flatten2chordlist (first lst))
                (do-flatten2chordlist (rest lst))))))

(defmethod! flatten2chordlist (chordtree)
  :icon '(235)
  :doc "
Given a nested list of chords, each of which is a flat list of pcs or
pitches, returns a flat list of the same chords -- that is, a list in
which each element is a flat list of pcs or pitches."
  :indoc '("(nested) list of chords")
  (do-flatten2chordlist chordtree))

;;;;; END
