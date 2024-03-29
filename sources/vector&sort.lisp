(in-package :om)

;;;;; ======================================================================
;;;;; OMTP 2.0
;;;;;
;;;;; VECTOR & SORT

;;;;; In this file:
; sort+
; sort-key_width
; vector-dotprod
; vector-angle
; incl-vec
; incl-vec-angle
; prog-vec
; prog-vec-angle
; sort-key_incl-vec-sum
; sort-key_incl-vec-angle
; sort-key_prog-vec-sum
; sort-key_prog-vec-angle


; ===================================================================
; SORT, AND MANAGE ITEMS WITH SAME POSITION IN SORT ORDER

(defmethod! sort+ ((lst list)  &optional (test '<) (key nil))
  :initvals '(nil < nil)
  :indoc '("the list" "test" "key")
  :doc "
Sorts list and reports when multiple items have same position in sort order.
<test> binary function name or object, indicating how to compare elements.
<key> binary function name or function object that will be applied to elements.

A function icon or subpatch icon in 'lambda' mode can be connected to <test>
and <key>.

The first output is the sorted list. The second output indicates equal element
(or key) values; for instance, if the second output begins (1 3 ...), then the first
element comes first in the sort order, the next three elements are tied for
second in the sort order, and so on."
  :numouts 2
  :icon *SORT_ICON*
  (let* ((sorted-lst (sort (copy-list lst) (or test #'<) :key key))
         (sorted-keys (if key (mapcar key sorted-lst) sorted-lst)))
    (values sorted-lst (count-runs sorted-keys nil nil))
    ))


(defun count-runs (lst progress prev-val)
  (if (null lst)
    (reverse progress)
    (if (and prev-val (= (first lst) prev-val))
      (count-runs (rest lst)
                  (cons (1+ (first progress)) (rest progress))
                  prev-val)
      (count-runs (rest lst)
                  (cons 1 progress)
                  (first lst)))))

; ===================================================================
; SORT AND THEN SELECT FROM THE TOP OF THE SORTED LIST

(defmethod! sort+select ((lst list)  (n integer) &optional (test '<) (key nil))
  :initvals '(nil 0 < nil)
  :indoc '("the list" "select how many?" "test" "key")
  :doc "
Sorts list and selects the N elements at the top of the list. If certain
elements score identically on the key used for sorting, and N is such that
some but not all of these elements should be selected, then this part of
the selection is made randomly.

A function icon or subpatch icon in 'lambda' mode can be connected to <test>
and <key>."

  :icon *FILT_ICON*

  (if (>= n (length lst)) lst
      (multiple-value-bind (sorted-elements equalities) (sort+ lst test key)
        (do-sort+select sorted-elements equalities n))))


(defun do-sort+select (sorted-elements equalities n)
  (unless (zerop n)
    (let ((e (first equalities)))
      (if (<= e n)
        (append (firstn sorted-elements e)
                (do-sort+select (nthcdr e sorted-elements) (rest equalities) (- n e)))
        (choose-n-randomly (firstn sorted-elements e) n)))))


(defun choose-n-randomly (lst n)
  (loop repeat n
        for l = lst then (remove-nth-elt rand-num l)
        for rand-num = (om-random 0 (1- (length l)))
        collect (nth rand-num l)))


(defun remove-nth-elt (n lst)
  (when lst
    (if (zerop n) (rest lst)
        (cons (first lst) (remove-nth-elt (1- n) (rest lst))))))


; ===================================================================
; SORT USING REGISTRAL WIDTH

(defmethod! sort-key_width ()
  :doc "
Returns a key function for sorting chords based on registral width."
  :icon *KEY_ICON*
  (lambda (chord)
    (- (reduce #'max chord) (reduce #'min chord))))

; ===================================================================
; BASIC VECTOR MATH

(defparameter *vec-angle-decimals* 3)

;-----

(defmethod! vector-dotprod (v w)
  :icon *VECT_ICON*
  :doc "
Returns the dot product of vectors V and W."
  :indoc '("list representing vector V" "list representing W, same length as V")
  (reduce #'+ (mapcar #'* v w)))

;-----

(defmethod! vector-angle (v w)
  :icon *VECT_ICON*
  :doc "
Returns the angle of length-N vectors V and W in N-dimensional space.
This is a value in [0, pi/2].

If one or both of the vectors have magnitude zero, then the angle
between them is not defined. But this function adopts a pragmatic
strategy: if both magnitudes are zero, then it returns 0; if one
magnitude is zero and the other is nonzero, then it returns pi/2."
  :indoc '("list representing vector V" "list representing W, same length as V")
  (cond ((and (all-zeros v) (all-zeros w)) 0.0)
        ((or (all-zeros v) (all-zeros w)) (om-round (/ pi 2) *vec-angle-decimals*))
        (t (om-round (acos (om-round (/ (vector-dotprod v w)
                                        (* (sqrt (vector-dotprod v v))
                                           (sqrt (vector-dotprod w w))))
                                     *vec-angle-decimals*))
                     *vec-angle-decimals*))))


(defun all-zeros (vec)
  (not (position-if-not #'zerop vec)))

; ===================================================================
; INCLUSION VECTORS

(defmethod! incl-vec (chord classreps &optional (n 12))
  :icon *VECT_ICON*
  :initvals '(nil (1 2 3 4 5 6) 12)
  :doc "
Given a chord C and a list of expressions representing equivalence
classes, computes an 'inclusion vector' that reports, for each
equivalence class E, the number of members of E that include or
are included in C. Use the optional parameter N to change the pc-
space modulus from its default value of 12.

When N is 12 and CLASSREPS is '(1 2 3 4 5 6), computes the
standard 'interval vector' of CHORD.

The CLASSREPS parameter should be a list of INCL-CLASSREP
expressions, which have a special syntax. For more information,
see PARSE-INCL-CLASSREPS and its documentation."
  :indoc '("list of integers"
           "list of INCL-CLASSREP expressions"
           "modulus of the pc space")
  (if (listp (first chord))
    (incl-vecs chord classreps n)
    (do-incl-vec chord classreps n)))


(defun do-incl-vec (chord classreps n) ; assume CHORD is truly a single chord (flat list)
  (mapcar (lambda (expression)
            (if (numberp expression)
              (ic-multiplicity (i->ic expression n) chord n)
              (let ((head (first expression)) (tail (rest expression)))
                (cond ((numberp head)
                       (loop for i in expression
                             sum (ic-multiplicity (i->ic i n) chord n)))
                      ((eq head :p)
                       (loop for i in (expand-range-expressions tail)
                             sum (i-multiplicity i chord)))
                      ((eq head :t)
                       (if (> (length tail) (length chord))
                         (t-class-multiplicity chord tail n)
                         (t-class-multiplicity tail chord n)))
                      ((eq head :ti)
                       (if (> (length tail) (length chord))
                         (ti-class-multiplicity chord tail n)
                         (ti-class-multiplicity tail chord n)))
                      ((eq head :tp)
                       (if (> (length tail) (length chord))
                         (tp-class-multiplicity chord tail)
                         (tp-class-multiplicity tail chord)))
                      ((eq head :tip)
                       (if (> (length tail) (length chord))
                         (tip-class-multiplicity chord tail)
                         (tip-class-multiplicity tail chord)))))))
          classreps))


(defun incl-vecs (chord classreps n)
  (loop for item in chord
        collect (incl-vec item classreps n)))

;-----

(defmethod! incl-vec-angle (chord1 chord2 classreps &optional (n 12))
  :icon *VECT_ICON*
  :initvals '(nil nil (1 2 3 4 5 6) 12)
  :doc "
Calculates length-k inclusion vectors for CHORD1 and CHORD2 (see
documentation of function INCL-VEC for details). These vectors are
given a geometric interpretation which locates them at a common
origin in k-dimensional space. The angle between the two vectors
is then calculated and returned.

When N is 12 and CLASSREPS is '(1 2 3 4 5 6), the inclusion vectors
are standard interval vectors, and the return value is the 'interval
angle' proposed as a measure of pcset similarity (smaller angles
indicating greater similarity) in Damon Scott and Eric J. Isaacson,
'The Interval Angle: A Similarity Measure for Pitch-Class Sets,'
Perspectives of New Music 36.2 (Summer 1998): 107-142."
  :indoc '("list of integers" "list of integers"
           "list of INCL-CLASSREP expressions"
           "modulus of the pc space")
  (vector-angle (do-incl-vec chord1 classreps n)
                (do-incl-vec chord2 classreps n)))



; ===================================================================
; INCLUSION OF SET CLASSES

(defun subsets (set n)
  "lists all the size-N subsets of SET"
  (declare (fixnum n))
  (let ((len (the fixnum (length set))))
    (cond ((< len n) nil)
          ((= len n) (list set))
          ((zerop n) (list nil))
          (t (append (mapcar (lambda (s) (cons (first set) s))
                             (subsets (rest set) (1- n)))
                     (subsets (rest set) n))))))

;-----
; t-class-multiplicity ti-class-multiplicity tp-class-multiplicity tip-class-multiplicity
;-----

(defun t-class-multiplicity (classrep chord n)
  "how many instances does CHORD contain of transpositions of the pcset CLASSREP?"
  (loop with classprime = (t-primeform classrep n)
        with subchords = (subsets chord (length classprime))
        for s in subchords
        count (equal (t-primeform s n) classprime)))


(defun ti-class-multiplicity (classrep chord n)
  "how many instances does CHORD contain of transpositions of the pcset CLASSREP and its inversion?"
  (let ((classprime (t-primeform classrep n))
        (classinv (t-primeform (nvert classrep 0 n) n)))
    (+ (the fixnum (t-class-multiplicity classprime chord n))
       (if (equal classprime classinv) 0
           (the fixnum (t-class-multiplicity classinv chord n))))))


(defun tp-class-multiplicity (classrep chord)
  "how many instances does CHORD contain of transpositions of the pitch set CLASSREP?"
  (loop with classprime = (tp-primeform classrep)
        with subchords = (subsets chord (length classprime))
        for s in subchords
        count (equal (tp-primeform s) classprime)))


(defun tip-class-multiplicity (classrep chord)
  "how many instances does CHORD contain of transpositions of the pitch set CLASSREP and its inversion?"
  (let ((classprime (tp-primeform classrep))
        (classinv (tp-primeform (nvert classrep 0 nil))))
    (format t "~&classprime ~A classinv ~A" classprime classinv)
    (+ (the fixnum (tp-class-multiplicity classprime chord))
       (if (equal classprime classinv) 0
           (the fixnum (tp-class-multiplicity classinv chord))))))

;-----

(defun tp-primeform (pitchset)
  "transpose so lowest pitch is zero"
  (let* ((sorted-pitches (sort (copy-list pitchset) #'<)))      ; 8/2004 PCN added copy-list
    (mapcar (lambda (pitch) (- pitch (first sorted-pitches)))
            sorted-pitches)))


; ===================================================================
; PROGRESSION VECTORS

(defmethod! prog-vec (from-chord to-chord classreps
                                 &optional (n 12))
  :icon *VECT_ICON*
  :initvals '(nil nil (1 2 3 4 5 6) 12)
  :doc "
For a given list of directed intervals, as specified in CLASSREPS,
calculates how many of each of these intervals can be formed from
an element of FROM-CHORD to an element of TO-CHORD. This result is
known as the 'progression vector'.

Depending on how CLASSREPS is configured, PROG-VEC calculations
may or may not make sense when FROM-CHORD and TO-CHORD are
interpreted as pcsets. (The modulus of the pitch class space defaults
to 12 but can be set to another value via the optional parameter N.)

The CLASSREPS parameter should be a list of PROG-CLASSREP
expressions, which have a special syntax. For more information,
see PARSE-PROG-CLASSREPS and its documentation."
  :indoc '("list of integers" "list of integers"
           "list of PROG-CLASSREP expressions"
           "modulus of the pc space")
  (mapcar (lambda (expression)
            (if (numberp expression)
              (dir-pc-int-multiplicity (mod expression n) from-chord to-chord n)
              (let ((head (first expression)) (tail (rest expression)))
                (cond ((numberp head)
                       (loop for i in expression
                             sum (dir-pc-int-multiplicity (mod i n) from-chord to-chord n)))
                      ((eq head :p)
                       (loop for i in (expand-range-expressions tail)
                             sum (dir-p-int-multiplicity i from-chord to-chord)))))))
          classreps))

;-----

(defmethod! prog-vec-angle (from1 to1 from2 to2 classreps &optional (n 12))
  :icon *VECT_ICON*
  :initvals '(nil nil (0 1 2 3 4 5 6 7 8 9 10 11) 12)
  :doc "
Calculates length-k progression vectors for pairs (FROM1 TO1) and
(FROM2 TO2) (see documentation of function PROG-VEC for details).
These vectors are given a geometric interpretation which locates
them at a common origin in k-dimensional space. The angle between
the two vectors is then calculated and returned."
  :indoc '("list of integers" "list of integers"
           "list of integers" "list of integers"
           "list of PROG-CLASSREP expressions"
           "modulus of the pc space")
  (vector-angle (prog-vec from1 to1 classreps n)
                (prog-vec from2 to2 classreps n)))

; ===================================================================
; DIRECTED INTERVAL UTILITIES

(defun dir-pc-int-multiplicity (i from-chord to-chord n)
  "counts instances of directed pc interval I (a value from 0 to N)"
  (declare (fixnum i n))
  (if (null from-chord) 0
      (+ (the fixnum
           (loop with first-note fixnum = (first from-chord)
                 for note fixnum in to-chord
                 count (= i (the fixnum (modular- n note first-note)))))
         (the fixnum
           (dir-pc-int-multiplicity i (rest from-chord) to-chord n)))))

(defun dir-p-int-multiplicity (i from-chord to-chord)
  "counts instances of directed pitch interval I (any integer)"
  (declare (fixnum i))
  (if (null from-chord) 0
      (+ (the fixnum
           (loop with first-note fixnum = (first from-chord)
                 for note fixnum in to-chord
                 count (= i (the fixnum (- note first-note)))))
         (the fixnum
           (dir-p-int-multiplicity i (rest from-chord) to-chord)))))

; ===================================================================
; ===================================================================
; SORT-KEYS BASED ON VECTORS

(defmethod! sort-key_incl-vec-sum (classreps weightlist &optional (n 12))
  :doc "
Returns a key function for sorting chords based on a weighted sum
of the positions in each chord's inclusion vector, calculated for
the equivalence classes represented in CLASSREPS. This parameter
should be a list of INCL-CLASSREP expressions, which have a
special syntax. For more information, see PARSE-INCL-CLASSREPS
and its documentation.

The modulus of the pitch class space defaults to 12 but can be set
to other values via the optional parameter N."
  :icon *KEY_ICON*
  :indoc '("list of INCL-CLASSREP expressions"
           "list of numbers"
           "modulus of the pc space")
  :initvals '((1 2 3 4 5 6) (8 4 2 2 1 5) 12)
  (lambda (chord)
    (vector-dotprod (do-incl-vec chord classreps n)
                    weightlist)))

;-----

(defmethod! sort-key_incl-vec-angle (classreps refchord &optional (n 12))
  :doc "
Returns a key function for sorting a list of chords based on the
angle measured from the inclusion vector of REFCHORD to the
inclusion vector of each chord in the list. Inclusion vectors are
calculated for the equivalence classes represented in CLASSREPS,
which should be a list of INCL-CLASSREP expressions with special
syntax. For more information, see PARSE-INCL-CLASSREPS and its
documentation.

The modulus of the pitch class space defaults to 12 but can be set
to other values via the optional parameter N."
  :icon *KEY_ICON*
  :indoc '("list of INCL-CLASSREP expressions"
           "list of integers"
           "modulus of the pc space")
  :initvals '((1 2 3 4 5 6) nil 12)
  (lambda (chord)
    (vector-angle (do-incl-vec refchord classreps n)
                  (do-incl-vec chord classreps n))))

;-----

(defmethod! sort-key_prog-vec-sum (classreps from-chord weightlist &optional (n 12))
  :doc "
Returns a key function for sorting a list of chords based on a
weighted sum of the positions in the progression vector from
FROM-CHORD to each chord in the list. Progression vectors are
calculated for the intervals represented in CLASSREPS, which
should be a list of PROG-CLASSREP expressions with special
syntax. For more information, see PARSE-PROG-CLASSREPS
and its documentation.

The modulus of the pitch class space defaults to 12 but can
be set to other values via the optional parameter N."
  :icon *KEY_ICON*
  :indoc '("list of PROG-CLASSREP expressions"
           "list of integers"
           "list of numbers"
           "modulus of the pc space")
  :initvals '((0 1 2 3 4 5 6 7 8 9 10 11) nil (0 4 4 1 1 1 1 1 1 1 4 4) 12)
  (lambda (chord)
    (vector-dotprod (prog-vec from-chord chord classreps n)
                    weightlist)))

;-----

(defmethod! sort-key_prog-vec-angle (classreps from-chord ref-from ref-to
                                               &optional (n 12))
  :doc "
Returns a key function for sorting a list of chords based on 
the angles measured from the progression vector of the pair
(REF-FROM REF-TO) to the progression vector of the pair
(FROM-CHORD <c>), where each chord in the list takes the
place of <c>. Progression vectors are calculated for the
intervals represented in CLASSREPS, which should be a list
of PROG-CLASSREP expressions with special syntax. For
more information, see PARSE-PROG-CLASSREPS and its
documentation.

The modulus of the pitch class space defaults to 12 but can
be set to other values via the optional parameter N."
  :icon *KEY_ICON*
  :indoc '("list of PROG-CLASSREP expressions"
           "list of integers" "list of integers" "list of integers"
           "modulus of the pc space")
  :initvals '((0 1 2 3 4 5 6 7 8 9 10 11) nil nil nil 12)
  (lambda (chord)
    (vector-angle (prog-vec ref-from ref-to classreps n)
                  (prog-vec from-chord chord classreps n))))

; ===================================================================
; COMBINING KEYS

(defmethod! mult-keys (&rest keys)
  :icon *MULT_KEY_ICON*
  :doc "
Takes any number of key functions and returns a new function for
sorting a list of chords based on the product of all the key values."
  (lambda (chord)
    (loop for k in keys
          for val = (funcall k chord) then (* val (funcall k chord))
          finally return val)))

;;;;; END
