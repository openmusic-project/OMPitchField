(in-package :om)

;;;;; ======================================================================
;;;;; OMTP 2.0
;;;;;
;;;;; FILTER

;;;;; In this file:
; filter-chordlist
; make-bounds-test
; make-width-test
; make-cardinality-test
; make-spacing-test
; make-register-test
; make-voicing-test
; and-tests
; or-tests
; ===================================================================
; ===================================================================



; FILTERING CHORD LISTS

(defmethod! filter-chordlist ((test t) (chordlist list) &optional (mode 'pass))
  :icon *FILT_ICON*
  :indoc '("test" "list of chords" "pass or reject")
  :menu-ins '((2 (("PASS" 'pass) ("REJECT" 'reject))))
  :doc "
TEST should be a predicate function returning T or NIL when applied to
the (nested) list CHORDLIST. The return value is a list like CHORDLIST
but with certain chords removed. If MODE is set to PASS (REJECT) then
chords for which TEST returns T (N) will be passed through and the
others removed.

FILTER-CHORDLIST performs as intended only when CHORDLIST has a
particular structure S, which if not empty can contain chords
(nonempty integer lists) or lists with structure S, but not a mix of
the two.

EXAMPLES (each item chord-K is a nonempty list of integers)

  GOOD: ((chord-1 chord-2) (chord-3) () (chord-4 chord-5))

  GOOD: (chord-1 chord-2 chord-3)

  GOOD: ((chord-1 chord-2) (() (chord-3 chord-4) (chord-5)))

  BAD: (chord-1 chord-2 NIL chord-3)
  contains a mix of chords and S-structures

  BAD: ((chord-1 (chord-2 chord-3)) (chord-4 chord-5))
  first sublist contains a mix of chords and S-structures"
  (if (listp (caar chordlist))
    (mapcar (lambda (item) (filter-chordlist test item mode))
            chordlist)
    (if (equal mode 'reject)
      (remove-if test chordlist)
      (remove-if-not test chordlist))))

; ===================================================================
; ===================================================================
; FILTER TESTS

(defmethod! make-bounds-test (lo hi)
  :icon *TEST_ICON*
  :indoc '("lowest permissible pitch" "highest permissible pitch")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test
if a pitch set fits entirely within the closed interval [LO, HI]."
  :initvals '(-6 17)
  (lambda (chord) (and (>= (reduce #'min chord) lo)
                       (<= (reduce #'max chord) hi))))

;-----

(defmethod! make-width-test (width)
  :icon *TEST_ICON*
  :indoc '("maximum permissible width")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test
if the interval between a pitch set's lowest and highest pitches is
less than or equal to WIDTH."
  :initvals '(18)
  (lambda (chord) (<= (- (reduce #'max chord) (reduce #'min chord))
                      width)))

;-----

(defmethod! make-cardinality-test (lo hi)
  :icon *TEST_ICON*
  :indoc '("least permissible cardinality" "greatest permissible cardinality")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test if
the number of notes in a pcset or pitch set is at least LO and at most HI."
  :initvals '(18)
  (lambda (chord) (<= lo (length chord) hi)))

; ===================================================================

#|
(defmethod! make-spacing-test (spacing-lists)
  :icon *TEST_ICON*
  :indoc '("each sublist of SPACING-LISTS contains integer pairs (LO HI)")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test if
the intervals between consecutive elements of a pitch set, traversed
from bottom to top, are in the ranges determined by the SPACING-LISTS.
Each of the SPACING-LISTS contains SPACING-PAIRS, which are pairs
(LO HI) specifying the minimum and maximum permissible distances
between consecutive pitches.

For a given chord C, testing proceeds as follows:

   The first item in SPACING-LISTS with an appropriate number of
   SPACING-PAIRS is located.

   The intervals of C are compared to the ranges of these SPACING-PAIRS.
   If every interval is in range, the test returns TRUE. Otherwise, the next
   item in SPACING-LISTS with an appropriate number of SPACING-PAIRS
   is located and the comparison step is repeated.

   If the intervals of C are not in the ranges determined by at least one of
   the SPACING-LISTS, the test returns FALSE."
  :initvals '((((3 9) (3 9))
               ((3 9) (3 9) (3 9))))
  
  (lambda (chord)
    (loop with chord-ints = (x->dx (sort (copy-list chord) #'<))
          with int-count = (length chord-ints)
          with right-length-lists
          = (remove-if-not (lambda (sp-list) (= (length sp-list) int-count))
                           spacing-lists)
          for sp-list in right-length-lists
          when (ints-in-ranges? chord-ints sp-list)
          return t))
  )


(defun ints-in-ranges? (ints ranges)
  (loop for i in ints and r in ranges
        unless (<= (first r) i (second r)) return nil
        finally return t))
|#

;Returning to 1.0 old version. the new one has a problem in ints-in-ranges?
;using return macro in a loop which is already defined in commonlisp  

(defmethod! make-spacing-test (spacing-pairs)
  :icon *TEST_ICON*
  :doc
"returns a test to see if the distances between consecutive notes of a chord,
traversed from bottom to top, are in the intervals specified by corresponding
elements of <spacing-pairs>; each element of this list is a pair (lo hi)
specifying the bounds of a closed interval. If there is a surplus of <spacing-
pairs>, ignores the extras; if there is a shortage, re-uses the last.

Example: with <spacing-pairs> = ((1 3) (6 8)), returns a test to see if a
three-note chord (lo med hi) meets the following requirements:

-- distance from lo to med at least 1, at most 3

-- distance from med to hi at least 6, at most 8
"
  
  :initvals '(((5 10) (1 5)))
  :indoc '("list of pairs of integers, SECOND of each pair no smaller than FIRST")
  :numouts 1
  
  (lambda (chord)
    (do ((work-chord (sort chord #'<)
                     (cdr work-chord))
         (work-bounds spacing-pairs
                      (or (cdr work-bounds)
                          work-bounds))
         (result t))
        ((or (null (cdr work-chord))
             (not result))
         result)
      (unless (<= (first (first work-bounds))
                  (- (second work-chord) (first work-chord))
                  (second (first work-bounds)))
        (setf result nil)))))

; ===================================================================

#|
(defmethod! make-register-test (register-lists)
  :icon *TEST_ICON*
  :indoc '("each sublist of REGISTER-LISTS contains integer pairs (LO HI)")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test if
the elements of a pitch set, traversed from bottom to top, sit within the
registers determined by the REGISTER-LISTS. Each of the REGISTER-LISTS
contains REGISTER-PAIRS, which are pairs (LO HI) specifying the register
from pitch LO through pitch HI.

For a given chord C, testing proceeds as follows:

   The first item in REGISTER-LISTS with an appropriate number of
   REGISTER-PAIRS is located.

   The pitches of C are compared to the registers of these REGISTER-
   PAIRS. If every pitch is within the corresponding register, the test
   returns TRUE. Otherwise, the next item in REGISTER-LISTS with an
   appropriate number of REGISTER-PAIRS is located and the
   comparison step is repeated.

   If the pitches of C are not in the registers determined by at least
   one of the REGISTER-LISTS, the rest returns FALSE."
  :initvals '((((-12 -1) (-5 6) (1 12))
               ((-12 3) (-12 3) (0 15) (0 15))))
  (lambda (chord)
    (loop with sorted-chord = (sort (copy-list chord) #'<)
          with chord-length = (length chord)
          with right-length-lists
          = (remove-if-not (lambda (r-list) (= (length r-list) chord-length))
                           register-lists)
          for r-list in right-length-lists
          when (ints-in-ranges? sorted-chord r-list)
          ; ints-in-ranges has wrong var names but right structure!
          return T))
  )
|#

(defmethod! make-register-test (reg-bounds reg-distribs)
  :icon *TEST_ICON*
  :indoc '("N integers define boundaries of N - 1 pitch registers"
           "each item is list of number of pitches expected in each register")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test if
the elements of a pitch set are distributed in particular ways across the
N - 1 registers determined by REG-BOUNDS. (Each register runs from its
lower bound to just below its upper bound.)

Each of the REG-DISTRIBS is a list of N - 1 numbers corresponding to the
expected number of pitches in each register, considered from low to high.

For a given chord C, testing proceeds as follows:

   If any pitches lie below the lowest (or above the highest) register
   specified in REG-BOUNDS, the test returns FALSE immediately.
   Otherwise, the registral distribution of pitches in C is compared to
   each element of REG-DISTRIBS. The test returns TRUE if a match is
   found, FALSE otherwise.

EXAMPLE

   REG-BOUNDS: (-12 0 12)
   REG-DISTRIBS: ((2 3) ; distrib-a
                            (0 5) ; distrib-b
                            (2 0) ; distrib-c
                            )

   Chord (-12 -6 3 5 7) matches distrib-a.
   Chord (0 3 5 6 7) matches distrib-b.
   Chord (-7 -5) matches distrib-c.
   Chord (-8 -7 -6 -5 -4) has distribution (5 0),
      which matches none of the expected distributions.
   Chord (1 7 12) has a pitch out of range."
  :initvals '((-9999 9999)
              ((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))
  (lambda (chord)
    (let ((chord-distrib (calculate-distrib chord reg-bounds)))
      (and chord-distrib
           (loop for rd in reg-distribs
                 when (equal chord-distrib rd)
                 return T)))))

(defun calculate-distrib (elements boundaries)
  (let* ((sorted-boundaries (sort (copy-list boundaries) #'<))
         (min-bound (first sorted-boundaries))
         (max-bound (first (last sorted-boundaries)))
         (template (make-list (1- (length boundaries))
                              :initial-element 0)))
    (loop for el in elements
          when (or (< el min-bound) (>= el max-bound))
          do (return NIL)
          else do (loop for sb in (rest sorted-boundaries)
                        for index upfrom 0
                        until (< el sb)
                        finally do (incf (nth index template)))
          finally (return template))))

; ===================================================================

(defmethod! make-voicing-test (voicing-pairs &optional (n 12))
  :icon *TEST_ICON*
  :indoc '("(INTS LIMS)" "modulus of pc space")
  :doc "
Returns a predicate, intended for use with FILTER-CHORDLIST, to test
if particular interval classes in a chord are voiced according to the
criteria specified in VSPEC-PAIRS, which is a list of pairs (INTS LIMS).
Each INTS is a list of undirected pitch intervals drawn from a single
mod-N interval class, and the absolute value of the corresponding LIM,
a positive (negative) number between 0 and 1 (-1), determines a lower
(upper) bound on the ratio J:K, where K is the multiplicity of occurrence
of the interval class represented in INTS, and J is the combined
multiplicity of occurrence of the undirected pitch intervals in INTS.

EXAMPLES (with N = 12)

- with VOICING-PAIRS = (((10 12) 3/4)), returns a test to see if at
least 3/4 of the instances of interval class 2 in a chord are voiced
as pitch intervals 10 or 22.

- with VOICING-PAIRS = (((1) 1/6) ((1) -1/2)), returns a test to see
if at least 1/6, but at most 1/2, of the instances of interval class 1
in a chord are voiced as pitch interval 1."
  :initvals '((((10 22) 3/4))
              12)
  (lambda (chord)
    (recursive-voicing-test chord voicing-pairs n)))


(defun recursive-voicing-test (chord voicing-pairs n)
  (or (null voicing-pairs)
      (let* ((v-pair (first voicing-pairs))
             (ic (i->ic (caar v-pair) n))
             (ic-count (ic-multiplicity ic chord n))
             (i-count (loop for i in (first v-pair)
                            sum (i-multiplicity i chord))))
        (and (or (zerop ic-count)       ; nothing to restrict
                 (>= (* (signum (second v-pair))
                        (/ i-count ic-count))
                     (second v-pair)))
             (recursive-voicing-test chord (rest voicing-pairs) n)))))

; ===================================================================
; INTERVAL UTILITIES

(defun i->ic (int n)
  (declare (fixnum int n))
  (min (mod int n)
       (mod (- int) n)))


(defun ic-multiplicity (ic chord n)     ; counts instances of undirected pc interval IC
  (declare (fixnum ic n))
  (if (null chord) 0
      (+ (the fixnum
           (loop with first-note fixnum = (first chord)
                 for note fixnum in chord
                 count (= ic (the fixnum (i->ic (- note first-note) n)))))
         (the fixnum
           (ic-multiplicity ic (rest chord) n)))))


(defun i-multiplicity (i chord)         ; counts instances of undirected pitch interval I
  (if (null chord) 0
      (+ (the fixnum
           (loop with first-note fixnum = (first chord)
                 for note fixnum in chord
                 count (= i (the fixnum (abs (- note first-note))))))
         (the fixnum
           (i-multiplicity i (rest chord))))))

;;;;; counting intervals from one chord to another
;updated by Teemu aka Nikoteemus

(defun ic-multiplicity2 (ic from-chord to-chord &optional (n 12))
  (if (null from-chord)
    0
    (+ (let ((first-note (first from-chord))
             (accum 0))
         (dolist (note to-chord accum)
           (when (= ic (i->ic (- note first-note) n))
             (incf accum))))
       (ic-multiplicity2 ic (rest from-chord) to-chord n))))

(defun i-multiplicity2 (i from-chord to-chord)
  (if (null from-chord)
    0
    (+ (let ((first-note (first from-chord))
             (accum 0))
         (dolist (note to-chord accum)
           (when (= i (- note first-note))
             (setf accum (1+ accum)))))
       (i-multiplicity2 i (rest from-chord) to-chord))))


(defun dir-pci-multiplicity2 (pci from-chord to-chord)
  (if (null from-chord)
    0
    (+ (let ((first-note (first from-chord))
             (accum 0))
         (dolist (note to-chord accum)
           (when (= pci (12- note first-note))
             (setf accum (1+ accum)))))
       (dir-pci-multiplicity2 pci (rest from-chord) to-chord))))

; ===================================================================
; vldg filtering

(defmethod! make-p-vldg-test ((from-chord list) (vlspec-triples list))
  :icon 130
  :doc
"returns a test to see if the voiceleading V from <from-chord> to a chord satisfies 
the criteria specified in <vlspec-triples>, which is a list of triples (ints lo hi).
Each 'ints' is a list containing one or more directed pitch intervals, and the 
corresponding 'lo' and 'hi' specify lower and upper limits on the combined
multiplicity of occurrence in V of the intervals from 'ints'."
  
  :initvals '((0 7 14)
              (((-3 -2 -1 1 2 3) 2 3)))
  :indoc '("list of integers"
           "list of triples, each structured: list of integers, integer, integer")
  :numouts 1
  
  (lambda (chord)
    (recursive-p-vldg-test from-chord chord vlspec-triples)))


(defun recursive-p-vldg-test (from-chord chord vlspec-triples)
  (if (null vlspec-triples)
    t
    (let* ((spec (first vlspec-triples))
           (i-count (let ((accum 0))
                      (dolist (i (first spec) accum)
                        (setf accum (+ accum (i-multiplicity2 i from-chord chord)))))))
      (and (<= (second spec) i-count (third spec))
           (recursive-p-vldg-test from-chord chord (rest vlspec-triples))))))


(defmethod! make-pc-vldg-test ((from-chord list) (vlspec-triples list))
; this one makes sense for filtering list of either pcsets or pitch sets
  :icon *TEST_ICON*
  :doc
"returns a test to see if the voiceleading V from <from-chord> to a chord satisfies 
the criteria specified in <vlspec-triples>, which is a list of triples (pci lo hi).
Each 'pci' is a directed or undirected pitch class interval; specifically, if 'pci'
is nonpositive, then its absolute value is understood to be an undirected pitch
class interval (interval class 0 to 6), and if 'pci' is nonnegative, then it is
understood to be a directed pitch class interval (0 to 11). In either case, the
corresponding 'lo' and 'hi' specify lower and upper limits on the multiplicity of
occurrence in V of 'pci'."

  :initvals '((0 2 7)
              ((3 0 0)))
  :indoc '("list of mod-12 integers"
           "list of triples: FIRST integer in [-6 , 11]; SECOND and THIRD integers")
  :numouts 1

  (lambda (chord)
    (recursive-pc-vldg-test from-chord chord vlspec-triples)))


(defun recursive-pc-vldg-test (from-chord chord vlspec-triples)
  (if (null vlspec-triples)
    t
    (let* ((spec (first vlspec-triples))
           (count (if (minusp (first spec))
                    (ic-multiplicity2 (- (first spec)) from-chord chord)
                    (dir-pci-multiplicity2 (first spec) from-chord chord))))
      (and (<= (second spec) count (third spec))
           (recursive-pc-vldg-test from-chord chord (rest vlspec-triples))))))



; ===================================================================
;------------------------
; abstract set class inclusion filtering

(defmethod! make-inclusion-test (incspec-triples &optional (mod 12))
; this one makes sense for filtering list of either pcsets or pitch sets
  :icon *TEST_ICON*
  :doc
"returns a test to see if a chord satisfies the criteria specified in
<incspec-triples>, which is a list of triples (classrep lo hi). Each
classrep is one of the following:

-- an integer in [1 , 6] representing an interval class

-- a list whose head is :t or :ti and whose tail is a list of mod12
integers representing a t- or ti-setclass

-- a list whose head is :tp or :tip and whose tail is a list of
integers representing a class of pitch sets related by pitch
transposition alone, or by pitch transposition and/or inversion

The values 'lo' and 'hi' determine lower and upper limits on the
multiplicity of occurrence of members of the corresponding class in
the chord to which this test is applied. Or if the 'classrep' is
LARGER than the chord, these limits apply to the multiplicity of
occurrence of t- or ti-transforms of the chord in the classrep."

  :initvals '((((:t 0 1 2) 0 1)) 12)       ; tests if chord contains 0 or 1 chromatic trichord(s)
  :indoc
  '("list of triples: FIRST integer in [1 , 6] or integer list beginning :t, :ti, :tp, or :tip; SECOND & THIRD integers")
  :numouts 1

  (lambda (chord)
    (recursive-inclusion-test (remove-duplicates chord) incspec-triples mod)))


(defun recursive-inclusion-test (chord incspec-triples n)
  (if (null incspec-triples)
    t
    (let* ((spec (first incspec-triples))
           (classcount (if (integerp (first spec))      ; interval class
                         (ic-multiplicity (i->ic (first spec) n) chord n);12 for the time being
                         (case (first (first spec))
                           (:t (if (> (length (rest (first spec)))      ; pc transposition
                                      (length chord))
                                 (t-class-multiplicity chord (rest (first spec)) n)
                                 (t-class-multiplicity (rest (first spec)) chord n )))
                           (:ti (if (> (length (rest (first spec)))     ; pc transposition/inversion
                                       (length chord))
                                  (ti-class-multiplicity chord (rest (first spec)) n)
                                  (ti-class-multiplicity (rest (first spec)) chord n)))
                           (:tp (if (> (length (rest (first spec)))     ; pitch transposition
                                       (length chord))
                                  (tp-class-multiplicity chord (rest (first spec)) n)
                                  (tp-class-multiplicity (rest (first spec)) chord n)))
                           (:tip (if (> (length (rest (first spec)))    ; pitch transposition/inversion
                                        (length chord))
                                   (tip-class-multiplicity chord (rest (first spec)) n)
                                   (tip-class-multiplicity (rest (first spec)) chord n)))
                           (otherwise
                            (format t "INCLUSION-TEST: ignoring ill-formed 'classrep' ~A~%" (first spec))
                            ;(ed-beep) ??
                            (om-beep)
                            (second spec))))))
      (and (<= (second spec) classcount (third spec))
           (recursive-inclusion-test chord (rest incspec-triples) n)))))


#|
(defun subsets (set n)
  "lists all the size-<n> subsets of <set>"
  (let ((len (length set)))
    (cond ((< len n) nil)
          ((= len n) (list set))
          ((zerop n) (list nil))
          (t
           (append (mapcar (lambda (s)
                             (cons (car set) s))
                           (subsets (cdr set) (1- n)))
                   (subsets (cdr set) n))))))


(defun t-class-multiplicity (classrep chord)
  "how many instances does <chord> contain of the t-setclass to which <classrep> belongs?"
  (let* ((classprime (t-primeform classrep))
         (subchords (subsets chord (length classprime)))
         (accum 0))
    (dolist (s subchords accum)
      (when (equal (t-primeform s) classprime)
        (setf accum (1+ accum))))))

(defun ti-class-multiplicity (classrep chord)
  "how many instances does <chord> contain of the ti-setclass to which <classrep> belongs?"
  (let ((classprime (t-primeform classrep))
        (classinv (t-primeform (mapcar #'12- classrep))))
    (+ (t-class-multiplicity classprime chord)
       (if (equal classprime classinv)
         0
         (t-class-multiplicity classinv chord)))))



(defun tp-class-multiplicity (classrep chord)
  "how many instances does <chord> contain of pitch transpositions of the pitch set <classrep>?"
  (let* ((classprime (tp-primeform classrep))
         (subchords (subsets chord (length classprime)))
         (accum 0))
    (dolist (s subchords accum)
      (when (equal (tp-primeform s) classprime)
        (setf accum (1+ accum))))))

(defun tip-class-multiplicity (classrep chord)
  "how many instances does <chord> contain of pitch transpositions of the pitch set <classrep> and its inversion?"
  (let* ((classprime (tp-primeform classrep))
         (classinv (tp-primeform (mapcar (lambda (x) (- x)) classrep))))
    (+ (tp-class-multiplicity classprime chord)
       (if (equal classprime classinv)
         0
         (tp-class-multiplicity classinv chord)))))


|#


; ===================================================================
; COMBINING TESTS

(defmethod! and-tests (&rest testfuns)
  :icon *AND_TEST_ICON*
  :doc "
Takes any number of predicate functions and returns a test that, for
a certain input, will return T if all the predicates return T for the
same input, or NIL if any of the predicates return NIL."
  (lambda (chord)
    (loop for tf in testfuns
          unless (funcall tf chord) return NIL
          finally (return T))))

;-----

(defmethod! or-tests (&rest testfuns)
  :icon *OR_TEST_ICON*
  :doc "
Takes any number of predicate functions and returns a test that, for
a certain input, will return T if any of the predicates return T for the
same input, or NIL if all the predicates return NIL."
  (lambda (chord)
    (loop for tf in testfuns
          when (funcall tf chord) return T
          finally (return NIL))))

;;;;; END
