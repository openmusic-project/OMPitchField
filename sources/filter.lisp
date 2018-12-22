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

(defmethod! filter-chordlist (test chordlist &optional (mode 'PASS))
  :icon *FILT_ICON*
  :indoc '("test" "list of chords" "pass or reject")
  :menu-ins '((2 (("PASS" 'PASS) ("REJECT" 'REJECT))))
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
    (mapcar (lambda (item) (filter-chord-list test item mode))
            chordlist)
    (if (eq mode 'REJECT)
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
          return T))
  )


(defun ints-in-ranges? (ints ranges)
  (loop for i in ints and r in ranges
        unless (<= (first r) i (second r)) return NIL
        finally return T))


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
          finally return template)))

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
          finally return T)))

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
          finally return NIL)))

;;;;; END
