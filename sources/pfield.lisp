(in-package :om)

;;;;; ======================================================================
;;;;; OMTP 2.0
;;;;;
;;;;; PFIELD

;;;;; In this file:
; make-cyc-pfield
; merge-pfields
; find-pc-in-field
; find-pcset-in-field
; find-bounded-chords-in-field [new, summer 2006]


; ===================================================================
; PFIELD CONSTRUCTION

(defmethod! make-cyc-pfield (generator origin lo hi)
  :icon *PFIELD_ICON*
  :doc "
Makes a pitch field that repeatedly unfolds the cyclic interval pattern
GENERATOR, transposed so that a cycle begins at ORIGIN, and truncated
to the range from LO through HI."
  :initvals '((1 4) 0 -12 24)
  :indoc '("list of positive integers" "integer" "integer" "integer")
  (when (<= lo hi)
    (let* ((gen-sum (reduce #'+ generator))
           (start (+ origin
                     (* gen-sum
                        (floor (- lo origin) gen-sum)))))
      (loop while (< start lo)
            do (setf start (+ start (first generator))
                     generator (rotate-left generator)))
      (loop for g = generator then (rotate-left g)
            and p = start then (+ p (first g))
            until (> p hi)
            collect p))))

;-----

(defmethod! merge-pfields (&rest pfield)
  :icon *PFIELD_ICON*
  :doc "
Merges the contents of any number of pitch fields, with duplicate pitches removed."
  (sort (remove-duplicates (loop for pf in pfield append pf)
                           :test #'equal)
        #'<))

; ===================================================================
; ===================================================================
; PFIELD SEARCHING

(defmethod! find-pc-in-field ((pc fixnum) field &optional (n 12))
  :icon *PFIELD_ICON*
  :doc "
Returns a list of all the pitches in FIELD that are congruent mod-N to PC.
The modulus defaults to 12 unless set to a different value via the optional
parameter N."
  :initvals '(0 (0 2 5 7 10 12 15) 12)
  :indoc '("mod-N integer (or list of them)"
           "list of integers"
           "modulus of the pc space")
  (when field
    (if (= (the fixnum (mod (first field) n)) pc)
      (cons (first field) (find-pc-in-field pc (rest field) n))
      (find-pc-in-field pc (rest field) n))))


(defmethod! find-pc-in-field ((pc list) field &optional (n 12))
  (loop for item in pc
        collect (find-pc-in-field item field n)))

;-----

(defmethod! find-pcset-in-field (pcset field &optional (n 12))
  :icon *PFIELD_ICON*
  :doc "
Returns a list of all the pitch sets in FIELD whose contents are congruent
mod-N to PCSET. The modulus defaults to 12 unless set to a different value
via the optional parameter N."
  :initvals '((0 2) (0 2 5 7 10 12 15) 12)
  :indoc '("list of mod-12 integers (or list of such lists)"
           "list of integers"
           "modulus of the pc space")
  (if (listp (first pcset))
    (find-pcsets-in-field pcset field n)
    (1each (mapcar (lambda (pc) (find-pc-in-field pc field n))
                   pcset))))


(defun find-pcsets-in-field (pcsets field n)
  (loop for item in pcsets
        collect (find-pcset-in-field item field n)))

;-----

(defmethod! find-bounded-chords-in-field (registers field)
  :icon *PFIELD_ICON*
  :doc "
Returns a list of all the pitch sets in FIELD whose elements
lie within specified REGISTERS."
  :initvals '(((-12 0) (-6 6) (0 12) (0 12))
              (-15 -12 -11 -8 -7 -5 -4 -2 1 2 5 10 11 14 15))
  :indoc '("list of pairs of integers"
           "list of integers")
  (remove-duplicates (all-combos (registers->pitches registers field))
                     :test 'equal))

(defun registers->pitches (registers field)
  (when registers
    (cons (trunc-to-bounds (first registers) field)
          (registers->pitches (rest registers) field))))

(defun trunc-to-bounds (bounds field)
  (when field
    (if (<= (first bounds) (first field) (second bounds))
      (cons (first field) (trunc-to-bounds bounds (rest field)))
      (trunc-to-bounds bounds (rest field)))))

; ===================================================================

(defun 1each (lists)
  "lists all possible collections containing one element from each of the LISTS of fixnums"
  (if (null lists)
    (list nil)
    (mapcan (lambda (x)
              (declare (fixnum x))
              (mapcar (lambda (y) (cons x y))
                      (1each (rest lists))))
            (first lists))))

; This function assumes each of the sources is a list of numbers.
; Each combo in its output is arranged in ascending order, so
; duplicate combos can be more easily detected and removed.
(defun all-combos (sources)
  "(all-combos '((1 2) (3 4))) ==> ((1 3) (1 4) (2 3) (2 4))"
  (if (null sources) (list nil)
      (when (first sources)
        (append (combine (first (first sources))
                         (prune (first (first sources)) (all-combos (rest sources))))
                (all-combos (cons (rest (first sources)) (rest sources)))))))

(defun combine (num lists)
  (mapcar (lambda (list) (sort (cons num list) #'<))
          lists))

(defun prune (num lists)
  (when lists
    (if (member num (first lists))
      (prune num (rest lists))
      (cons (first lists) (prune num (rest lists))))))


    
;;;;; END

