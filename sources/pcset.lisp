;;;;; ======================================================================
;;;;; OMTP 2.0
;;;;;
;;;;; PCSET

;;;;; In this file:
; list-t-primeforms
; list-ti-primeforms
; t-primeform
; ti-primeform
; expand-t-setclass
; expand-ti-setclass
; xpose
; nvert
; set-complement

; ===================================================================
; PRIMEFORMS AND SET CLASSES

(defmethod! list-ti-primeforms ((card number) &optional (n 12) tag)
  :icon *PC_ICON*
  :doc "
Lists the primeform of each ti-setclass of cardinality CARD. This parameter,
normally an integer, can also be a list of integers, to produce results for
more than one cardinality.  Normally the pc space is twelve equal steps per
octave (semitones). If the optional parameter N is set to 24 rather than 12,
then the pc space is twenty-four equal steps per octave (quartertones); in
this case, CARD is currently limited to values of 8 or less. The TAG option
facilitates the construction of INCL-CLASSREP expressions, which serve as
inputs to several functions in OMPF.

A ti-setclass is a family of pcsets related to one another by transposition
or inversion."
  :initvals '(3 12 nil)
  :indoc '("number of elements in each primeform (or a list of such numbers)"
           "modulus of the pc space"
           "optional to tag w/:ti")
  :menuins '((1 (("12" 12) ("24" 24)))
            (2 (("NORMAL" nil) ("TAGGED [:ti]" :ti))))
  (let ((primeforms (nth card (if (= n 24)
                                *mod24primeforms*
                                *mod12primeforms*))))
    (if tag
      (mapcar (lambda (primeform) (cons tag primeform))
              primeforms)
      primeforms)))


(defmethod! list-ti-primeforms ((card list) &optional (n 12) tag)
  (loop for item in card collect (list-ti-primeforms item n tag)))

;-----

(defmethod! list-t-primeforms ((card number) &optional (n 12) tag)
  :icon *PC_ICON*
  :doc "
Lists the primeform of each t-setclass of cardinality CARD. This parameter,
normally an integer, can also be a list of integers, to produce results for
more than one cardinality.  Normally the pc space is twelve equal steps per
octave (semitones). If the optional parameter N is set to 24 rather than 12,
then the pc space is twenty-four equal steps per octave (quartertones); in
this case, CARD is currently limited to values of 8 or less. The TAG option
facilitates the construction of INCL-CLASSREP expressions, which serve as
inputs to several functions in OMPF.

A t-setclass is a family of pcsets related to one another by transposition."
  :initvals '(3 12 nil)
  :indoc '("number of elements in each primeform (or a list of such numbers)"
           "modulus of the pc space"
           "optional to tag w/:t")
  :menuins '((1 (("12" 12) ("24" 24)))
            (2 (("NORMAL" nil) ("TAGGED [:t]" :t))))
  (reduce #'append
          (mapcar (lambda (pcset)
                    (let* ((set-contents (if tag (cdr pcset) pcset))
                           (inv (t-primeform (nvert set-contents 0 n)))
                           (partner (unless (equal inv set-contents)
                                      (list (if tag (cons tag inv) inv)))))
                      (cons pcset partner)))
                  (list-ti-primeforms card n tag))))


(defmethod! list-t-primeforms ((card list) &optional (n 12) tag)
  (loop for item in card collect (list-t-primeforms item n tag)))

; ===================================================================

(defmethod! t-primeform (pcset &optional (n 12))
  :icon *PC_ICON*
  :doc "
A t-setclass is a family of pcsets related to one another by transposition, and
its t-primeform is a member of the family, designated to represent the entire
family. The optional parameter N can be used to set the number of equal steps
per octave to something other than the default of 12.

Will tolerate integers out of the mod-N range in PCSET."
  :initvals '((1 3 4) 12)
  :indoc '("pcset or list of them"
           "modulus of the pc space")
  (if (listp (car pcset))
    ;then multiple pcsets
    (t-primeforms pcset n)
    ;else
    (let* ((pcs (do-modular-reduction pcset n))
           (adj (adj-ints pcs n))
           (best-ints (butlast (most-left-packed-rotation adj))))
      (ints->pcs best-ints 0 n))))


(defun t-primeforms (pcset n)
  (loop for item in pcset collect (t-primeform item n)))

;-----

(defmethod! ti-primeform (pcset &optional (n 12))
  :icon *PC_ICON*
  :doc "
A ti-setclass is a family of pcsets related to one another by transposition
or inversion, and its ti-primeform is a member of the family, designated to
represent the entire family. The optional parameter N can be used to set the
number of equal steps per octave to something other than the default of 12.

Will tolerate integers out of the mod-N range in PCSET."
  :initvals '((1 3 4) 12)
  :indoc '("pcset or list of them"
           "modulus of the pc space")
  (if (listp (car pcset))
    ;then multiple pcsets
    (ti-primeforms pcset n)
    ;else
    (let* ((pcs (do-modular-reduction pcset n))
           (adj (adj-ints pcs n))
           (best-ints (butlast (most-left-packed-order adj))))
      (ints->pcs best-ints 0 n))))


(defun ti-primeforms (pcset n)
  (loop for item in pcset collect (ti-primeform item n)))

; ===================================================================

(defmethod! expand-t-setclass (pcset &optional (n 12))
  :icon *PC_ICON*
  :doc "
Given any member of a t-setclass, lists every member of that t-setclass.
The optional parameter N can be used to set the number of equal steps per
octave to something other than the default of 12.

Will tolerate integers out of the mod-N range in PCSET."
  :initvals '((1 3 4) 12)
  :indoc '("pcset or list of them"
           "modulus of the pc space")
  (if (listp (first pcset))
    (expand-t-setclasses pcset)
    (let ((t-prime (t-primeform pcset n)))
      (loop with result = nil
            repeat n
            for x-pcset = pcset then (xpose x-pcset 1 n)
            unless (member x-pcset result) collect x-pcset into result
            finally return result))))


(defmethod! expand-t-setclasses (pcset &optional (n 12))
  (loop for item in pcset collect (expand-t-setclass item n)))

;-----

(defmethod! expand-ti-setclass (pcset &optional (n 12))
  :icon *PC_ICON*
  :doc "
Given any member of a ti-setclass, lists every member of that ti-setclass.
The optional parameter N can be used to set the number of equal steps per
octave to something other than the default of 12.

Will tolerate integers out of the mod-N range in PCSET."
  :initvals '((1 3 4) 12)
  :indoc '("pcset or list of them"
           "modulus of the pc space")
  (if (listp (first pcset))
    (expand-ti-setclasses pcset n)
    (let ((t-setclass (expand-t-setclass pcset n))
          (inv (nvert pcset 0 n)))
      (if (member inv t-setclass :test #'equal)
        t-setclass
        (append t-setclass (expand-t-setclass inv n))))))
          

(defmethod! expand-ti-setclasses (pcset &optional (n 12))
  (loop for item in pcset collect (expand-ti-setclass item n)))

; ===================================================================
; PITCH AND PC OPERATIONS

(defmethod! xpose ((p-or-pc number) interval &optional (n 12))
  :icon *P/PC_ICON*
  :doc "
Performs pc- or p-space transposition of P-OR-PC by a designated
INTERVAL. The default case is a pc-space of twelve equal steps per
octave (semitones). The optional parameter N can be used to specify
a different number of equal steps per octave, or it can be set to
NIL for p-space.

Will tolerate integers out of the mod-N range in P-OR-PC and INTERVAL
when performing pc-space transposition."
  :initvals '(0 1 12)
  :indoc '("pitch or pc number (or list of them)"
           "interval of transposition"
           "modulus of the pc space (or NIL for p space)")
  (declare (fixnum p-or-pc interval))
  (if n
    (the fixnum (modular+ n p-or-pc interval))
    (the fixnum (+ p-or-pc interval))))


(defmethod! xpose ((p-or-pc list) interval &optional (n 12))
  (loop for element in p-or-pc collect (xpose element interval n)))

;-----

(defmethod! nvert ((p-or-pc number) index &optional (n 12))
  :icon *P/PC_ICON*
  :doc "
Performs pc- or p-space inversion of P-OR-PC by a designated INDEX.
The default case is a pc-space of twelve equal steps per octave
(semitones). The optional parameter N can be used to specify a
different number of equal steps per octave, or it can be set to NIL
for p-space.

Will tolerate integers out of the mod-N range in P-OR-PC and INDEX
when performing pc-space inversion."
  :initvals '(1 0 12)
  :indoc '("pitch or pc number (or list of them)"
           "index of inversion"
           "modulus of the pc space (or NIL for p space)")
  (declare (fixnum p-or-pc index))
  (if n
    (the fixnum (modular- n index p-or-pc))
    (the fixnum (- index p-or-pc))))


(defmethod! nvert ((p-or-pc list) index &optional (n 12))
  (loop for element in p-or-pc collect (nvert element index n)))

;-----

(defmethod! set-complement (p-or-pc-set &optional (space *12aggregate*))
  :icon *P/PC_ICON*
  :doc "
Performs complementation of P-OR-PC with respect to the twelve-pc
aggregate or another space when designated by the optional parameter
SPACE."
  :initvals '((0 1 2) '(0 1 2 3 4 5 6 7 8 9 10 11))
  :indoc '("pitch-or-pc set (or list of them)"
           "space in which complementation is performed")
  (if (listp (first p-or-pc-set)) ; works for atoms or sublists BUT NOT A MIXTURE
    (sets-complement p-or-pc-set space)
    (set-difference space p-or-pc-set)))


(defun sets-complement (sets space)
  (loop for item in sets collect (set-complement item space)))

; ===================================================================
; ===================================================================
; MODULAR ARITHMETIC

(defun modular+ (n &rest nums)
  (the fixnum (mod (the fixnum (reduce  #'+ nums)) n)))

(defun modular- (n &rest nums)
  (the fixnum (mod (the fixnum (reduce #'- nums)) n)))

; this DO-MODULAR-REDUCTION function is identical to the P->PC utility
; [but make N an optional param in that, keep this fast, call this from that]
(defun do-modular-reduction (val n)
  (declare (fixnum n))
  (if (numberp val)
    (the fixnum (mod val n))
    (loop for v in val collect (do-modular-reduction v n))))

; ===================================================================
; PRIME FORM CALCULATIONS

(defun adj-ints (pcset n)
  "intervals between adjacent elements, including wrap-around interval from last to first"
  (setf pcset (sort (remove-duplicates pcset) #'<))
  (setf pcset (append pcset
                      (list (the fixnum (+ (first pcset) n)))))
  (loop for q in pcset
        for r in (rest pcset)
        collect (the fixnum (- r q))))

(defun rotate-left (lst)
  "rotate one position left: (rotate-left '(0 1 2 3)) ==> (1 2 3 0)"
  (append (rest lst) (list (first lst))))

(defun rotate-right (lst)
  "rotate one position right: (rotate-right '(0 1 2 3)) ==> (3 0 1 2)"
  (append (last lst) (butlast lst)))

(defun most-left-packed-rotation (ints)
  (loop repeat (length (rest ints))
        with rev = (reverse ints)
        with best = rev
        for candidate = (rotate-left rev) then (rotate-left candidate)
        do (when (bigger candidate best) (setf best candidate))
        finally (return (reverse best))
        ))

(defun most-left-packed-order (ints)
  "tests rotations and their retrogrades"
  (loop repeat (length ints)
        for candidate = (rotate-left ints) then (rotate-left candidate)
        for rev-candidate = (reverse candidate)
        do
        (when (bigger candidate ints) (setf ints candidate))
        (when (bigger rev-candidate ints) (setf ints rev-candidate))
        finally (return (reverse ints))
        ))

(defun bigger (list1 list2)
  (when (and list1 list2)
    (or (> (the fixnum (first list1)) (the fixnum (first list2)))
        (and (= (the fixnum (first list1)) (the fixnum (first list2)))
             (bigger (rest list1) (rest list2))))))

(defun ints->pcs (ints start n)
  (declare (fixnum start n))
  (cons start
        (loop for int fixnum in ints
              sum int into accum
              collect (modular+ n start accum))))

;;;;; END
