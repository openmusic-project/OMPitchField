;;;;============================================================================
;;;; OMPF.lib
;;;;
;;;; OMPitchField library version 2.0
;;;; author: Paul Nauert
;;;;
;;;; VERSION 0.5, 8 November 2001
;;;; with the assistance of C. Agon, G. Assayag, K. Haddad
;;;; and the support of the University of California, Santa Cruz Faculty Senate
;;;;
;;;; VERSION 2.0, 1 June 2004 and subsequent refinements through summer 2006
;;;; with the support of the University of California, Santa Cruz Faculty Senate
;;;; and the Columbia University Department of Music
;;;;============================================================================ 

(in-package :om)

;--------------------------------------------------
;Variable definition with files to load 
;--------------------------------------------------

(defvar *sources-dir* "source files")
(setf *sources-dir* (append (pathname-directory *load-pathname*) (list "sources")))

(defvar *PitchField-lib-files* nil)
(setf *PitchField-lib-files*
      '("database"
        "pcset"
        "pfield"
        "filter"
        "vector&sort"
        "utility"
        ))


;--------------------------------------------------
;Loading & compiling files 
;--------------------------------------------------

(mapc #'(lambda (file) (compile&load (make-pathname :directory *sources-dir* :name file))) *PitchField-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *subpackages-list1* nil)
(setf *subpackages-list1*
      '(("pcset" nil nil
         (
          list-t-primeforms list-ti-primeforms t-primeform ti-primeform
          expand-t-setclass expand-ti-setclass xpose nvert set-complement
          ) nil)
        ("pfield" nil nil
         (
          make-cyc-pfield merge-pfields find-pc-in-field
          find-pcset-in-field find-bounded-chords-in-field
          ) nil)
        ("filter" nil nil
         (
          filter-chordlist make-bounds-test make-width-test make-cardinality-test
          make-spacing-test make-register-test make-voicing-test and-tests or-tests
          ) nil)
        ("vector & sort" nil nil
         (
          vector-dotprod vector-angle incl-vec incl-vec-angle prog-vec
          prog-vec-angle sort+ sort+select sort-key_incl-vec-sum sort-key_incl-vec-angle
          sort-key_prog-vec-sum sort-key_prog-vec-angle sort-key_width mult-keys
          ) nil)
        ("utility" nil nil
         (
          mc->p p->mc p->pc parse-incl-classreps parse-prog-classreps
          flatten2chordlist
          ) nil)
        ))

;--------------------------------------------------
;filling packages
;--------------------------------------------------

(om::fill-library *subpackages-list1*)
