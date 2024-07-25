;;; motion.el -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "29.1"))
;; Keywords: editing

;;; Commentary:
;;
;; Motion provides some macros to define motion - that defined as command
;; to get region from current point.
;;
;; You can define new motion with `motion-define' macro.
;;
;; (motion-define my:new-motion
;;   "motion of word"
;;   :forward
;;   (let* ((start (point)))
;;     (forwardh-word)
;;     (list start . (point))))
;;
;; You can use `my:new-motion-forward' as function to run operator with region
;; from current point to next word.
;;

;;: Customization:

;;; Code:

(defgroup motion nil
  "motion customization group"
  :prefix "motion-")

;;;###autoload
(defmacro motion-define (name docstring &rest definitions)
  "Define motion wity definition.

`definitions' as plist to define each motion.

`:forward' defines motion of forward with `-forward' suffix.
`:backward' defines motion of backward with `-backward' suffix.
`:inner' defines motion for inner of around with `-around-inner' suffix.
`:outer' defines motion for outer of around with `-around-outer' suffix.

All of functions that are defined in `definitions' exports global
with `NAME' prefix.
All definitions in `definitions' MUST returns cons (start . end) or nil.
If nil returns from a definition, do not apply operator that pass by
generated function.
"
  (declare (indent 2))
  (let ((inner-body (plist-get definitions :inner))
        (outer-body (plist-get definitions :outer))
        (forward-body (plist-get definitions :forward))
        (backward-body (plist-get definitions :backward))
        (fname (symbol-name name)))
    `(progn
       ,(when forward-body
          `(defun ,(intern (seq-concatenate 'string fname "-forward")) (operator &rest args)
             ,docstring
             (save-excursion
               (when-let* ((region ,forward-body))
                 (funcall operator (car region) (cdr region))
                 ))
             )
          )
       ,(when backward-body
          `(defun ,(intern (seq-concatenate 'string fname "-backward")) (operator &rest args)
             ,docstring
             (save-excursion
               (when-let* ((region ,backward-body))
                 (funcall operator (car region) (cdr region))
                 ))
             )
          )
       ,(when inner-body
          `(defun ,(intern (seq-concatenate 'string fname "-around-inner")) (operator &rest args)
             ,docstring
             (save-excursion
               (when-let* ((region ,inner-body))
                 (funcall operator (car region) (cdr region))
                 ))
             ))
       ,(when outer-body
          `(defun ,(intern (seq-concatenate 'string fname "-around-outer")) (operator &rest args)
             ,docstring
             (save-excursion
               (when-let* ((region ,outer-body))
                 (funcall operator (car region) (cdr region))
                 ))
             ))
       ))
  )

;;;###autoload
(defmacro motion-define-thing (name thing)
  "Define motion for `THING'.

This macro defines motion via `motion-define' with `NAME'.
Use `THING' in defined motion.
"
  `(motion-define ,name "Motion for `THING'"
     :inner
     (let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (car thing) (cdr thing)))
     :outer
     (let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (car thing) (cdr thing)))
     :forward
     (let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (point) (cdr thing)))
     :backward
     (let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (car thing) (point)))
     ))

(provide 'motion)
