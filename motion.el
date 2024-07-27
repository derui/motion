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
;;     (cons start (point))))
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
          `(defun ,(intern (seq-concatenate 'string fname "-forward")) (operator)
             ,docstring
             (lambda (&rest arg)
               (interactive)
               (save-excursion
                 (when-let* ((region ,forward-body))
                   (funcall operator (car region) (cdr region))
                   )))
             )
          )
       ,(when backward-body
          `(defun ,(intern (seq-concatenate 'string fname "-backward")) (operator)
             ,docstring
             (lambda (&rest arg)
               (interactive)
               (save-excursion
                 (when-let* ((region ,backward-body))
                   (funcall operator (car region) (cdr region))
                   )))
             )
          )
       ,(when inner-body
          `(defun ,(intern (seq-concatenate 'string fname "-around-inner")) (operator)
             ,docstring
             (lambda (&rest arg)
               (interactive)
               (save-excursion
                 (when-let* ((region ,inner-body))
                   (funcall operator (car region) (cdr region))
                   )))
             ))
       ,(when outer-body
          `(defun ,(intern (seq-concatenate 'string fname "-around-outer")) (operator)
             ,docstring
             (lambda (&rest arg)
               (interactive)
               (save-excursion
                 (when-let* ((region ,outer-body))
                   (funcall operator (car region) (cdr region))
                   )))
             ))
       ))
  )

;;;###autoload
(defmacro motion-define-thing (name thing)
  "Define motion for `THING'.

This macro defines motion via `motion-define' with `NAME'.
Use `THING' in defined motion.
Allowed `THING' for this macro is same as `thing-at-point'.
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

;;;###autoload
(defmacro motion-define-pair (name pair &optional not-bound)
  "Define motion for `PAIR'

`PAIR' is cons cell (start char . end char). Each value of cell are
character to define pair.

Motions defined by this macro does not have forward/backward motion.

For example, Given pair to define motion for single quote should be
(?' . ?') .
Do not call operator from defined motions if not found any pair from
search area defined below..

Inner and outer motions define by this macro searchs nearest character
bound for `NOT-BOUND' argument. If `NOT-BOUND' is not passed, a motion searchs
forward only the line of the cursor.
If `NOT-BOUND' is passed with any symbol, defined motion searchs
forward to end of the buffer. This behavior may occur some performance issues when
a buffer is too large.
"
  (when-let* (pair
              (start-char (car pair))
              (end-char (cdr pair))
              (start-char-str (string start-char))
              (end-char-str (string end-char)))
    `(motion-define ,name ,(format "Motion for `PAIR' %s/%s" start-char-str end-char-str)
       :inner
       (when-let* ((start ,(if not-bound
                               `(search-forward ,start-char-str nil t)
                             `(search-forward ,start-char-str (pos-eol) t)))
                   (end ,(if not-bound
                             `(search-forward ,end-char-str nil t)
                           `(search-forward ,end-char-str (pos-eol) t)))
                   (end-fixed (and end
                                   (1- end))))
         '(start . end-fixed))
       :outer
       (when-let* ((start ,(if not-bound
                               `(search-forward ,start-char-str nil t)
                             `(search-forward ,start-char-str (pos-eol) t)))
                   (start-fixed (and start
                                     (1- start)))
                   (end ,(if not-bound
                             `(search-forward ,end-char-str nil t)
                           `(search-forward ,end-char-str (pos-eol) t))))
         '(start-fixed . end))))
  )

(provide 'motion)
