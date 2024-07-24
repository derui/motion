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
;; (motion-define my:new-motion ()
;;                "doc string for this motion"
;;                (let* ((start (point)))
;;                  (forward-word)
;;                  (start (point))
;;                  ))
;;
;; You can use `my:new-motion' as function to run operator with region
;; from current point to next word.
;; Definition of motion by macro `motion-define' has some utility variable in macro.
;;
;; * `motion--mode' mode of current motion if defined motion give `around' argument
;;   `motion--mode' holds `inner' ,`outer' or nil.

;;: Customization:

;;; Code:
(defgroup motion nil
  "motion customization group"
  :prefix "motion-")

;;;###autoload
(defmacro motion-define (name arguments docstring &rest body)
  "Define motion wity definition.

`arguments' holds some parameter list. Give `:around' with t to define
inner/outer function to get inner/outer region around something.
"
  (declare (indent 0))
  (let ((make-around-p (plist-get arguments :around))
        (fname (symbol-name name)))
    `(progn
       (defun ,(intern fname) (operator &rest args)
         ,docstring
         (save-excursion
           (let ((motion--mode (or (plist-get args :mode)
                                   nil)))
             (when-let* ((region (progn ,@body)))
               (apply operator region)
               )))
         )
       ,(when make-around-p
          `(defun ,(intern (seq-concatenate 'string fname "-inner")) (operator)
             ,docstring
             (,(intern fname) operator :mode 'inner)))
       ,(when make-around-p
          `(defun ,(intern (seq-concatenate 'string fname "-outer")) (operator)
             ,docstring
             (,(intern fname) operator :mode 'outer)))
       ))
  )

(provide 'motion)
