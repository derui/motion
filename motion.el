;;; motion.el --- simple motion library -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: https://github.com/derui/motion
;; Version: 0.3.2
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

(defmacro motion--define-internal-motion (fname docstring body)
  "Return internal definition of motion."
  `(defun ,(intern fname) (operator &rest arguments)
     ,docstring
     (let ((after-hook (plist-get arguments :after))
           (body ',body))
       `(lambda (&rest arg)
          (interactive)
          (save-excursion
            (prog1
                (when-let* ((region ,body))
                  (,operator (car region) (cdr region)))
              (when ',after-hook
                (funcall ',after-hook))))))))

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
          (let ((fname (seq-concatenate 'string fname "-forward")))
            `(motion--define-internal-motion ,fname ,docstring ,forward-body)))
       ,(when backward-body
          (let ((fname (seq-concatenate 'string fname "-backward")))
            `(motion--define-internal-motion ,fname ,docstring ,backward-body)))
       ,(when inner-body
          (let ((fname (seq-concatenate 'string fname "-around-inner")))
            `(motion--define-internal-motion ,fname ,docstring ,inner-body)))
       ,(when outer-body
          (let ((fname (seq-concatenate 'string fname "-around-outer")))
            `(motion--define-internal-motion ,fname ,docstring ,outer-body)))
       ))
  )

;;;###autoload
(defmacro motion-define-thing (name thing)
  "Define motion for `THING'.

This macro defines motion via `motion-define' with `NAME'.
Use `THING' in defined motion.
Allowed `THING' for this macro is same as `thing-at-point'.
"
  `(motion-define ,(intern (symbol-name name)) "Motion for `THING'"
     :inner
     (when-let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (car thing) (cdr thing)))
     :outer
     (when-let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (car thing) (cdr thing)))
     :forward
     (when-let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (point) (cdr thing)))
     :backward
     (when-let ((thing (bounds-of-thing-at-point ,thing)))
       (cons (car thing) (point)))
     ))

(defmacro motion--get-balanced-region-of-pair (pair not-bound)
  "Generate code to get balanced region of pair.

This macro can not handle same character in pair"
  (unless (and (listp pair)
               (not (char-equal (caadr pair) (cdadr pair))))
    (error "pair must have difference characters"))

  (when-let* ((pair (cadr pair))
              (start-char (car pair))
              (end-char (cdr pair))
              (start-char-str (string start-char))
              (end-char-str (string end-char))
              (char-regex (regexp-opt (list start-char-str end-char-str))))
    `(let (stack end)
       (when-let* ((start ,(if not-bound
                               `(or(when (search-backward ,start-char-str nil t)
                                     (forward-char 1)
                                     (point))
                                   (search-forward ,start-char-str nil t))
                             `(or (when (search-backward ,start-char-str (pos-bol) t)
                                    (forward-char 1)
                                    (point))
                                  (search-forward ,start-char-str (pos-eol) t))))
                   (stack (push start stack)))
         (ignore-errors
           (while stack
             (let* ((searched-point (search-forward-regexp ,char-regex ,(if not-bound 'nil '(pos-eol))))
                    (char (match-string-no-properties 0)))
               (cond
                ((string= ,start-char-str char)
                 (push searched-point stack))
                ((string= ,end-char-str char)
                 (when (= 1 (seq-length stack))
                   (setq end searched-point))
                 (pop stack))))))
         (when end
           (cons (1- start) end))))))

;;;###autoload
(defmacro motion-define-pair (name pair &optional not-bound)
  "Define motion for `PAIR'

`PAIR' is cons cell (start char . end char). Each value of cell are
character to define pair.

Motions defined by this macro does not have forward/backward motion.

For example, Given pair to define motion for single quote should be '(?' . ?') .
Do not call operator from defined motions if not found any pair from
search area defined below..

Inner and outer motions define by this macro searchs nearest character
bound for `NOT-BOUND' argument. If `NOT-BOUND' is not passed, a motion searchs
forward only the line of the cursor.
If `NOT-BOUND' is passed with any symbol, defined motion searchs
forward to end of the buffer. This behavior may occur some performance issues when
a buffer is too large.
"
  (when-let* ((pair (cadr pair))
              (start-char (car pair))
              (end-char (cdr pair))
              (start-char-str (string start-char))
              (end-char-str (string end-char)))
    (if (string= start-char-str end-char-str)
        `(motion-define ,(intern (symbol-name name)) ,(format "Motion for `PAIR' %s/%s" start-char-str end-char-str)
           :inner
           (when-let* ((start ,(if not-bound
                                   `(or (when-let ((_ (search-backward ,start-char-str nil t)))
                                          (forward-char)
                                          (point))
                                        (search-forward ,start-char-str nil t))
                                 `(or (when-let ((_ (search-backward ,start-char-str (pos-bol) t)))
                                        (forward-char)
                                        (point))
                                      (search-forward ,start-char-str (pos-eol) t))))
                       (end ,(if not-bound
                                 `(search-forward ,end-char-str nil t)
                               `(search-forward ,end-char-str (pos-eol) t)))
                       (end-fixed (and end
                                       (1- end))))
             (cons start end-fixed))
           :outer
           (when-let* ((start ,(if not-bound
                                   `(or (when-let ((_ (search-backward ,start-char-str nil t)))
                                          (forward-char)
                                          (point))
                                        (search-forward ,start-char-str nil t))
                                 `(or (when-let ((_ (search-backward ,start-char-str (pos-bol) t)))
                                        (forward-char)
                                        (point))
                                      (search-forward ,start-char-str (pos-eol) t))))
                       (start-fixed (and start
                                         (1- start)))
                       (end ,(if not-bound
                                 `(search-forward ,end-char-str nil t)
                               `(search-forward ,end-char-str (pos-eol) t))))
             (cons start-fixed end)))
      `(motion-define ,(intern (symbol-name name)) ,(format "Motion for `PAIR' %s/%s" start-char-str end-char-str)
         :inner
         (when-let* ((region (motion--get-balanced-region-of-pair ',(cons start-char end-char) ,not-bound))
                     (start (car region))
                     (end (cdr region)))
           (cons (1+ start) (1- end)))
         :outer
         (when-let* ((region (motion--get-balanced-region-of-pair ',(cons start-char end-char) ,not-bound))
                     (start (car region))
                     (end (cdr region)))
           (cons start end)))))
  )

(provide 'motion)
;;; motion.el ends here
