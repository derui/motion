;;; motion-tests.el --- tests for motion -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: editing

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'motion)

;; define tests

(ert-deftest define-simple-motion ()
  "define simplest motion"
  (motion-define motion-test1 ()
                 "test" '(1 3))
  (let* ((ret (motion-test1 (lambda (s e)
                              (should (= 1 s))
                              (should (= 3 e))
                              (+ s e))
                            )))
    (should (= ret 4))))

(ert-deftest save-excursion-on-motion ()
  (motion-define motion-test1 ()
                 "test"
                 (forward-char 2)
                 (list (point) (+ (point) 3)))
  (with-temp-buffer
    (insert "foobarbaz")
    (goto-char (point-min))

    (motion-test1 (lambda (s e)
                    (should (= 3 s))
                    (should (= 6 e))))
    (should (= 1 (point)))))

(ert-deftest around-motion ()
  (motion-define motion-test1 (:around t)
                 "test"
                 (cond
                  (motion--mode
                   (let ((thing (bounds-of-thing-at-point 'word)))
                     (and thing
                          (list (car thing) (cdr thing)))))
                  (t
                   (let ((s (point)))
                     (forward-word)
                     (list s (point)))
                   ))
                 )
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 2)

    (motion-test1 (lambda (s e)
                    (should (= 2 s))
                    (should (= 4 e))))
    (motion-test1-inner (lambda (s e)
                          (should (= 1 s))
                          (should (= 4 e))))
    (motion-test1-outer (lambda (s e)
                          (should (= 1 s))
                          (should (= 4 e))))
    ))

(ert-deftest do-not-apply-operator-if-motion-return-nil ()
  (motion-define motion-test1 (:around t)
                 "test"
                 nil
                 )
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 2)

    (let (called)
      (motion-test1 (lambda (s e)
                      (setq called t)))
      (should (equal called nil)))
    ))

(ert t)
