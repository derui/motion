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
  (motion-define motion-test1
      "test"
    :forward
    '(1 . 3))
  (let* ((ret (motion-test1-forward (lambda (s e)
                                      (should (= 1 s))
                                      (should (= 3 e))
                                      (+ s e))
                                    )))
    (should (= ret 4)))
  (should (not (fboundp 'motion-test1-backward)))
  (should (not (fboundp 'motion-test1-inner)))
  (should (not (fboundp 'motion-test1-outer)))
  )

(ert-deftest save-excursion-on-motion ()
  (motion-define motion-test1
      "test"
    :forward
    (progn 
      (forward-char 2)
      (cons (point) (+ (point) 3))))
  (with-temp-buffer
    (insert "foobarbaz")
    (goto-char (point-min))

    (motion-test1-forward (lambda (s e)
                            (should (= 3 s))
                            (should (= 6 e))))
    (should (= 1 (point)))))

(ert-deftest around-motion ()
  (motion-define motion-test1
      "test"
    :inner
    (let ((thing (bounds-of-thing-at-point 'word)))
      (and thing
           (cons (car thing) (cdr thing))))
    :outer
    (let ((thing (bounds-of-thing-at-point 'word)))
      (and thing
           (cons (car thing) (cdr thing))))
    :forward
    (let ((s (point)))
      (forward-word)
      (cons s (point))))
  
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 2)

    (motion-test1-forward (lambda (s e)
                            (should (= 2 s))
                            (should (= 4 e))))
    (motion-test1-around-inner (lambda (s e)
                                 (should (= 1 s))
                                 (should (= 4 e))))
    (motion-test1-around-outer (lambda (s e)
                                 (should (= 1 s))
                                 (should (= 4 e))))
    ))

(ert-deftest do-not-apply-operator-if-motion-return-nil ()
  (motion-define motion-test1
      "test"
    :inner
    (progn nil)
    )
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 2)

    (let (called)
      (motion-test1-around-inner (lambda (s e)
                                   (setq called t)))
      (should (equal called nil)))
    ))

(ert-deftest motion-for-thing ()
  (motion-define-thing motion-thing 'word)
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 2)

    (motion-thing-forward (lambda (s e)
                            (should (= 2 s))
                            (should (= 4 e))))
    (motion-thing-backward (lambda (s e)
                             (should (= 1 s))
                             (should (= 2 e))))
    (motion-thing-around-inner (lambda (s e)
                                 (should (= 1 s))
                                 (should (= 4 e))))
    (motion-thing-around-outer (lambda (s e)
                                 (should (= 1 s))
                                 (should (= 4 e))))
    ))

(ert-deftest motion-for-pair ()
  (motion-define-pair motion-pair (?' . ?'))
  (with-temp-buffer
    (insert "'ignored' skip 'should be'")
    (search-forward "skip" nil t)

    (motion-pair-around-inner (lambda (s e)
                                (should (string= (buffer-substring s e) "should be"))))
    (motion-pair-around-outer (lambda (s e)
                                (should (string= (buffer-substring s e) "'should be'"))))
    ))

(ert-deftest motion-for-pair-with-bound ()
  (motion-define-pair motion-pair (?` . ?') t)
  (with-temp-buffer
    (insert "'ignored' skip
 `should be'")
    (search-forward "skip" nil t)

    (motion-pair-around-inner (lambda (s e)
                                (should (string= (buffer-substring s e) "should be"))))
    (motion-pair-around-outer (lambda (s e)
                                (should (string= (buffer-substring s e) "'should be'"))))
    ))

(ert-deftest motion-for-pair-without-bound ()
  (motion-define-pair motion-pair (?` . ?') t)
  (with-temp-buffer
    (insert "'ignored' skip
 `should be'")
    (search-forward "skip" nil t)

    (motion-pair-around-inner (lambda (s e)
                                (ert-fail "should not call this operator")))
    (motion-pair-around-outer (lambda (s e)
                                (ert-fail "should not call this operator")))
    ))

(ert-deftest motion-pair-withtou-bound-contains-newline ()
  (motion-define-pair motion-pair (?' . ?') t)
  (with-temp-buffer
    (insert "'ignored' skip 'should
be'")
    (search-forward "skip" nil t)

    (motion-pair-around-inner (lambda (s e)
                                (ert-fail "should not call this operator")))
    (motion-pair-around-outer (lambda (s e)
                                (ert-fail "should not call this operator")))
    ))

(ert t)
