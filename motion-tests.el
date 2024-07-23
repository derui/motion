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

(ert-deftest alphabet-to-kana ()
  (should (equal "lx" (chokan--roman-to-hiragana "lx")))
  (should (equal "l" (chokan--roman-to-hiragana "l")))
  (should (equal "し" (chokan--roman-to-hiragana "si")))
  (should (equal "しじ" (chokan--roman-to-hiragana "sizi")))
  (should (equal "してん" (chokan--roman-to-hiragana "sitenn")))
  (should (equal "z"
                 (chokan--roman-to-hiragana "z")))
  (should (equal "あ"
                 (chokan--roman-to-hiragana "a")))
  (should (equal "qい"
                 (chokan--roman-to-hiragana "qi")))
  )

(ert-deftest sokuon-to-kana ()
  (should (equal "っt"
                 (chokan--roman-to-hiragana "tt")))
  (should (equal "っす"
                 (chokan--roman-to-hiragana "ssu")))
  (should (equal "っった"
                 (chokan--roman-to-hiragana "ttta")))
  (should (equal "かったら"
                 (chokan--roman-to-hiragana "kattara")))
  )

(ert-deftest kana-and-alphabet ()
  (should (equal "pろgらm"
                 (chokan--roman-to-hiragana "pろgらm")))
  (should (equal "とちゅう"
                 (chokan--roman-to-hiragana "とtyuu")))
  )

(ert-deftest hira-to-kata ()
  (should (equal "カ" (chokan--roman-hira-to-kata "か")))
  (should (equal "ッt" (chokan--roman-hira-to-kata "っt")))
  (should (equal "t" (chokan--roman-hira-to-kata "t")))
  (should (equal "シュ" (chokan--roman-hira-to-kata "しゅ")))
  (let* ((prop (propertize "t" :test-prop t))
         (str (seq-concatenate 'string "っ" prop)))
    (should (equal "ッt" (chokan--roman-hira-to-kata str)))
    (should (equal t (get-text-property 1 :test-prop (chokan--roman-hira-to-kata str))))
    )
  )

(ert-deftest en-symbol-to-ja-symbol ()
  (should (equal "。" (chokan--symbol-convert-to-ja ".")))
  (should (equal "、" (chokan--symbol-convert-to-ja ",")))
  (should (equal "ー" (chokan--symbol-convert-to-ja "-"))))
(ert t)
