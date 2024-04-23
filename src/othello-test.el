;;; othello-test.el --- Unit tests for Othello functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: April 22, 2024
;; Version: 0.8.18
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/othello
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Othello library unit tests
;;
;;  Source code: ~/repos/othello/src/othello.el
;;
;;; Code:

; load Othello library
(load-file "~/repos/othello/src/othello.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun othello-test-compound (error-prelude)
  (when (not (o-zero-p (- 204 (o-sum (o-map
                                  (lambda (a) (* a a))
                                  (o-range (o-inc 8)))))))
    (error (concat error-prelude "error: compound (1) test(s) failed")))
  (o-assert-equal
    (o-call (o-memoize 'o-inc) 8)
    9
    (error (concat error-prelude "error: compound (1) test(s) failed")))
  (o-assert-equal
    (o-range 1 3)
    '(1 2)
    (error (concat error-prelude "error: compound (1) test(s) failed")))
  (o-assert-equal
    (o-range 100 (o-inc 200) 20)
    '(100 120 140 160 180 200)
    (error (concat error-prelude "error: compound (1) test(s) failed"))))


(defun othello-test-compound2 (error-prelude)
  (o-assert-equal
    (o-product (o-filter 'o-odd-p (o-map
                          (lambda (a) (* a a a))
                          (o-range (o-dec 10)))))
    1157625
    (concat error-prelude "error: compound (2) test(s) failed"))
  (o-assert-equal
    (o-init '(3 1 2))
    '(3 1)
    (concat error-prelude "error: compound (2) test(s) failed"))
  (o-assert-equal
    (o-last '(3 1 2))
    2
    (concat error-prelude "error: compound (2) test(s) failed"))
  (o-assert-equal
    (o-all-p 'o-even-p (o-map
                   (lambda (a) (* 2 a))
                   (o-range (o-inc 31))))
    t
    (concat error-prelude "error: compound (2) test(s) failed"))
  (o-assert-equal
   (o-gcd 18 30 12)
    6
    (concat error-prelude "error: compound (2) test(s) failed"))
  (o-assert-equal
    (not (o-any-p 'o-ascii-numeric-p (list 46 47 58 59)))
    (o-all-p 'o-ascii-numeric-p (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (concat error-prelude "error: compound (2) test(s) failed"))
  (o-assert-equal
    (o-flatten '(3 1 (2 1 2)))
    '(3 1 2 1 2)
    (concat error-prelude "error: compound (2) test(s) failed")))


(defun othello-test-compound3 (error-prelude)
  (when (o-any-p 'o-even-p '(3 1 5 9 7))
    (error (concat error-prelude "error: compound (3) test(s) failed")))
  (o-assert-equal
    (o-remove-duplicates '(8 1 2 8 5 4 0 8))
    '(8 1 2 5 4 0)
    (concat error-prelude "error: compound (3) test(s) failed"))
  (o-assert-equal
   (o-contains-p 0 '(3 1 2 0 5 4))
   t
   (concat error-prelude "error: compound (3) test(s) failed"))
  (o-assert-equal
   (o-contains-p 0 '(3 1 2 5 4))
   nil
   (concat error-prelude "error: compound (3) test(s) failed")))


(defun othello-test-function-composition (error-prelude)
  (when (o-not-equal-p (o-thread
                        5
                        'sqrt
                        (lambda (a) (- a 1))
                        (lambda (a) (/ a 2)))
    (o-call (o-pipe
             'sqrt
             (lambda (a) (- a 1))
             (lambda (a) (/ a 2)))
     5))
    (error (concat error-prelude "error: function composition (1) test(s) failed"))))


(defun othello-test-function-composition2 (error-prelude)
  (when (o-not= (o-thread 5
                'sqrt
                (lambda (a) (- a 1))
                (lambda (a) (/ a 2)))
              (o-call (o-compose (lambda (a) (/ a 2))
                             (lambda (a) (- a 1))
                             'sqrt)
               5))
    (error (concat error-prelude "error: function composition (2) test(s) failed"))))


(defun othello-test-string-o-join (error-prelude)
  (o-assert-equal
    (o-join '("3" "1" "2" "5" "4"))
    "31254"
    (concat error-prelude "error: string join test(s) failed"))
  (o-assert-equal
    (o-join '("3" "1" "2" "5" "4") ", ")
    "3, 1, 2, 5, 4"
    (concat error-prelude "error: string join test(s) failed"))
  (let ((s "desafortunadamente"))
    (o-assert-equal
      (o-thread s
        'string-to-list
        'reverse
        'o-join-chars)
      (reverse s)
      (concat error-prelude "error: string join test(s) failed"))))


(defun othello-test-curry (error-prelude)
  (letrec ((square (lambda (a) (* a a)))
           (o-sum-squares (lambda (a b)
                          (sqrt (+ (o-call square a)
                                   (o-call square b))))))
    (o-assert-equal
      (o-call o-sum-squares 3 4)
      (o-call (o-call (o-curry2 o-sum-squares) 3) 4)
      (concat error-prelude "error: curry test(s) failed"))))


(defun othello-test-partial (error-prelude)
  (letrec ((square (lambda (a) (* a a)))
           (o-sum-squares (lambda (a b)
                          (sqrt (+ (o-call square a)
                                   (o-call square b))))))
    (o-assert-equal
      (o-call o-sum-squares 3 4)
      (o-call (o-partial o-sum-squares 3) 4)
      (concat error-prelude "error: partial test(s) failed"))))


(defun othello-test-fold-left (error-prelude)
  (o-assert-equal
    (o-fold-left (lambda (acc a) (+ acc (* a a))) 0 (o-range (o-inc 8)))
    204
    (concat error-prelude "error: fold-left test(s) failed"))
  (let ((input "this is a test"))
    (o-assert-equal
      (fold
        (lambda (acc a)
               (concat acc (o-join-chars (list a))))
        ""
        (string-to-list input))
      input
      (concat error-prelude "error: fold test(s) failed")))
  (let ((test-list '(3 1 2 5 4)))
    (o-assert-equal
      (o-fold-left
        (lambda (acc a) (cons a acc))
        '()
        test-list)
      (reverse test-list)
      (concat error-prelude "error: fold-left (2) test(s) failed"))))


(defun othello-test-fold-right (error-prelude)
  (let ((test-list '(3 1 2 5 4)))
    (o-assert-equal
      (o-fold-right
        (lambda (acc a) (cons a acc))
        '()
        test-list)
      test-list
      (concat error-prelude "error: fold-right test(s) failed"))))


(defun othello-test-drop-take (error-prelude)
  (o-assert-equal
    (o-take 3 '(3 1 2 0 5 4))
    '(3 1 2)
    (concat error-prelude "error: take test(s) failed"))
  (o-assert-equal
    (o-takebut 2 '(3 1 2 0 5 4))
    '(3 1 2 0)
    (concat error-prelude "error: take test(s) failed"))
  (o-assert-equal
    (o-drop 3 '(3 1 2 0 5 4))
    '(0 5 4)
    (concat error-prelude "error: drop test(s) failed"))
  (o-assert-equal
    (o-dropbut 2 '(3 1 2 0 5 4))
    '(5 4)
    (concat error-prelude "error: drop test(s) failed")))


(defun othello-test-slice (error-prelude)
  (o-assert-equal
   (o-slice 1 4 '(3 1 2 5 4 0))
   '(1 2 5)
   (concat error-prelude "error: slice test(s) failed")))


(defun othello-test-zip (error-prelude)
  (o-assert-equal
   (o-zip '(3 1 2 5 4)
          '(0 1 2 3))
   '((3 0) (1 1) (2 2) (5 3))
   (concat error-prelude "error: zip test(s) failed"))
  (o-assert-equal
   (o-zip-with-index (make-list 9 0))
   '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0))
   (concat error-prelude "error: zip-with-index test(s) failed"))
  (o-assert-equal
   (o-enumerate (make-list 9 0))
   '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0))
   (concat error-prelude "error: enumerate test(s) failed")))


(defun othello-test-zip-with (error-prelude)
  (o-assert-equal
   (o-zip-with
    '+
    '(3 1 2 5 4)
    '(0 1 2 3))
   '(3 2 4 8)
   (concat error-prelude "error: zip test(s) failed")))


(defun othello-test-enumerate-partition (error-prelude)
  (o-assert-equal
    (o-enumerate '(3 1 2 5 4))
    '((0 3) (1 1) (2 2) (3 5) (4 4))
    (concat error-prelude "error: enumerate test(s) failed"))
  (o-assert-equal
    (o-partition 'o-odd-p '(8 1 2 0 3 5 4 6))
    '((5 3 1) (6 4 0 2 8))
    (concat error-prelude "error: enumerate test(s) failed")))


(defun othello-test-count-elements (error-prelude)
  (o-assert-equal
    (let ((s "As twilight cascaded upon the horizon, the iridescent hues of
              amaranthine skies caressed the gentle whispers of the zephyr,
              weaving an ephemeral symphony of love that intertwined the souls
              of all living beings in the tender embrace of nature's eternal
              harmony.")
          (get-count (lambda (key counts)
                       (o-tail (assoc key counts)))))
      (o-call get-count ?e (o-count-elements (string-to-list s))))
    33
    (concat error-prelude "error: drop test(s) failed")))


(defun othello-test-o-begin (error-prelude)
  (o-assert-equal
    (let ((cnt 0))
      (o-begin (setq cnt (o-inc cnt))
          (setq cnt (o-inc cnt))
          (setq cnt (o-inc cnt)))
      cnt)
    3
    (concat error-prelude "error: begin test(s) failed")))


(defun othello-test-for-comprehension (error-prelude)
  (o-assert-equal
    (o-for-comp ((a (o-range (o-inc 8)))) (* a a a))
    '(0 1 8 27 64 125 216 343 512)
    (concat error-prelude "error: for comprehension test(s) failed"))
  (o-assert-equal
    (o-for-comp ((pair (o-enumerate '(3 1 2))))
      (let ((ind (car pair))
            (a (cadr pair)))
        (* ind (* a a))))
    '(0 1 8)
    (concat error-prelude "error: for comprehension test(s) failed"))
  (o-assert-equal
    (o-for-comp ((i (o-range 3))
                 (j (o-range 3)))
      (cons i j))
    '((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2) (2 . 0) (2 . 1) (2 . 2))
    (concat error-prelude "error: for comprehension test(s) failed")))


(defun othello-test-equality (error-prelude)
  (o-assert-equal
    (o-not= 1 1.0 1) ; nil
    (o-eq-p 1 1.0) ; nil
    (concat error-prelude "error: equality test(s) failed"))
  (o-assert-equal
    (o-not-eq-p 1 1.0) ; t
    (o-not-equal-p 1 1.0) ; t
    (concat error-prelude "error: equality test(s) failed"))
  (let ((a "eight")
        (b "eight"))
    (o-assert-equal
      (o-equal-p a b) ; t
      (o-not-eq-p a b) ; t
      (concat error-prelude "error: equality test(s) failed")))
  (o-assert-equal
    (o-not= 1 1.0) ; nil
    (o-not-equal-p 1 1) ; nil
    (concat error-prelude "error: equality test(s) failed")))


(defun othello-test-chars (error-prelude)
  (o-assert-equal
    (o-char-to-int ?8)
    8
    (concat error-prelude "error: equality test(s) failed"))
  (o-assert-equal
    (o-char-to-ord ?8)
    56
    (concat error-prelude "error: equality test(s) failed")))

(defun othello-test-impure (error-prelude)
  (let ((n 0))
    (o-for-each
      (lambda (a)
        (setq n (+ n (* a a))))
      '(3 1 2 0 5 4))
    (o-assert-equal
      n
      55
      (concat error-prelude "error: impure test(s) failed")))
  (let ((n 0))
    (o-for ((a '(3 1 2 0 5 4)))
      (setq n (+ n (* a a))))
    (o-assert-equal
      n
      55
      (concat error-prelude "error: impure test(s) failed"))))


(defun othello-test-o-list-ref (error-prelude)
  (o-assert-equal
    (o-list-ref '(3 1 2 0 5) 2)
    2
    (concat error-prelude "error: list-ref test(s) failed"))
  (o-assert-equal
    (o-list-ref '((8 1 2)
                 (0 5 4)) 1 2)
    4
    (concat error-prelude "error: list-ref test(s) failed"))
  (o-assert-equal
    (o-list-ref '(8 1 2) 0)
    8
    (concat error-prelude "error: list-ref test(s) failed")))


(defun othello-test-logic (error-prelude)
  (o-assert-equal
   (o-true-p t)
   t
   (concat error-prelude "error: true-p test(s) failed"))
  (o-assert-equal
   (o-true-p nil)
   nil
   (concat error-prelude "error: true-p test(s) failed"))
  (o-assert-equal
   (o-false-p nil)
   t
   (concat error-prelude "error: false-p test(s) failed"))
  (o-assert-equal
   (o-empty-p '())
   t
   (concat error-prelude "error: empty-p test(s) failed")))

(defun othello-test-adjacent-map (error-prelude)
  (o-assert-equal
   (o-adjacent-map '* '(3 1 2 0 5 4))
   '(3 2 0 0 20)
   (concat error-prelude "error: adjacent-map test(s) failed")))


(defun othello-test-when (error-prelude)
  (o-assert-equal
   (o-when t 'success)
   'success
   (concat error-prelude "error: when test(s) failed"))
  (o-assert-equal
   (o-when nil 'success)
   nil
   (concat error-prelude "error: when test(s) failed"))
  (o-assert-equal
   (o-when-not t 'success)
   nil
   (concat error-prelude "error: when test(s) failed"))
  (o-assert-equal
   (o-when-not nil 'success)
   'success
   (concat error-prelude "error: when test(s) failed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run unit tests

(defun othello-test-run-tests (&rest tests)
  (letrec ((prelude "othello-test ... ")
           (execute-tests (lambda (fns)
                            (cond ((o-null-p fns) nil)
                                  (o-else (o-call (car fns) prelude)
                                          (o-call execute-tests (cdr fns)))))))
    (message (concat prelude "running tests..."))
    (o-call execute-tests tests)
    (message (concat prelude "passed all tests"))))


(othello-test-run-tests
  'othello-test-compound
  'othello-test-compound2
  'othello-test-compound3
  'othello-test-function-composition
  'othello-test-function-composition2
  'othello-test-string-o-join
  'othello-test-curry
  'othello-test-partial
  'othello-test-fold-left
  'othello-test-fold-right
  'othello-test-drop-take
  'othello-test-slice
  'othello-test-zip
  'othello-test-zip-with
  'othello-test-enumerate-partition
  'othello-test-count-elements
  'othello-test-o-begin
  'othello-test-for-comprehension
  'othello-test-equality
  'othello-test-chars
  'othello-test-impure
  'othello-test-logic
  'othello-test-adjacent-map
  'othello-test-when)



(provide 'othello-test)
;;; othello-test.el ends here
