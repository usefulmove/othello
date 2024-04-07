;;; othello-test.el --- Unit tests for Othello functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: April 7, 2024
;; Version: 0.6.0
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/othello
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Othello unit tests
;;
;;  Source code: ~/repos/othello/src/othello.el
;;
;;; Code:

(load-file "~/repos/othello/src/othello.el") ; load Othello language


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun othello-test-compound (error-prelude)
  (when (not (zerop (- 204 (sum (map
                                  (\ (a) (* a a))
                                  (range (inc 8)))))))
    (error (concat error-prelude "error: compound test(s) failed")))
  (assert-equal
    (call (memoize 'inc) 8)
    9
    (error (concat error-prelude "error: compound test(s) failed")))
  (assert-equal
    (range 1 3)
    '(1 2)
    (error (concat error-prelude "error: compound test(s) failed")))
  (assert-equal
    (range 100 (inc 200) 20)
    '(100 120 140 160 180 200)
    (error (concat error-prelude "error: compound test(s) failed"))))


(defun othello-test-compound2 (error-prelude)
  (assert-equal
    (prod (filter 'oddp (map
                          (\ (a) (* a a a))
                          (range (dec 10)))))
    1157625
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (init '(3 1 2))
    '(3 1)
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (end '(3 1 2))
    2
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (allp 'evenp (map
                   (\ (a) (* 2 a))
                   (range (inc 31))))
    t
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (gcd 18 30 12)
    6
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (not (anyp 'ascii-numericp (list 46 47 58 59)))
    (allp 'ascii-numericp (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (flatten '(3 1 (2 1 2)))
    '(3 1 2 1 2)
    (concat error-prelude "error: compound2 test(s) failed")))


(defun othello-test-compound3 (error-prelude)
  (when (anyp 'evenp '(3 1 5 9 7))
    (error (concat error-prelude "error: compound3 test(s) failed")))
  (assert-equal
    (remove-duplicates '(8 1 2 8 5 4 0 8))
    '(8 1 2 5 4 0)
    (concat error-prelude "error: compound3 test(s) failed"))
  (assert-equal
   (containsp 0 '(3 1 2 0 5 4))
   t
   (concat error-prelude "error: compound3 test(s) failed"))
  (assert-equal
   (containsp 0 '(3 1 2 5 4))
   nil
   (concat error-prelude "error: compound3 test(s) failed")))


(defun othello-test-function-composition (error-prelude)
  (when (not-equal-p (thread 5
                      'sqrt
                      (\ (a) (- a 1))
                      (\ (a) (/ a 2)))
                    (call (pipe 'sqrt
                                (\ (a) (- a 1))
                                (\ (a) (/ a 2)))
                     5))
    (error (concat error-prelude "error: function composition (1) test(s) failed"))))


(defun othello-test-function-composition2 (error-prelude)
  (when (not= (thread 5
                'sqrt
                (\ (a) (- a 1))
                (\ (a) (/ a 2)))
              (call (compose (\ (a) (/ a 2))
                             (\ (a) (- a 1))
                             'sqrt)
               5))
    (error (concat error-prelude "error: function composition (2) test(s) failed"))))


(defun othello-test-string-join (error-prelude)
  (assert-equal
    (join '("3" "1" "2" "5" "4"))
    "31254"
    (concat error-prelude "error: string join test(s) failed"))
  (assert-equal
    (join '("3" "1" "2" "5" "4") ", ")
    "3, 1, 2, 5, 4"
    (concat error-prelude "error: string join test(s) failed"))
  (let ((s "desafortunadamente"))
    (assert-equal
      (thread s
        'string-to-list
        'reverse
        'join-chars)
      (reverse s)
      (concat error-prelude "error: string join test(s) failed"))))


(defun othello-test-curry (error-prelude)
  (letrec ((square (\ (a) (* a a)))
           (sum-squares (lambda (a b)
                          (sqrt (+ (call square a)
                                   (call square b))))))
    (assert-equal
      (call sum-squares 3 4)
      (call (call (curry2 sum-squares) 3) 4)
      (concat error-prelude "error: curry test(s) failed"))))


(defun othello-test-partial (error-prelude)
  (letrec ((square (\ (a) (* a a)))
           (sum-squares (lambda (a b)
                          (sqrt (+ (call square a)
                                   (call square b))))))
    (assert-equal
      (call sum-squares 3 4)
      (call (partial sum-squares 3) 4)
      (concat error-prelude "error: partial test(s) failed"))))


(defun othello-test-foldl (error-prelude)
  (assert-equal
    (foldl (lambda (acc a) (+ acc (* a a))) 0 (range (inc 8)))
    204
    (concat error-prelude "error: foldl test(s) failed"))
  (let ((input "this is a test"))
    (assert-equal
      (fold
        (lambda (acc a)
               (concat acc (join-chars (list a))))
        ""
        (string-to-list input))
      input
      (concat error-prelude "error: fold test(s) failed")))
  (let ((test-list '(3 1 2 5 4)))
    (assert-equal
      (foldl
        (lambda (acc a) (cons a acc))
        '()
        test-list)
      (reverse test-list)
      (concat error-prelude "error: foldl (2) test(s) failed"))))


(defun othello-test-foldr (error-prelude)
  (let ((test-list '(3 1 2 5 4)))
    (assert-equal
      (foldr
        (lambda (acc a) (cons a acc))
        '()
        test-list)
      test-list
      (concat error-prelude "error: foldr test(s) failed"))))


(defun othello-test-drop-take (error-prelude)
  (assert-equal
    (take 3 '(3 1 2 0 5 4))
    '(3 1 2)
    (concat error-prelude "error: take test(s) failed"))
  (assert-equal
    (takebut 2 '(3 1 2 0 5 4))
    '(3 1 2 0)
    (concat error-prelude "error: take test(s) failed"))
  (assert-equal
    (drop 3 '(3 1 2 0 5 4))
    '(0 5 4)
    (concat error-prelude "error: drop test(s) failed"))
  (assert-equal
    (dropbut 2 '(3 1 2 0 5 4))
    '(5 4)
    (concat error-prelude "error: drop test(s) failed")))


(defun othello-test-zip (error-prelude)
  (assert-equal
    (zip '(3 1 2 5 4)
         '(0 1 2 3))
    '((3 . 0) (1 . 1) (2 . 2) (5 . 3))
    (concat error-prelude "error: zip test(s) failed")))


(defun othello-test-enumerate-partition (error-prelude)
  (assert-equal
    (enumerate '(3 1 2 5 4))
    '((0 . 3) (1 . 1) (2 . 2) (3 . 5) (4 . 4))
    (concat error-prelude "error: enumerate test(s) failed"))
  (assert-equal
    (partition 'oddp '(8 1 2 0 3 5 4 6))
    '((5 3 1) (6 4 0 2 8))
    (concat error-prelude "error: enumerate test(s) failed")))


(defun othello-test-tally (error-prelude)
  (assert-equal
    (let ((s "As twilight cascaded upon the horizon, the iridescent hues of
              amaranthine skies caressed the gentle whispers of the zephyr,
              weaving an ephemeral symphony of love that intertwined the souls
              of all living beings in the tender embrace of nature's eternal
              harmony.")
          (get-count (lambda (key counts)
                       (cdr (assoc key counts)))))
      (call get-count ?e (tally (string-to-list s))))
    33
    (concat error-prelude "error: drop test(s) failed")))


(defun othello-test-begin (error-prelude)
  (assert-equal
    (let ((cnt 0))
      (begin (setq cnt (inc cnt))
          (setq cnt (inc cnt))
          (setq cnt (inc cnt)))
      cnt)
    3
    (concat error-prelude "error: begin test(s) failed")))


(defun othello-test-for-comprehension (error-prelude)
  (assert-equal
    (for-comp ((a (range (inc 8)))) (* a a a))
    '(0 1 8 27 64 125 216 343 512)
    (concat error-prelude "error: for comprehension test(s) failed"))
  (assert-equal
    (for-comp ((pair (enumerate '(3 1 2))))
      (let ((i (car pair))
            (a (cdr pair)))
        (* i (* a a))))
    '(0 1 8)
    (concat error-prelude "error: for comprehension test(s) failed"))
  (assert-equal
    (for-comp ((i (range 3))
               (j (range 3)))
      (cons i j))
    '((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2) (2 . 0) (2 . 1) (2 . 2))
    (concat error-prelude "error: for comprehension test(s) failed")))


(defun othello-test-equality (error-prelude)
  (assert-equal
    (not= 1 1.0 1) ; nil
    (eqp 1 1.0) ; nil
    (concat error-prelude "error: equality test(s) failed"))
  (assert-equal
    (not-eq-p 1 1.0) ; t
    (not-equal-p 1 1.0) ; t
    (concat error-prelude "error: equality test(s) failed"))
  (let ((a "eight")
        (b "eight"))
    (assert-equal
      (equalp a b) ; t
      (not-eq-p a b) ; t
      (concat error-prelude "error: equality test(s) failed")))
  (assert-equal
    (not= 1 1.0) ; nil
    (not-equal-p 1 1) ; nil
    (concat error-prelude "error: equality test(s) failed")))


(defun othello-test-chars (error-prelude)
  (assert-equal
    (char-to-int ?8)
    8
    (concat error-prelude "error: equality test(s) failed"))
  (assert-equal
    (char-to-ord ?8)
    56
    (concat error-prelude "error: equality test(s) failed")))

(defun othello-test-impure (error-prelude)
  (let ((n 0))
    (for-each
      (lambda (a)
        (setq n (+ n (* a a))))
      '(3 1 2 0 5 4))
    (assert-equal
      n
      55
      (concat error-prelude "error: impure test(s) failed")))
  (let ((n 0))
    (for ((a '(3 1 2 0 5 4)))
      (setq n (+ n (* a a))))
    (assert-equal
      n
      55
      (concat error-prelude "error: impure test(s) failed"))))


(defun othello-test-list-ref (error-prelude)
  (assert-equal
    (list-ref '(3 1 2 0 5) 2)
    2
    (concat error-prelude "error: list-ref test(s) failed"))
  (assert-equal
    (list-ref '((8 1 2)
                 (0 5 4)) 1 2)
    4
    (concat error-prelude "error: list-ref test(s) failed"))
  (assert-equal
    (list-ref '(8 1 2) 0)
    8
    (concat error-prelude "error: list-ref test(s) failed")))


(defun othello-test-logic (error-prelude)
  (assert-equal
   (truep t)
   t
   (concat error-prelude "error: truep test(s) failed"))
  (assert-equal
   (truep nil)
   nil
   (concat error-prelude "error: truep test(s) failed"))
  (assert-equal
   (emptyp '())
   t
   (concat error-prelude "error: emptyp test(s) failed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run unit tests

(defun othello-test-run-tests (&rest tests)
  (letrec ((prelude "othello-test ... ")
           (execute-tests (lambda (fns)
                            (cond ((nullp fns) nil)
                                  (else (call (car fns) prelude)
                                        (call execute-tests (cdr fns)))))))
    (message (concat prelude "running tests..."))
    (call execute-tests tests)
    (message (concat prelude "passed all tests"))))


(othello-test-run-tests
  'othello-test-compound
  'othello-test-compound2
  'othello-test-compound3
  'othello-test-function-composition
  'othello-test-function-composition2
  'othello-test-string-join
  'othello-test-curry
  'othello-test-partial
  'othello-test-foldl
  'othello-test-foldr
  'othello-test-drop-take
  'othello-test-zip
  'othello-test-enumerate-partition
  'othello-test-tally
  'othello-test-begin
  'othello-test-for-comprehension
  'othello-test-equality
  'othello-test-chars
  'othello-test-impure
  'othello-test-logic)



(provide 'othello-test)
;;; othello-test.el ends here
