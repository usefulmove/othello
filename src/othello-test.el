;;; othello-test.el --- Unit tests for Othello functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: April 27, 2024
;; Version: 0.9.21
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

(ert-deftest othello-test-compound ()
  (should (= (o-sum (o-map
                     (lambda (a) (* a a))
                     (o-range (o-inc 8))))
             204))
  (should (= (o-call (o-memoize 'o-inc) 8)
             9))
  (should (equal (o-range 1 3)
                 '(1 2)))
  (should (equal (o-range 100 (o-inc 200) 20)
                 '(100 120 140 160 180 200))))

(ert-deftest othello-test-compound2 ()
  (should (= (o-product (o-filter 'o-odd-p (o-map
                                            (lambda (a) (* a a a))
                                            (o-range (o-dec 10)))))
             1157625))
  (should (equal (o-init '(3 1 2))
                 '(3 1)))
  (should (= (o-last '(3 1 2))
             2))
  (should (eq (o-all-p 'o-even-p (o-map
                                  (lambda (a) (* 2 a))
                                  (o-range (o-inc 31))))
              t))
  (should (= (o-gcd 18 30 12)
             6))
  (should (eq (not (o-any-p 'o-ascii-numeric-p (list 46 47 58 59)))
              (o-all-p 'o-ascii-numeric-p (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
  (should (equal (o-flatten '(3 1 (2 1 2)))
                 '(3 1 2 1 2))))


(ert-deftest othello-test-compound3 ()
  (should-not (o-any-p 'o-even-p '(3 1 5 9 7)))
  (should (equal (o-remove-duplicates '(8 1 2 8 5 4 0 8))
                 '(8 1 2 5 4 0)))
  (should (eq (o-contains-p 0 '(3 1 2 0 5 4))
              t))
  (should (eq (o-contains-p 0 '(3 1 2 5 4))
              nil)))


(ert-deftest othello-test-function-composition ()
  (should (equal (o-thread
                  5
                  'sqrt
                  (lambda (a) (- a 1))
                  (lambda (a) (/ a 2)))
                 (o-call (o-pipe
                          'sqrt
                          (lambda (a) (- a 1))
                          (lambda (a) (/ a 2)))
                         5))))


(ert-deftest othello-test-function-composition2 ()
  (should (equal (o-thread
                  5
                  'sqrt
                  (lambda (a) (- a 1))
                  (lambda (a) (/ a 2)))
                 (o-call (o-compose (lambda (a) (/ a 2))
                                    (lambda (a) (- a 1))
                                    'sqrt)
                         5))))


(ert-deftest othello-test-string-o-join ()
  (should (equal (o-join '("3" "1" "2" "5" "4"))
                 "31254"))
  (should (equal (o-join '("3" "1" "2" "5" "4") ", ")
                 "3, 1, 2, 5, 4"))
  (let ((s "desafortunadamente"))
    (should (equal (o-thread
                    s
                    'string-to-list
                    'reverse
                    'o-join-chars)
                   (reverse s)))))


(ert-deftest othello-test-curry ()
  (letrec ((square (lambda (a) (* a a)))
           (o-sum-squares (lambda (a b)
                          (sqrt (+ (o-call square a)
                                   (o-call square b))))))
    (should (equal (o-call o-sum-squares 3 4)
                   (o-call (o-call (o-curry2 o-sum-squares) 3) 4)))))


(ert-deftest othello-test-partial ()
  (letrec ((square (lambda (a) (* a a)))
           (o-sum-squares (lambda (a b)
                          (sqrt (+ (o-call square a)
                                   (o-call square b))))))
    (should (equal (o-call o-sum-squares 3 4)
                   (o-call (o-partial o-sum-squares 3) 4)))))


(ert-deftest othello-test-fold-left ()
  (should (equal (o-fold-left (lambda (acc a) (+ acc (* a a))) 0 (o-range (o-inc 8)))
                 204))
  (let ((input "this is a test"))
    (should (equal (o-fold
                    (lambda (acc a)
                      (concat acc (o-join-chars (list a))))
                    ""
                    (string-to-list input))
                   input)))
  (let ((test-list '(3 1 2 5 4)))
    (should (equal (o-fold-left
                    (lambda (acc a) (cons a acc))
                    '()
                    test-list)
                   (reverse test-list)))))


(ert-deftest othello-test-fold-right ()
  (let ((test-list '(3 1 2 5 4)))
    (should (equal (o-fold-right
                    (lambda (acc a) (cons a acc))
                    '()
                    test-list)
                   test-list))))


(ert-deftest othello-test-drop-take ()
  (should (equal (o-take 3 '(3 1 2 0 5 4))
                 '(3 1 2)))
  (should (equal (o-takebut 2 '(3 1 2 0 5 4))
                 '(3 1 2 0)))
  (should (equal (o-drop 3 '(3 1 2 0 5 4))
                 '(0 5 4)))
  (should (equal (o-dropbut 2 '(3 1 2 0 5 4))
                 '(5 4))))


(ert-deftest othello-test-slice ()
  (should (equal (o-slice 1 4 '(3 1 2 5 4 0))
                 '(1 2 5))))


(ert-deftest othello-test-zip ()
  (should (equal
           (o-zip '(3 1 2 5 4)
                  '(0 1 2 3))
           '((3 0) (1 1) (2 2) (5 3))))
  (should (equal
           (o-zip-with-index (make-list 9 0))
           '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0))))
  (should (equal
           (o-enumerate (make-list 9 0))
           '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0)))))


(ert-deftest othello-test-zip-with ()
  (should (equal (o-zip-with
                  '+
                  '(3 1 2 5 4)
                  '(0 1 2 3))
                 '(3 2 4 8))))


(ert-deftest othello-test-enumerate-partition ()
  (should (equal 
           (o-enumerate '(3 1 2 5 4))
           '((0 3) (1 1) (2 2) (3 5) (4 4))))
  (should (equal 
           (o-partition 'o-odd-p '(8 1 2 0 3 5 4 6))
           '((5 3 1) (6 4 0 2 8)))))


(ert-deftest othello-test-count-elements ()
  (should (equal 
           (let ((s "As twilight cascaded upon the horizon, the iridescent hues of
                     amaranthine skies caressed the gentle whispers of the zephyr,
                     weaving an ephemeral symphony of love that intertwined the souls
                     of all living beings in the tender embrace of nature's eternal
                     harmony.")
                 (get-count (lambda (key counts)
                              (o-tail (assoc key counts)))))
             (o-call get-count ?e (o-count-elements (string-to-list s))))
           33)))


(ert-deftest othello-test-o-begin ()
  (should (equal (let ((cnt 0))
                   (o-begin (setq cnt (o-inc cnt))
                            (setq cnt (o-inc cnt))
                            (setq cnt (o-inc cnt)))
                   cnt)
                 3)))


(ert-deftest othello-test-for-comprehension ()
  (should (equal 
           (o-for-comp ((a (o-range (o-inc 8)))) (* a a a))
           '(0 1 8 27 64 125 216 343 512)))
  (should (equal 
           (o-for-comp ((pair (o-enumerate '(3 1 2))))
                       (let ((ind (car pair))
                             (a (cadr pair)))
                         (* ind (* a a))))
           '(0 1 8)))
  (should (equal 
           (o-for-comp ((i (o-range 3))
                        (j (o-range 3)))
                       (cons i j))
           '((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2) (2 . 0) (2 . 1) (2 . 2)))))


(ert-deftest othello-test-equality ()
  (should (equal 
           (o-not= 1 1.0 1) ; nil
           (o-eq-p 1 1.0))) ; nil
  (should (equal 
           (o-not-eq-p 1 1.0) ; t
           (o-not-equal-p 1 1.0))) ; t
  (let ((a "eight")
        (b "eight"))
    (should (equal 
             (o-equal-p a b) ; t
             (o-not-eq-p a b)))) ; t
  (should (equal 
           (o-not= 1 1.0) ; nil
           (o-not-equal-p 1 1)))) ; nil


(ert-deftest othello-test-chars ()
  (should (equal 
           (o-char-to-int ?8)
           8))
  (should (equal 
           (o-char-to-ord ?8)
           56)))


(ert-deftest othello-test-impure ()
  (let ((n 0))
    (o-for-each
      (lambda (a)
        (setq n (+ n (* a a))))
      '(3 1 2 0 5 4))
    (should (= n 55)))
  (let ((n 0))
    (o-for ((a '(3 1 2 0 5 4)))
      (setq n (+ n (* a a))))
    (should (= n 55))))


(ert-deftest othello-test-o-list-ref ()
  (should (equal
           (o-list-ref '(3 1 2 0 5) 2)
           2))
  (should (equal 
           (o-list-ref '((8 1 2)
                         (0 5 4)) 1 2)
           4))
  (should (equal 
           (o-list-ref '(8 1 2) 0)
           8)))


(ert-deftest othello-test-logic ()
  (should (o-true-p t))
  (should-not (o-true-p nil))
  (should (o-false-p nil))
  (should (o-empty-p '())))


(ert-deftest othello-test-adjacent-map ()
  (should (equal 
           (o-adjacent-map '* '(3 1 2 0 5 4))
           '(3 2 0 0 20))))


(ert-deftest othello-test-when ()
  (should (equal 
           (o-when t 'success)
           'success))
  (should-not (o-when nil 'success))
  (should-not (o-when-not t 'success))
  (should (equal 
           (o-when-not nil 'success)
           'success)))



(provide 'othello-test)
;;; othello-test.el ends here
