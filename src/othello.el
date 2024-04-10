;;; othello.el --- Othello functional programming library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: April 9, 2024
;; Version: 0.6.5
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/othello
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Othello functional programming library
;;
;;  Unit tests: ~/repos/othello/src/othello-test.el
;;
;;; Code:

(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros


;; o-not= :: T -> U -> V -> ... -> boolean
(defmacro o-not= (&rest args)
  "Test that objects are not numerically equal."
  `(not (= ,@args)))


;; o-equalp :: T -> U -> boolean
(defmacro o-equalp (a b)
  "Test that objects A and B have equal components."
  `(equal ,a ,b))


;; o-not-equal-p :: T -> U -> boolean
(defmacro o-not-equal-p (a b)
  "Test that objects A and B do not have equal components."
  `(not (equal ,a ,b)))


;; o-eqp :: T -> U -> boolean
(defmacro o-eqp (a b)
  "Test that objects A and B are the same object."
  `(eq ,a ,b))


;; o-not-eq-p :: T -> U -> boolean
(defmacro o-not-eq-p (a b)
  "Test that objects A and B are not the same object."
  `(not (eq ,a ,b)))


;; o-truep :: T -> boolean
(defmacro o-truep (a)
  "Test that object A is true."
  `(not (null ,a)))


;; o-nullp :: T -> boolean
(defmacro o-nullp (a)
  "Test that object A is not null."
  `(null ,a))

;; o-containsp :: T -> [T] -> boolean
(defmacro o-containsp (a lst)
  `(o-truep (member ,a ,lst)))


;; o-emptyp :: [T] -> boolean
(defmacro o-emptyp (a)
  `(null ,a))


;; o-call
(defmacro o-call (f &rest args)
  `(funcall ,f ,@args))


;; o-assert-equal :: sexp -> sexp -> string -> nil (IMPURE)
(defmacro o-assert-equal (exp1 exp2 error-msg)
  `(when (not (equal ,exp1 ,exp2))
  (error ,error-msg)))


;; o-map :: (T -> U) -> [T] -> [U]
(defmacro o-map (f lst)
  `(mapcar ,f ,lst))


;; o-filter :: (T -> boolean) -> [T] -> [T]
(defmacro o-filter (f lst)
  `(cl-remove-if-not ,f ,lst))


;; o-flatten :: [[T]] -> [T]
(fset 'o-flatten '-flatten)


;; o-begin
(defmacro o-begin (&rest forms)
  "Evaluate body FORMS sequentially and return value of the last one."
  `(let () ,@forms))


;; o-for-comp
(defmacro o-for-comp (bindings &rest body)
  "Comprehension. Bind variables in BINDINGS and iterate BODY over binding
permutations to generate list of mapped results."
  (let ((current-binding (car bindings))
        (remaining-bindings (cdr bindings)))
    (if (null remaining-bindings)
        `(mapcar (lambda ,(list (car current-binding)) ,@body) ,(cadr current-binding))
      `(cl-mapcan
         (lambda ,(list (car current-binding))
           (o-for-comp ,remaining-bindings ,@body))
         ,(cadr current-binding)))))


;; o-for
(defmacro o-for (bindings &rest body)
  "Execute BODY for every permutation of BINDINGS."
  (let ((current-binding (car bindings))
        (remaining-bindings (cdr bindings)))
    (if (null remaining-bindings)
        `(dolist (,(car current-binding) ,(cadr current-binding))
           ,@body)
      `(dolist (,(car current-binding) ,(cadr current-binding))
         (o-for ,remaining-bindings ,@body)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; o-fold-left :: (U -> T -> U) -> U -> [T] -> U
(defun o-fold-left (f acc lst)
  "Fold (reduce) list (LST) using applied function F starting with initial value
ACC for the accumulator. Fold right (o-fold-right) works in the opposite
direction through the list compared with fold left (o-fold-left)."
  (cond ((null lst) acc)
        (t (o-fold-left f (funcall f acc (car lst)) (cdr lst)))))


;; fold :: (U -> T -> U) -> U -> [T] -> U
(fset 'fold 'o-fold-left)


;; o-fold-right :: (U -> T -> U) -> U -> [T] -> U
(defun o-fold-right (f acc lst)
  "Fold (reduce) list (LST) using applied function F starting with initial value
ACC for the accumulator. Fold right (o-fold-right) works in the opposite
direction through the list compared with fold left (o-fold-left)."
  (o-fold-left f acc (reverse lst)))


;; o-partial :: (... -> T -> U) -> [...] -> (T -> U)
(defun o-partial (&rest args)
  "Return unary function when passed an n-ary function and (- n 1) arguments."
  (let ((f (car args))
        (fargs (cdr args)))
    (lambda (a)
      (apply f (append fargs (list a))))))


;; o-thread :: T -> [(T -> T)] -> T
(defun o-thread (seed &rest fns)
  "Thread a SEED value through the function defined by the composition of the
list of functions (FNS). This higher-order function can simplify (and make more
expressive) deeply nested compositional patterns."
  (cond ((null fns) seed)
        (t (apply 'o-thread (cons (funcall (car fns) seed)
                                (cdr fns))))))


;; o-compose :: [(T -> T)] -> (T -> T)
(defun o-compose (&rest fns)
  "Create composed function constructed of function arguments (FNS)."
  (cond ((null fns) 'identity)
        (t (let ((last-fn (car fns))
                 (rest-fn (apply 'o-compose (cdr fns))))
             (lambda (seed)
             (funcall last-fn (funcall rest-fn seed)))))))


;; o-pipe :: [(T -> T)] -> (T -> T)
(defun o-pipe (&rest fns)
  "Create composed function constructed of function arguments (FNS). The order
of function application is reversed from the o-compose function."
  (apply 'o-compose (reverse fns)))


;; o-curry2 :: (T -> U -> V) -> (T -> (U -> V))
(defun o-curry2 (f)
  "Return curried binary function F."
  (lambda (a)
    (lambda (b) (funcall f a b))))


;; o-range :: number -> (optional) number -> [number]
(defun o-range (&rest args)
  "Generate a list of values from FROM (inclusive) to TO (non-inclusive)."
  (let ((from (if (= 1 (length args))
                  0
                (car args)))
        (to (if (= 1 (length args))
                (car args)
              (cadr args)))
        (step (if (= 3 (length args))
                  (caddr args)
                1)))
    (cond ((>= from to) '())
          (t (cons from (o-range (+ step from) to step))))))


;; o-for-each :: (T -> ?) -> [T] -> nil (IMPURE)
(defun o-for-each (f lst)
  "Execute function F on each of the elements of list (LST)."
  (unless (null lst)
    (funcall f (car lst))
    (o-for-each f (cdr lst))))


;; o-inc :: number -> number
(defun o-inc (n)
  "Increment number N."
  (+ 1 n))


;; o-dec :: number -> number
(defun o-dec (n)
  "Decrement number N."
  (- n 1))


;; o-evenp :: number -> boolean
(defun o-evenp (n)
  "Is N even?"
  (= 0 (mod n 2)))


;; o-oddp :: number -> boolean
(defun o-oddp (n)
  "Is N odd?"
  (= 1 (mod n 2)))


;; o-zerop :: number -> boolean
(defun o-zerop (n)
  "Is N equal to zero?"
  (= 0 n))


;; o-ascii-numeric-p :: char -> boolean
(defun o-ascii-numeric-p (c)
  "Check is C a valid ascii numeric character?"
  (and (>= c ?0) (<= c ?9)))


;; o-sum :: [T] -> T
(defun o-sum (lst)
  "Sum elements of list (LST)."
  (apply '+ lst))


;; o-product :: [T] -> T
(defun o-product (lst)
  "Calculate the product of elements of list (LST)."
  (apply '* lst))


;; o-allp :: (T -> boolean) -> [T] -> boolean
(defun o-allp (f lst)
  "Check that function applied to all values in the list returns true."
  (cond ((null lst) t)
        ((not (funcall f (car lst))) nil)
        (t (o-allp f (cdr lst)))))


;; o-anyp :: (T -> boolean) -> [T] -> boolean
(defun o-anyp (f lst)
  "Check that function (F) applied to at least one value in the
list (LST) returns true."
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (o-anyp f (cdr lst)))))


;; o-init :: [T] -> [T]
(defun o-init (lst)
  "Return all elements of list (LST) except first."
  (reverse (cdr (reverse lst))))


;; o-last :: [T] -> [T]
(defun o-last (lst)
  "Return the last element of the list (LST)."
  (car (reverse lst)))


;; o-join-chars :: [char] -> string
(defun o-join-chars (chars)
  "O-Join the elements of list of characters (CHARS) into a string."
  (apply 'string chars))


;; o-char-to-int :: char -> integer
(defun o-char-to-int (c)
  "Convert numeric character (C) to its numeric equivalent. Return -1 if
character does not represent an integer value."
  (if (member c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (- c ?0)
    -1))


;; o-char-to-ord :: char -> integer
(defun o-char-to-ord (c)
  "Convert character (C) to its ordinal value."
  c)


;; o-gcd :: int -> int -> ... -> int (n-ary)
(defun o-gcd (&rest args)
  "Calculate the greatest common denominator of number arguments (ARGS)."
  (cl-labels ((o-gcd (a b)
                  (cond ((= 0 b) a)
                          (t (o-gcd b (mod a b))))))
    (cond ((= 2 (length args)) (o-gcd (car args)
                                    (cadr args)))
          (t (apply 'o-gcd (cons (o-gcd (car args)
                                    (cadr args))
                               (cddr args)))))))


;; o-take :: int -> [T] -> [T]
(defun o-take (n lst)
  "Take first N elements from list (LST)."
  (cond ((null lst) '())
        ((= 0 n) '())
        (t (cons (car lst)
                 (o-take (- n 1) (cdr lst))))))


;; o-takebut :: int -> [T] -> [T]
(defun o-takebut (n lst)
  "Take all but last N elements from list (LST)."
  (o-take (- (length lst) n) lst))


;; o-drop :: int -> [T] -> [T]
(defun o-drop (n lst)
  "Drop first N elements from list (LST)."
  (cond ((null lst) '())
        ((= 0 n) lst)
        (t (o-drop (- n 1) (cdr lst)))))


;; o-dropbut :: int -> [T] -> [T]
(defun o-dropbut (n lst)
  "Drop all but last N elements from list (LST)."
  (o-drop (- (length lst) n) lst))


;; o-slice :: [T] -> int -> int -> [T]
(defun o-slice (start end lst)
  (o-drop start (o-take end lst)))


;; o-zip :: [T] -> [U] -> [[T U]]
(defun o-zip (lst1 lst2)
  "Zip two lists (LST1) and (LST2) together and return an association list in
which the first element comes from LST1 and the second element comes from LST2.
The resulting zipped association list will have the same length as the shortest
of the two provided lists."
  (cond ((or (null lst1)
             (null lst2)) '())
        (t (cons (cons (car lst1)
                       (list (car lst2)))
                 (o-zip (cdr lst1)
                        (cdr lst2))))))


;; o-zip-with-index :: [T]  ->  [[int T]]
(defun o-zip-with-index (lst)
  (o-zip (o-range (length lst)) lst))


;; o-enumerate :: [T] -> [[integer . T]]
(defun o-enumerate (lst)
  "Enumerate the list (LST) by returning an association list whose elements are
the element index (0-based) and the element itself."
  (o-zip (o-range (length lst))
       lst))


;; o-partition :: (T -> boolean) -> [T] -> [[T] [T]]
(defun o-partition (f lst)
  "Partition list (LST) into two lists using predicate function (F). The return
value is a list of lists with the first element is the list of elements for
which F returns t (true), and the second element is the list of elements for
which F returns nil (false)."
  (o-fold-left
    (lambda (acc e)
      (if (o-call f e)
        (list (cons e (car acc)) ; match - add to first element of accumulator
              (cadr acc))
        (list (car acc)
              (cons e (cadr acc))))) ; no match - added to second element of accumulator
    '(() ())
    lst))


;; o-tally :: [T] -> [T . integer]
(defun o-tally (lst &optional o-map)
  "Count elements in list (LST) and return an association list with
key-count pairs."
  (setq counts-hash (if o-map
                        o-map
                        (make-hash-table :test 'equal)))

  (unless (null lst)
    (puthash ; add first element to table
     (car lst)
     (+ 1 (gethash (car lst) counts-hash 0))
     counts-hash)
    (o-tally (cdr lst) counts-hash)) ; recursively run on rest of list

  (let ((counts '()))
    (maphash ; convert hash table to association list
      (lambda (key value)
        (setq counts (cons (cons key value) counts)))
      counts-hash)
    (reverse counts)))


;; o-join :: [string] -> (optional) string -> string
(defun o-join (lst &optional sep)
  "Concatenate the list of strings (LST) into one using the provided
separator (SEP)."
  (o-fold-left
    (lambda (acc s)
      (concat acc sep s))
    (car lst)
    (cdr lst)))


;; o-memoize :: (T -> U) -> (T -> U)
(defun o-memoize (f)
  "Return a memoized version of the provided unary function (F)."
  (let ((hmap (make-hash-table :test 'equal)))
    (lambda (a)
      (let ((cached-value (gethash a hmap :none)))
        (if (o-not-equal-p :none cached-value) cached-value
          (let ((return (o-call f a)))
            (puthash a return hmap)
            return))))))


;; o-list-ref :: [[T]] -> integer -> ... -> T
(defun o-list-ref (nested-lst &rest inds)
  "Get item in the nested list (NESTED-LST) referenced by the
specified indicies (INDS)."
  (letrec ((o-list-ref-base (lambda (lst ind)
                            (if (= 0 ind)
                              (car lst)
                              (o-list-ref (cdr lst) (- ind 1))))))
    (cond ((= 0 (length inds)) nil)
          ((= 1 (length inds)) (funcall o-list-ref-base
                                 nested-lst
                                 (car inds)))
          (t (apply 'o-list-ref
               (funcall o-list-ref-base
                 nested-lst
                 (car inds))
               (cdr inds))))))


;; o-remove-duplicates :: [T] -> [T]
(defun o-remove-duplicates (lst)
  "Remove duplicates from list (LST)."
  (let ((counts (o-tally lst)))
    (o-map 'car counts)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(setq o-else t)



(provide 'othello)
;;; othello.el ends here
