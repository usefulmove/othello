;;; othello.el --- Othello functional programming library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: March 4, 2024
;; Version: 0.12.26
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


;;; ( predicates )

;; o-not= :: T -> U -> V -> ... -> boolean
(defmacro o-not= (&rest args)
  "Test that objects are not numerically equal."
  `(not (= ,@args)))


;; o-equal-p :: T -> U -> boolean
(fset 'o-equal-p #'equal)


;; o-not-equal-p :: T -> U -> boolean
(defmacro o-not-equal-p (a b)
  "Test that objects A and B do not have equal components."
  `(not (equal ,a ,b)))


;; o-eq-p :: T -> U -> boolean
(fset 'o-eq-p #'eq)


;; o-not-eq-p :: T -> U -> boolean
(defmacro o-not-eq-p (a b)
  "Test that objects A and B are not the same object."
  `(not (eq ,a ,b)))


;; o-true-p :: T -> boolean
(defmacro o-true-p (a)
  "Test that object A is true."
  `(not (null ,a)))


;; o-false-p :: T -> boolean
(fset 'o-false-p #'null)


;; o-null-p :: T -> boolean
(fset 'o-null-p #'null)


;; o-empty-p :: [T] -> boolean
(fset 'o-empty-p #'null)


;; o-contains-p :: T -> [T] -> boolean
(defmacro o-contains-p (a lst)
  `(o-true-p (member ,a ,lst)))


;; o-all-p :: (T -> boolean) -> [T] -> boolean
(defun o-all-p (f lst)
  "Check that function applied to all values in the list returns true."
  (cond ((null lst) t)
        ((not (funcall f (car lst))) nil)
        (t (o-all-p f (cdr lst)))))


;; o-any-p :: (T -> boolean) -> [T] -> boolean
(defun o-any-p (f lst)
  "Check that function (F) applied to at least one value in the
list (LST) returns true."
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (o-any-p f (cdr lst)))))


;; o-even-p :: number -> boolean
(defun o-even-p (n)
  "Is N even?"
  (= 0 (mod n 2)))


;; o-odd-p :: number -> boolean
(defun o-odd-p (n)
  "Is N odd?"
  (= 1 (mod n 2)))


;; o-zero-p :: number -> boolean
(defun o-zero-p (n)
  "Is N equal to zero?"
  (= 0 n))


;; o-ascii-numeric-p :: char -> boolean
(defun o-ascii-numeric-p (c)
  "Check is C a valid ascii numeric character?"
  (and (>= c ?0) (<= c ?9)))


;;; ( miscellaneous )


;; o-call
(defmacro o-call (f &rest args)
  `(funcall ,f ,@args))


;; o-assert-equal :: sexp -> sexp -> string -> nil (IMPURE)
(defmacro o-assert-equal (sexp1 sexp2 error-msg)
  `(when (not (equal ,sexp1 ,sexp2))
  (error ,error-msg)))


;; o-map :: (T -> U) -> [T] -> [U]
(fset 'o-map #'mapcar)


;; o-filter :: (T -> boolean) -> [T] -> [T]
(defmacro o-filter (f lst)
  `(cl-remove-if-not ,f ,lst))


;; o-flatten :: [[T]] -> [T]
(fset 'o-flatten #'-flatten)


;; o-begin
(defmacro o-begin (&rest sexps)
  "Evaluate body S-expressions (SEXPS) sequentially and return value of the
last one. Similar to `progn'"
  `(let () ,@sexps))


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


;; o-id-debug :: T -> T (impure)
(defun o-id-debug (object)
  (message (prin1-to-string object))
  object)


;; o-fold-left :: (U -> T -> U) -> U -> [T] -> U
(defmacro o-fold-left (f acc lst)
  "Fold (reduce) list (LST) using applied function F starting with initial value
ACC for the accumulator. Fold right (o-fold-right) works in the opposite
direction through the list compared with fold left (o-fold-left)."
  `(seq-reduce ,f ,lst ,acc))

;; o-fold :: (U -> T -> U) -> U -> [T] -> U
(fset 'o-fold #'o-fold-left)


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
  (message (prin1-to-string fns))
  (cond ((null fns) seed)
        (t (let ((f (car fns))
                 (rest (cdr fns)))
             (apply 'o-thread (funcall f seed) rest)))))


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


;; o-range :: integer -> (optional) integer -> (optional) integer -> [integer]
(defun o-range (&rest args)
  "Generate a list of values from FROM (inclusive, optional) to TO
(non-inclusive) by STEP (optional). All arguments should be positive integers.
If only one argument is provided. The list provided will start at zero and end
at the provided TO (non-inclusive)."
  (let ((n-args (length args)))
    (cond ((= 0 n-args) (number-sequence 0))
          ((= 1 n-args) (number-sequence 0 (- (abs (car args)) 1)))
          ((= 2 n-args) (number-sequence
                         (abs (car args))
                         (- (abs (cadr args)) 1)))
          (t (number-sequence
              (car args)
              (- (abs (cadr args)) 1)
              (abs (caddr args)))))))


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


;; o-sum :: [T] -> T
(defun o-sum (lst)
  "Sum elements of list (LST)."
  (apply '+ lst))


;; o-product :: [T] -> T
(defun o-product (lst)
  "Calculate the product of elements of list (LST)."
  (apply '* lst))


;; o-init :: [T] -> [T]
(defun o-init (lst)
  "Return all elements of list (LST) except first."
  (reverse (cdr (reverse lst))))


;; o-last :: [T] -> [T]
(defun o-last (lst)
  "Return the last element of the list (LST)."
  (car (reverse lst)))


;; o-tail :: [T] -> [T]
(fset 'o-tail #'cdr)


;; o-join-chars :: [char] -> string
(defun o-join-chars (chars)
  "O-Join the elements of list of characters (CHARS) into a string."
  (apply 'string chars))


;; o-char-to-int :: char -> integer
(defun o-char-to-int (c)
  "Convert numeric character (C) to its numeric equivalent."
  (if (member c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      (- c ?0)
    (error "invalid argument passed to o-char-to-int")))


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


;; o-zip :: [T] -> [U] -> ... -> [[T U ...]]
(defun o-zip (&rest lsts)
  "Zip lists (LSTS) together and return a list of lists in which the first
element is a list of the first elements of each list. The resulting zipped list
will have the same length as the shortest of the provided lists."
  (letrec ((any-null-p (lambda (lst)
                    (cond ((null lst) nil)
                          ((null (car lst)) t)
                          (t (funcall any-null-p (cdr lst)))))))
    (cond ((null lsts) '())
          ((funcall any-null-p lsts) '())
          (t (cons (apply 'list (mapcar 'car lsts))
                   (apply 'o-zip (mapcar 'cdr lsts)))))))


;; o-zip-with-index :: [T] -> [[int T]]
(defun o-zip-with-index (lst)
  (o-zip (o-range (length lst)) lst))



;; o-zip-with :: (T -> U -> V) -> [T] -> [U] -> [V]
(defun o-zip-with (f lst1 lst2)
  (o-map
   (lambda (sublst)
     (apply f sublst))
   (o-zip lst1 lst2)))


;; o-enumerate :: [T] -> [[integer . T]]
(defun o-enumerate (lst &rest args)
  "Enumerate the list (LST) by returning an association list whose elements are
the element index (0-based) and the element itself."
  (let ((index (if (null args)
                   0
                 (car args))))
    (if (null lst)
        '()
      (cons (cons index (car lst))
            (o-enumerate (cdr lst) (+ 1 index))))))


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


;; o-count-elements :: [T] -> [T . integer]
(defun o-count-elements (lst &optional o-map)
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
    (o-count-elements (cdr lst) counts-hash)) ; recursively process tail of list

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
  (let ((counts (o-count-elements lst)))
    (o-map 'car counts)))


;; o-adjacent-map :: [T] -> (T -> U) -> [U]
(defun o-adjacent-map (f lst)
  (o-zip-with f (o-init lst) (cdr lst)))


;; o-when
(defmacro o-when (condition &rest sexps)
  `(when ,condition ,@sexps))


;; o-when-not
(defmacro o-when-not (condition &rest sexps)
  `(unless ,condition ,@sexps))


;; o-else
(setq o-else t)



(provide 'othello)
;;; othello.el ends here
