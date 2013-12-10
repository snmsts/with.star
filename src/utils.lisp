;;; -*- mode: lisp -*-
(in-package :with*)

(defun export-more-with-*-macros ()
  (mapc #'export 
        '(with
          defun*
          defmethod*
          defvar*
          defparameter*
          defconstant*)))

;; TODO: hooks required or not..
(defun process-with (args context)
  (dolist (elt args context)
    (assert (keywordp elt))
    (let ((str (string elt)))
      (assert (> (length str) 0))
      (setq context 
            (case (aref str 0)
              (#\/ (remove (intern (subseq str 1) :keyword) context))
              (#\$ (let ((elt (intern (subseq str 1) :keyword)))
                     (append (remove elt context) (list elt))))
              (t (cons elt (remove elt context))))))))

(defun contextp (keyword &optional (context *context*))
  (assert (keywordp keyword))
  (member keyword context))

(defmacro with (args &body body)
  (let ((context (process-with (if (keywordp args) (list args) args)
                               *context*)))
    `(let ((*context* ',context))
       ,@body)))

(defmacro with* (args &body body)
  `(with ,args 
     ,@(labels ((recur (body)
                  (cond ((consp body)
                         (cons (recur (car body))
                               (recur (cdr body))))
                        ((symbolp body)
                         (or (get body 'defmacro*) body))
                        (t body))))
         (recur body))))
#+nil
(macroexpand-1 '(with* () defun))

;;enable annot
(defmacro use-annot ()
  (let ((set '(fdefinition `(setf ,(intern #.(string '#:annotation-arity) 
                                    #.(string '#:cl-annot.core))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (find-package :cl-annot)
         (with-output-to-string (*standard-output*)
           (ql:quickload :cl-annot)))
       (funcall ,set 2 'with*)
       (funcall ,set 2 'with)
       t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :cl-annot)
    (use-annot)))
