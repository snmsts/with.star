;; TODO redefinition of generic function.
;; TODO improve pickup-vars-from-lambda-list

(in-package :with*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *context* '(:default))
  (defvar *options* nil)
  (defgeneric expander (macro key opts))
  (defmethod expander ((macro t) (key t) (opts t))
    #'identity)
  ;; TODO: I know I'm not serious enough... I know.
  (defun pickup-vars-from-lambda-list (lambda-list)
    (loop :with vars
          :with mode := :req 
          :for elts :on lambda-list
          :for elt := (first elts)
          :for new-mode := (case elt
                             (&allow-other-keys :req)
                             (&aux :req)
                             (&body :req)
                             (&environment :req)
                             (&key :key)
                             (&optional :opt)
                             (&rest :req)
                             (&whole :req))
          :do (cond
                (new-mode (setf mode new-mode))
                ((listp elt)
                 (setf vars
                       (case mode 
                         ((:req)
                          (append (nreverse (pickup-vars-from-lambda-list elt))
                                  vars))
                         ((:opt)
                          (append `(,(first elt) ,@(when (third elt) (list (third elt)))) vars))
                         ((:key)
                          (cons (if (listp elt)
                                    (if (listp (first elt))
                                        (second (first elt))
                                        (first elt))
                                    elt)
                                vars)))))
                ((and (case mode 
                        ((:req :opt :aux) t))
                      (symbolp elt))
                 (push elt vars)))
              (when (and (consp elts)
                         (symbolp (cdr elts))
                         (cdr elts))
                (push (cdr elts) vars))
          :finally (return (nreverse vars)))))

(defmacro defmacro% (macro-name
                     lambda-list &body body)
  (let* ((x (gensym (string '#:x/)))
         (opts (gensym (string '#:opts/)))
         (orig-name (when (listp macro-name)
                      (prog1 (second macro-name)
                        (setf macro-name (first macro-name)))))
         (symbol (gensym (string '#:symbol/)))
         (expander (if (keywordp lambda-list)
                       (prog1 lambda-list
                         (setf lambda-list (first body)
                               body (rest body)))
                       :default))
         (result (if (eql expander :default)
                     (gensym (string '#:result/))
                     (prog1 (caar body)
                       (setf body (rest body)))))
         (docstring (when (stringp (first body))
                      (first body)))
         (args (pickup-vars-from-lambda-list
                lambda-list)))
    `(progn
       (defmethod expander ((,symbol (eql ',macro-name))
                            (,x (eql ,expander))
                            (,opts t))
         (flet ((,#1=(intern (format nil "~A/~A" macro-name expander)) (,x)
                  (apply #'(lambda (,result 
                                    &optional 
                                      ,@args)
                             (list `(,@,result 
                                     ,@(let* ((*options* ,opts)
                                              (,symbol (progn ,@body)))
                                         (when ,symbol
                                           (list ,symbol))))
                                   ,@args)) ,x)))
           #',#1#))
       ,@(if (eql expander :default)
             `((setf (get ',macro-name 'args) ',args)
               ,@(when orig-name `((setf (get ',orig-name 'defmacro*) ',macro-name)))
               (defmacro ,macro-name ,lambda-list
                 ,@(when docstring (list docstring))
                 (let ((,result (reduce
                                 #'(lambda (,result ,symbol)
                                     (funcall (expander ',macro-name ,symbol nil)
                                              ,result))
                                 (process-with (get ',macro-name 'context) *context*)
                                 :initial-value
                                 (list nil ,@args))))
                   (if (cdr (first ,result))
                       `(progn ,@(first ,`,result))
                       (caar ,result)))))
             `(',macro-name)))))

(defmacro% (defmacro* defmacro) (macro-name lambda-list &body body)
  "defmacro*"
  (macroexpand-1 `(defmacro% ,macro-name ,lambda-list ,@body)))

(defmacro* defmacro* :unbound (macro-name lambda-list &body body)
  `(fmakunbound ',macro-name))

(defmacro* defmacro* :export (macro-name lambda-list &body body) (result)
  `(export ',macro-name))

#+nil
(progn
  ;; some text
  (defmacro* (let** let) (&whole whole bindings &body body)
    "hage"
    `(let ,bindings ,@body))

  (defmacro* let** :not (&whole whole bindings &body body) (result)
    `(let/ ,bindings ,@body))
  (with (:not :*)
    (macroexpand-1 '(let ((a 1) (b 2)) (list a b))))
  (funcall (expander 'let** :default) (list nil nil '((a 1) (b 2)))))

#+nil
(pickup-vars-from-lambda-list '((f g) a b c &optional (d 3) &key i (j 1) ((k l) 2) e . h))
#+=> (F G A B C D J L H)
