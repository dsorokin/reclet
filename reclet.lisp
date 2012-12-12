;;;; RECLET -- a recursive LET for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :reclet
  (:use :cl :lazy)
  (:export #:reclet))

(in-package :reclet)

;;(defmacro reclet (((name value)) &body body)
;;  (let ((x (gensym)))
;;    `(let ((,x (cons nil nil)))
;;       (symbol-macrolet ((,name (force (car ,x))))
;;         (setf (car ,x) (delay ,value :thread-safe t))
;;         ,@body))))

(defmacro reclet (decls &body body)
  (labels
      ((make-infos (decls)
         (loop for decl in decls collect
              (destructuring-bind (name value) decl
                (list :name name :value value :gen (gensym)))))
       (gen-let (info)
         `(,(getf info :gen) nil))
       (gen-symbol-macrolet (info)
         `(,(getf info :name) (force ,(getf info :gen))))
       (gen-setf (info)
         `(setf ,(getf info :gen) (delay ,(getf info :value) 
                                         :thread-safe t)))
       (gen-lets (infos)
         (loop for info in infos collect (gen-let info)))
       (gen-symbol-macrolets (infos)
         (loop for info in infos collect (gen-symbol-macrolet info)))
       (gen-setfs (infos)
         (loop for info in infos collect (gen-setf info))))
    (let ((infos (make-infos decls)))
      `(let (,@(gen-lets infos))
         (symbol-macrolet (,@(gen-symbol-macrolets infos))
           ,@(gen-setfs infos)
           ,@body)))))
