;;;; RECLET -- a recursive LET for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem :reclet
  :version "0.1"
  :description "A recursive LET for Common Lisp."
  :licence "MIT"
  :depends-on (trivial-lazy)
  :components ((:file "reclet")
               (:static-file "README")))

