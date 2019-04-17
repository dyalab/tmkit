(defpackage smt-symbol
  (:use :cl)
  (:export

   :declare-fun
   :|declare-fun|

   :declare-const
   :|declare-const|

   :declare-enum
   :|declare-enum|

   :declare-datatype
   :|declare-datatype|

   :declare-datatypes
   :|declare-datatypes|

   :define-fun
   :|define-fun|

   :check-sat
   :|check-sat|

   :get-value
   :|get-value|

   :ite
   :|ite|

   :bool
   :|Bool|

   :int
   :|Int|

   :real
   :|Real|

   :push
   :|push|

   :pop
   :|pop|

   :let
   :|let|

   :|not|
   :|and|
   :|or|
   :|assert|

   :distinct
   :|distinct|

   :true
   :|true|

   :false
   :|false|

   :=>
   :<=>

   :^

   :minimize
   :|minimize|

   :maximize
   :|maximize|

   :soft-assert
   :|soft-assert|

   :now
   :|now|

   :next
   :|next|
   ))
