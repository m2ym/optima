(asdf:defsystem :optima.test
  :depends-on (:fiveam :optima :optima.ppcre)
  :components ((:file "test/suite")))
