(asdf:defsystem :optima.quasiquote
  :description "Quasiquote extension from fare-quasiquote.
This is a metapackage linked to :fare-quasiquote-optima"
  :version "0.0"
  :author "Francois-Rene Rideau"
  :license "MIT" ;; same as original :fare-quasiquote-optima
  :depends-on (:fare-quasiquote-optima))
