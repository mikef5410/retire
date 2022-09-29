;;;; retire.asd

(asdf:defsystem #:retire
  :description "Tell me how long till retirement"
  :author "Mike Ferrara <mikef@mrf.sonoma.ca.us"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "retire")))
