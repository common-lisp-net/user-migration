(defsystem user-migration
  :name "user-migration"
  :version "0.0.1"
  :maintainer "Hans Huebner"
  :author "Hans Huebner"
  :licence "You don't even have to buy me a beer"
  :serial t
  :description "Migrate common-lisp.net mailing lists back to mailman"
  :depends-on (:cl-postgres+local-time
               :postmodern
               :cl-postgres
               :hunchentoot
               :cl-ppcre
               :xhtmlgen)
  :components((:file "user-migration")))
