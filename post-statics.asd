; (asdf - another syste definition facility )
(asdf:defsystem :post-statics
  :description "Autumn Lisp Game Jam 2019 entry"
  :author ""
  :license "GPLv3"
  :depends-on (:trivial-gamekit :trivial-gamekit-postproc)
  :serial t
  :components ((:file "packages")
               (:file "main")))
