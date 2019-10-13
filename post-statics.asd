; (asdf - another syste definition facility )
(asdf:defsystem :post-statics
  :description "Autumn Lisp Game Jam 2019 entry"
  :author ""
  :license "GPLv3"
  :depends-on (:trivial-gamekit :trivial-gamekit-postproc :trivial-gamekit-fistmachine)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "main")
               (:module :state
                :serial t
                :components ((:file "init")
                             (:file "main-menu")
                             (:file "pause-menu")
                             (:file "gameplay")
                             (:file "end")
                             (:file "credits")))))
