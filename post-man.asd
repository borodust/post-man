(asdf:defsystem :post-man
  :description "Autumn Lisp Game Jam 2019 entry"
  :author ""
  :license "GPLv3"
  :depends-on (:alexandria
               :trivial-gamekit
               :trivial-gamekit-postproc
               :trivial-gamekit-fistmachine
               :trivial-gamekit-input-handler)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "resources")
               (:file "main")
               (:module :state
                :serial t
                :components ((:file "init")
                             (:file "main-menu")
                             (:file "pause-menu")
                             (:file "gameplay")
                             (:file "end")
                             (:file "credits")))))
