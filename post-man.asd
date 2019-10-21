(pushnew :bodge-gl2 *features*)

(asdf:defsystem :post-man
  :description "Autumn Lisp Game Jam 2019 entry"
  :author ""
  :license "GPLv3"
  :depends-on (:alexandria
               :trivial-gamekit
               :trivial-gamekit-fistmachine
               :trivial-gamekit-input-handler
               :bodge-heap
               :random-state
               :babel)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "resources")
               (:file "main")
               (:module :object
                :serial t
                :components ((:file "object")
                             (:file "being")
                             (:file "box")
                             (:file "rack")
                             (:file "bogdan")
                             (:file "level")
                             (:file "rob-o-man")))
               (:module :state
                :serial t
                :components ((:file "state")
                             (:file "init")
                             (:file "pause-menu")
                             (:file "gameplay")
                             (:file "end")
                             (:file "main-menu")
                             (:file "credits")))))
