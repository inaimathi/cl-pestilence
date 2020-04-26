;;;; cl-pestilence.asd

(asdf:defsystem #:cl-pestilence
  :serial t
  :description "Starting on a pestilence simulation engine"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :depends-on (#+sbcl
	       #:alexandria #:anaphora #:cl-fad #:closer-mop
	       #:cl-who #:cl-css #:parenscript #:ironclad
	       #:house #:fact-base
	       #:edit-distance)
  :components ((:module
                src :components
                ((:file "package")
		 (:file "cl-pestilence")

		 (:module
		  front-end :components
		  ((:file "base")
		   (:file "pestilence")))))))
