(in-package :cl-pestilence)

(house:define-handler (js/pestilence.js :content-type "application/javascript") ()
  (parenscript:ps
    (console.log "WELCOME TO PESTILENCE!")
    (get/json "/api/world/state" (create :world "test")
	      (lambda (world)
		(console.log "WORLD:" world)))))
