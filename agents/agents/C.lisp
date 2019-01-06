(defstructure (C
  (:include C-agent
   (program
     (let ((plan nil))
     #'(lambda (percept)
	 (destructuring-bind (alive glitter gold forward-threat
			      has-sword has-arrow has-alcohol has-gold
			      fa fb fc fg fgold fo fp
			      la lb lc lg lgold lo lp
			      ra rb rc rg rgold ro rp
			      ba bb bc bg bgold bo bp) percept
	 (setf choices ())
	 ; forward availability
	 (if (and (not fg) (not fb) (not fo) (not fp))
	     (pushnew 'forward choices))
	 ; left availability
	 (if (and (not lg) (not lb) (not lo) (not lp))
	     (pushnew '(move left) choices))
	 ; right availability
	 (if (and (not rg) (not rb) (not ro) (not rp))
	     (pushnew '(move right) choices))
	 ; back availability
	 (if (and (not bg) (not bb) (not bo) (not bp))
	     (pushnew '(move back) choices))
	 ; turn left
	 (if (and (not lo) (or lgold lg bgold bg fo fp lb ro rp lb bb))
	     (pushnew '(turn left) choices))
	 ; turn right
	 (if (and (not ro) (or rgold rg bgold bg fo fp rb lo lp rb bb))
	     (pushnew '(turn right) choices))

	 ; pick an action
	 (cond ((not alive)
		'slow-agonizing-death)
	       (glitter
		'grab)
	       ((and has-arrow (or fg fb forward-threat))
		'shoot)
	       ; gold in front
	       ((and (or fgold gold) (not fg) (not fb) (not fo) (not fp))
		   'forward)
	       ; gold on left
	       ((and (or lgold) (not lg) (not lb) (not lo) (not lp))
		   '(move left))
	       ; gold on right
	       ((and (or rgold) (not rg) (not rb) (not ro) (not rp))
		   '(move right))
	       ; gold in back
	       ((and (or bgold) (not bg) (not bb) (not bo) (not bp))
		   '(move back))
	       ; g on left
	       (lg
		'(turn left))
	       ; g on right
	       (rg
		'(turn right))
	       ; g on back
	       (bg
		(random-element '((turn left) (turn right))))
	       (choices
		(random-element choices))
		;plan is unused so far...
		; must get to goal point once it gets gold
;	       (plan
;		(pop plan))
	       (alive (random-element '((turn right) (turn left))))))))))))