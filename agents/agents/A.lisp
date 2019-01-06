(defstructure (A
  (:include A-agent
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

         ;other percepts
         (cond ((not alive)
		'slow-agonizing-death)
	       (glitter
		'grab)
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
               ;when C is not adjacent and threat detected, attack all squares
               ((and has-sword (or fb fg lb lg rb rg) (not fc) (not lc) (not rc))
		'swing)
               ;if C is on either side and threat in front only attack front 
               ((and has-sword (or fb fg) (not fc) (or lc rc))
		'thrust)               
               (forward-threat
                'forward)
               ; g on left
	       (lg
		'(turn left))
	       ; g on right
	       (rg
		'(turn right))
	       ; g on back
	       (bg
		(random-element '((turn left) (turn right))))
               ;C in front and there is no threat in front
               ((and fc (not fb) (not fg))
                'forward)
               ;C on left and no threat on left
               ((and lc (not lb) (not lg))
                '(move left))
               ;C on right and no threat on right
               ((and rc (not rb) (not rg))
                '(move right))
               ;C behind and no threat behind
               ((and bc (not bb) (not bg))
                '(move back))
               
	       (plan
		(pop plan))
	       (alive
		(random-element '((turn right) (turn left))))))))))))

(defmethod thrust ((env our-world) agent-body)
  (let ((sword (find-if #'sword-p (object-contents agent-body))))
    (when sword
      (setq new-loc (add-locs (object-loc agent-body)
			      (object-heading agent-body)))
      (if (find-object-if #'object-alive? new-loc env)
	  (kill (find-object-if #'object-alive? new-loc env))))))
