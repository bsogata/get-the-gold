(defstructure (B
  (:include B-agent
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
    (if (and (not fo) (not fp))
        (pushnew 'forward choices))
    ; left availability
    (if (and (not lo) (not lp))
        (pushnew '(move left) choices))
    ; right availability
    (if (and (not ro) (not rp))
        (pushnew '(move right) choices))
    ; back availability
    (if (and (not bo) (not bp))
        (pushnew '(move back) choices))
    ; turn left
    (if (and (not lo) (not lp) (or lgold lg bgold bg fo fp ro rp))
        (pushnew '(turn left) choices))
    ; turn right
    (if (and (not ro) (not rp) (or rgold rg bgold bg fo fp lo lp))
        (pushnew '(turn right) choices))
    
    ; Given the above and other stimuli,
    ; make an action of some sort
    (cond 
     ; If not alive, possibly respawn
     ((not alive)
      'temporary-death)
     ; If there is gold in the current cell, take it
     (glitter
      'grab)
     ; If B has alcohol, either drink it or take some other action
     ; (with heavy bias toward drinking)
     (has-alcohol
      (dotimes (x (* 4 (list-length choices)))
        (setf choices (append choices '(drink))))
      (random-element choices))
     ; Actually, I do not quite know what this does,
     ; but I assume it works somehow
     (gold
      'forward)
     
     ; Interaction with A and C
     ; If not carrying gold and enemy nearby, then attack
     ; Enemy to front
     ((and (not has-gold) (or fa fc))
      'forward)
     ; Enemy to rear
     ((and (not has-gold) (or ba bc))
      '(move back))
     ; Enemy to right
     ((and (not has-gold) (or ra rc))
      '(move right))
     ; Enemy to left
     ((and (not has-gold) (or la lc))
      '(move left))
       
     ; Handles interaction with gold
     ; Attempt to flock around gold
     ; If the B carrying gold is to the right, turn right
     ((and rg (not (or ro rp)))
      '(turn right))
     ; If the B carrying gold is to the left, turn left
     ((and lg (not (or lo lp)))
      '(turn left))     
     ; If there is gold in front, move forward
     ((and (or fgold fg) (not (or fo fp)))
      'forward)
     ; If there is gold to rear, move backward
     ((and (or bgold bg) (not (or bo bp)))
      '(move back))
     ; If there is gold to right, move right
     ((and rgold (not (or ro rp)))
      '(move right))
     ; If there is gold to left, move left
     ((and lgold (not (or ro rp)))
      '(move left))
     ; This would work if B was actually capable of planning
;     (plan
;      (pop plan))
     ; Else do something sort of intelligent
     (t (random-element `(,(random-element choices) 'nothing)))))))))))
