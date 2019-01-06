;;; File: ABCG.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Our World Environment

(defparameter *A-alive* t)
(defparameter *C-alive* t)
(defparameter *A-gold* nil)
(defparameter *C-gold* nil)
(defparameter *A-loc* nil)
(defparameter *C-loc* nil)
(defparameter *C-arrows* 0)
(defparameter *respawn-rate* 4)
(defparameter *max-jugs* 2)

(defstructure (our-world (:include grid-environment))
    "A really dangerous world.")

(defstructure (A-agent-body (:include agent-body
    (name "A")
    (contents (list (make-sword)))))
    "A is given a sword.")

(defstructure (B-agent-body (:include agent-body
    (name "B")
    (contents (loop for x from 1 to *max-jugs* collect (make-alcohol)))))
    "B is given alcohol to drink.")

(defstructure (C-agent-body (:include agent-body
    (name "C")
    (contents (loop for x from 1 to *C-arrow* collect (make-arrow)))))
    "C can be given arrows to shoot.")

(defstructure (A-agent (:include agent (body (make-A-agent-body)))))
(defstructure (B-agent (:include agent (body (make-B-agent-body)))))
(defstructure (C-agent (:include agent (body (make-C-agent-body)))))

(defstructure (pit     (:include object (name "O"))))
(defstructure (gold    (:include object (name "$") (size 0.1))))
(defstructure (arrow   (:include object (name "!") (size 0.01))))
(defstructure (sword   (:include object (name "t") (size 0.05))))
(defstructure (alcohol (:include object (name "&") (size 0.001))))

;;;; Defining the generic functions

(defmethod reset ()
  (setq *A-alive* nil)
  (setq *C-alive* nil)
  (setq *A-gold* nil)
  (setq *C-gold* nil)
  (setq *A-loc* nil)
  (setq *C-loc* nil))

(defmethod update-fn ((env our-world))
  (for each agent in (environment-agents env) do
       ; set *A-alive* t if there exists an A and it is alive
       (if (and (null *A-alive*)
		(equal (object-name (agent-body agent)) "A")
		(object-alive? (agent-body agent)))
	   (setq *A-alive* t))
       ; set *C-alive* t if there exists an C and it is alive
       (if (and (null *C-alive*)
		(equal (object-name (agent-body agent)) "C")
		(object-alive? (agent-body agent)))
	   (setq *C-alive* t))
       ; A and a B/G in the same tile
       (if (and (equal (object-name (agent-body agent)) "A")
		(find-object-if #'deadly? (object-loc (agent-body agent)) env))
	   (kill (agent-body agent)))
       ; C and a B/G in the same tile
       (if (and (equal (object-name (agent-body agent)) "C")
		(find-object-if #'deadly? (object-loc (agent-body agent)) env))
	   (kill (agent-body agent)))
       ; kill anything that is in same tile as pits
       (if (find-object-if #'pit-p (object-loc (agent-body agent)) env)
	   (kill (agent-body agent)))
       ; whatever in agent's hand gets dropped if not alive
       (if (not (object-alive? (agent-body agent)))
	   (release env (agent-body agent)))
       ; if a B has gold, turn its name into G
       (if (and (equal (object-name (agent-body agent)) "B")
		(find-if #'gold-p (object-contents (agent-body agent))))
	   (setf (object-name (agent-body agent)) "G"))
       ; if a G doesn't have gold, then rename back to B
       (if (and (equal (object-name (agent-body agent)) "G")
		(null (find-if #'gold-p (object-contents (agent-body agent)))))
	   (setf (object-name (agent-body agent)) "B"))
       ; if A was alive, then killed
       (if (and *A-alive*
		(equal (object-name (agent-body agent)) "A")
		(not (object-alive? (agent-body agent))))
	   (setq *A-alive* nil))
       ; if C was alive, then killed
       (if (and *C-alive*
		(equal (object-name (agent-body agent)) "C")
		(not (object-alive? (agent-body agent))))
	   (setq *C-alive* nil))
       ; if A is holding gold
       (if (and (equal (object-name (agent-body agent)) "A")
		(agent-body-holding (agent-body agent))
		(equal (object-name (agent-body-holding (agent-body agent)))
		       "$"))
	   (setq *A-gold* t))
       ; if C is holding gold
       (if (and (equal (object-name (agent-body agent)) "C")
		(agent-body-holding (agent-body agent))
		(equal (object-name (agent-body-holding (agent-body agent)))
		       "$"))
	   (setq *C-gold* t))
       ; if A is dead, then it can't have the gold
       (if (not *A-alive*) (setq *A-gold* nil))
       ; if C is dead, then it can't have the gold
       (if (not *C-alive*) (setq *C-gold* nil))
       ; update location of A
       (if (equal (object-name (agent-body agent)) "A")
	   (setq *A-loc* (object-loc (agent-body agent))))
       ; update location of C
       (if (equal (object-name (agent-body agent)) "C")
	   (setq *C-loc* (object-loc (agent-body agent)))))
  ;; Do the normal thing
  (call-next-method))

(defmethod termination? ((env our-world))
      ; when all agents die
  (or (call-next-method)
      ; when A arrives is alive and retrieves gold
      (and *A-alive* *A-gold*)
      ; when C arrives is alive and retrieves gold
      (and *C-alive* *C-gold*)
      ; when both A and C do not exist or they both are dead
      (and (not *A-alive*) (not *C-alive*))))

(defmethod performance-measure ((env our-world) agent)
  "Score 1000 for getting the gold, with penalty of 10000 if dead
  and penalty of 1 for each step taken."
  (let ((agent-body (agent-body agent)))
    (- (if (some #'gold-p (object-contents agent-body)) 1000 0)
       (if (object-alive? agent-body) 0 10000)
       (environment-step env))))

(defmethod get-percept ((env our-world) agent)
  (setq loc (object-loc (agent-body agent)))
  (setq h (object-heading (agent-body agent)))
  (setq name (object-name (agent-body agent)))
  (setq l (cond 
	    ((equal h '(1 0)) '(0 1))
	    ((equal h '(0 1)) '(-1 0))
	    ((equal h '(-1 0)) '(0 -1))
	    ((equal h '(0 -1)) '(1 0))))
  (setq r (cond 
	    ((equal h '(1 0)) '(0 -1))
	    ((equal h '(0 1)) '(1 0))
	    ((equal h '(-1 0)) '(0 1))
	    ((equal h '(0 -1)) '(-1 0))))
  (setq b (cond 
	    ((equal h '(1 0)) '(-1 0))
	    ((equal h '(0 1)) '(0 -1))
	    ((equal h '(-1 0)) '(1 0))
	    ((equal h '(0 -1)) '(0 1))))
  (setq front (add-locs loc h))
  (setq left (add-locs loc l))
  (setq right (add-locs loc r))
  (setq back (add-locs loc b))
  (let ((front-side (find-object-if #'object-alive? front env))
	(left-side (find-object-if #'object-alive? left env))
	(right-side (find-object-if #'object-alive? right env))
	(back-side (find-object-if #'object-alive? back env))
	(forward-sight (far-sight env (agent-body agent))))
    (list
     (if (object-alive? (agent-body agent)) 'alive)
     (if (find-object-if #'gold-p loc env) 'glitter)
     (if (equal forward-sight 'gold) 'gold)
     (cond
       ; alive A and C (non-deadly) see B and G (deadly) as threats
       ((and (object-alive? (agent-body agent))
	     (equal forward-sight 'deadly)
	     (not (deadly? (agent-body agent))))
	'forward-threat)
       ; alive B and G (deadly) see A and C (non-deadly) as threat
       ((and (object-alive? (agent-body agent))
	     (equal forward-sight 'alive)
	     (deadly? (agent-body agent)))
	'forward-threat))
     (if (find-if #'sword-p (object-contents (agent-body agent)))
	 'has-sword)
     (if (find-if #'arrow-p (object-contents (agent-body agent)))
	 'has-arrow)
     (if (find-if #'alcohol-p (object-contents (agent-body agent)))
	 'has-alcohol)
     (if (find-if #'gold-p (object-contents (agent-body agent)))
	 'has-gold)
     ; front block
     (if (and front-side (equal (object-name front-side) "A")) 'fa)
     (if (and front-side (equal (object-name front-side) "B")) 'fb)
     (if (and front-side (equal (object-name front-side) "C")) 'fc)
     (if (and front-side (equal (object-name front-side) "G")) 'fg)
     (if (find-object-if #'gold-p front env) 'fgold)
     (if (find-object-if #'obstacle-p front env) 'fo)
     (if (find-object-if #'pit-p front env) 'fp)
     ; left block
     (if (and left-side (equal (object-name left-side) "A")) 'la)
     (if (and left-side (equal (object-name left-side) "B")) 'lb)
     (if (and left-side (equal (object-name left-side) "C")) 'lc)
     (if (and left-side (equal (object-name left-side) "G")) 'lg)
     (if (find-object-if #'gold-p left env) 'lgold)
     (if (find-object-if #'obstacle-p left env) 'lo)
     (if (find-object-if #'pit-p left env) 'lp)
     ; right block
     (if (and right-side (equal (object-name right-side) "A")) 'ra)
     (if (and right-side (equal (object-name right-side) "B")) 'rb)
     (if (and right-side (equal (object-name right-side) "C")) 'rc)
     (if (and right-side (equal (object-name right-side) "G")) 'rg)
     (if (find-object-if #'gold-p right env) 'rgold)
     (if (find-object-if #'obstacle-p right env) 'ro)
     (if (find-object-if #'pit-p right env) 'rp)
     ; back block
     (if (and back-side (equal (object-name back-side) "A")) 'ba)
     (if (and back-side (equal (object-name back-side) "B")) 'bb)
     (if (and back-side (equal (object-name back-side) "C")) 'bc)
     (if (and back-side (equal (object-name back-side) "G")) 'bg)
     (if (find-object-if #'gold-p back env) 'bgold)
     (if (find-object-if #'obstacle-p back env) 'bo)
     (if (find-object-if #'pit-p back env) 'bp))))

(defmethod legal-actions ((env our-world))
  "In the wumpus world, agents can move around, grab gold and shoot arrows."
  '(shoot grab release forward turn swing temporary-death move drink))

(defun deadly? (object)
  "Alive G agents and alive B agents are deadly to A and C."
  (or (and (equal (object-name object) "G") (object-alive? object))
      (and (equal (object-name object) "B") (object-alive? object))))

;;;; Actions

(defmethod move ((env environment) agent-body direction)
  (setq loc (object-loc agent-body))
  (setq h (object-heading agent-body))
  (setq l (cond 
	    ((equal h '(1 0)) '(0 1))
	    ((equal h '(0 1)) '(-1 0))
	    ((equal h '(-1 0)) '(0 -1))
	    ((equal h '(0 -1)) '(1 0))))
  (setq r (cond 
	    ((equal h '(1 0)) '(0 -1))
	    ((equal h '(0 1)) '(1 0))
	    ((equal h '(-1 0)) '(0 1))
	    ((equal h '(0 -1)) '(-1 0))))
  (setq b (cond 
	    ((equal h '(1 0)) '(-1 0))
	    ((equal h '(0 1)) '(0 -1))
	    ((equal h '(-1 0)) '(1 0))
	    ((equal h '(0 -1)) '(0 1))))
  (setq left (add-locs loc l))
  (setq right (add-locs loc r))
  (setq back (add-locs loc b))
  (cond ((equal direction 'left)
	 (move-object-to agent-body left env))
	((equal direction 'right)
	 (move-object-to agent-body right env))
	((equal direction 'back)
	 (move-object-to agent-body back env))))

(defmethod temporary-death ((env our-world) agent-body)
  ; B's have some chance of respawning each turn after death until resurrected
  (setq num (random *respawn-rate*))
  (if (< num 1)
      (setf (object-alive? agent-body) t)))

(defmethod drink ((env our-world) agent-body)
  (let ((alcohol (find-if #'alcohol-p (object-contents agent-body))))
    (when alcohol
      (setf (object-contents agent-body)
	    (delete alcohol (object-contents agent-body))))))


(defmethod swing ((env our-world) agent-body)
  (let ((sword (find-if #'sword-p (object-contents agent-body))))
    (when sword
      (turn env agent-body 'left)
      (setq new-loc (add-locs (object-loc agent-body)
			      (object-heading agent-body)))
      (if (find-object-if #'object-alive? new-loc env)
	  (kill (find-object-if #'object-alive? new-loc env)))
      (turn env agent-body 'right)
      (setq new-loc (add-locs (object-loc agent-body)
			      (object-heading agent-body)))
      (if (find-object-if #'object-alive? new-loc env)
	  (kill (find-object-if #'object-alive? new-loc env)))
      (turn env agent-body 'right)
      (setq new-loc (add-locs (object-loc agent-body)
			      (object-heading agent-body)))
      (if (find-object-if #'object-alive? new-loc env)
	  (kill (find-object-if #'object-alive? new-loc env)))
      (turn env agent-body 'left))))

(defmethod shoot ((env our-world) agent-body)
  (let ((arrow (find-if #'arrow-p (object-contents agent-body))))
    (when arrow
      (setf (object-contents agent-body)
	    (delete arrow (object-contents agent-body)))
      (propagate-arrow (object-loc agent-body)
		       (object-heading agent-body) env))))

(defun propagate-arrow (loc heading env)
  "An arrow keeps going until it kills something or hits a wall."
  (let ((new-loc (add-locs loc heading)))
    (cond ((find-object-if #'object-alive? new-loc env)
	   (kill (find-object-if #'object-alive? new-loc env)))
	  ((find-object-if #'obstacle-p new-loc env))
	  (t (propagate-arrow new-loc heading env)))))

(defun far-sight ((env our-world) agent-body)
  "An agent can look ahead, beyond more than one tile, until it sees something."
  (recursive-sight (object-loc agent-body) (object-heading agent-body) env))

(defun recursive-sight (loc heading env)
  "Keep looking until it sees something, return what it sees."
  (let ((new-loc (add-locs loc heading)))
    (cond
      ;gold
      ((find-object-if #'gold-p new-loc env)
       'gold)
      ; alive B or G
      ((find-object-if #'deadly? new-loc env)
       'deadly)
      ; remaining alive agents should be A or C
      ((find-object-if #'object-alive? new-loc env)
       'alive)
      ; pits
      ((find-object-if #'pit-p new-loc env)
       'pit)
      ; obstacles
      ((find-object-if #'obstacle-p new-loc env)
       'obstacle)
      ; recursion
      (t (recursive-sight new-loc heading env)))))

(defun kill (object)
  "Make the object no longer alive."
  (when (object-alive? object)
    (setf (object-alive? object) nil)))