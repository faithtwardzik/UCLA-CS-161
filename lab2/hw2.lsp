;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; BFS accepts a single list of trees FRINGE and returns a list of leaf nodes in BFS-tra; versal order
(defun BFS (FRINGE)
	(cond ((null FRINGE) nil)
	; If FRINGE is empty or null, return nothing 
		  ((atom (first FRINGE)) (cons (first FRINGE) (BFS (rest FRINGE)))) 
		  ; if first element in FRINGE is NOT a list, it must be a leaf node. Add it to top-level
		  ; list of leaf nodes and recurse over rest of FRINGE
		  (T (BFS (append (rest FRINGE) (first FRINGE)))) 
		  ; otherwise, first element is a list. Add it to FIFO and call BFS on the rest of FRINGE
	)
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (cond   
        ((equal S '(T T T T)) t) 
        ; if the current state equals the goal state, return true
        (t NIL) 
	; else return false
    )
)


; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)     
    (cond ((equal A 'h) ; Homer alone
           (cond ((or (equal (second S) (third S)) (equal (second S) (fourth S))) nil)
                  ; respectively if baby and dog or baby and poison are on the same side, return nil
                  (t (list (cons (NOT (first S)) (rest S))))))
                  ; else valid config, move Homer alone
          
          ((equal 'b A) ; baby and Homer
           (cond ((equal (first S) (second S)) (list (list (not (first S)) (not (second S)) (third S) (fourth S))))
	   ; if Homer and baby are on same side, set config so that they are switched (no invalid config when both
           ; are on same side)
                  (t nil)))
                  ; else Homer not on same side as baby, invalid config
          
          ((equal A 'd) ; dog and Homer
           (cond ((not (equal (first S) (third S))) nil)
                  ; if dog and homer not on same side, invalid
                  ((equal (second S) (fourth S)) nil)
		  ; if baby and poison on same side, invalid
                  (t (list (list (NOT (first S)) (second S) (NOT (third S)) (fourth S))))))
                  ; else valid, make dog/Homer switch and change config
          
          
          ((equal A 'p) ; poison and Homer
           (cond ((not (equal (first S) (fourth S))) nil)
                  ; invalid, poison and Homer not on same side
                  ((equal (second S) (third S)) nil)
                  ; baby and dog on same side, invalid config
                  (t (list (list (NOT (car S)) (second S) (third S) (NOT (fourth S)))))))
                  ; else valid, make poison/Homer switch and change config
           
          (T nil)
          ; anomaly case, invalid parameter passed into function
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
    ; call each version of NEXT-STATE to acquire all possible states reachable from current state
)


; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    ; Current state not on stack
    (cond ((null STATES) nil)
           ; If stack of states is empty return nil, S is not member of STATES
           (t (cond ((equal S (first STATES)) t)
              ; If current state equals first element in STATES, S is member, return true
              (t (ON-PATH S (rest STATES)))
              ; continue through rest of stack of states until S found
              )
           )
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond ((null STATES) NIL) 
        ; all states reached, return up recursive tree
        (t
            (let ((singleDFS (DFS (car STATES) PATH ) )) 
            ; use local variable to keep result of single DFS search 
                (cond ((null singleDFS) (MULT-DFS (cdr STATES) PATH)) 
                ; if singleDFS returns nil, must run DFS starting with rest of STATES
                    (t singleDFS) 
		    ; Else singleDFS not null, return value
                )
            )
        ) 
    )

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond ((FINAL-STATE S) (append PATH (list S))) 
    ; goal state acquired, append path from initial state to S onto goal state S to get entire path
        (t (MULT-DFS (SUCC-FN S) (append PATH (list S)))) 
        ; else goal state not yet acquired, run DFS on all successor states to current state and append
        ; these paths onto S
    )
)
    
