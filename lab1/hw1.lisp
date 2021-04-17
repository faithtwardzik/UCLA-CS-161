; PAD takes in N and returns the Nth Padovan number
; SUMS returns the number of additions completed by PAD
; ANON takes a tree and returns the same tree, with all variables
; replaced by '?' 

; PAD accepts a single integer argument N and returns 
; the Nth Padovan number
(defun PAD (N)
  (cond ((equal N 1) 1)
	((equal N 2) 1)
        ((equal N 0) 1)
        (t (+ (PAD (- N 2)) (PAD (- N 3))))
        )
  )

; SUMS takes in a single integer argument N and returns
; the number of additions carried out recursively by PAD
; when running with N as its argument
(defun SUMS (N)
  (cond ((< N 3) 0)
        ((equal N 3) 1)
        ((equal N 4) 1)
        (t (+ 1 (+ (SUMS (- N 2)) (SUMS (- N 3)))))
        )
  )

; ANON accepts a single list TREE, recursively replaces
; all elements with '?' while retaining the tree's
; structure, and returns the anonymized tree
(defun ANON (TREE)
  (cond ((not TREE) nil)
        ((atom TREE) '?)
        (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
        )
  )

