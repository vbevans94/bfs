(setq inputgraph '( (1 . (2 3)) (2 . (1 3)) (3 . (1 2 4 5)) (4 . (3 6)) (6 . (4 5)) (5 . (3 6)) ))

(defun bfs (graph q1 q2)
	(setq visited '())
	(loop while (not (null q1))
		do (progn
			(print "Process next element in bfs.")
			(setq x (car q1))
			(print x)
			
			(print "Add this element to visited")
			(push x visited)
			(print visited)
		
			(print "Remove proceed node from the queue.")
			(setq q1 (cdr q1))
			(print q1)
		
			(print "Find and save into toenqueue var new nodes to be added to the search.")
			(setq copied graph)
			(print copied)
			(loop while (not (null copied))
				do (progn
					(if (eq (caar copied) x)
						(progn
							(setq toenqueue (cdar copied))
						)
					)
					(setq copied (cdr copied))
				)
			)
			
			(print "We are going to enqueue: ")
			(print toenqueue)
			(print visited)
			(print q1)
			(setq toenqueue (nset-difference toenqueue visited :test 'equal))
			(setq toenqueue (nset-difference toenqueue q1 :test 'equal))
			(print toenqueue)
			
			(print "Enqueue new nodes.")
			(if (null q1)
				(setq q1 toenqueue)
				(nconc q1 toenqueue)
			)
			(print q1)
			
			(print "Remove from graph for not seeing them again.")
			(setq graph (remove-if #'(lambda (item) (equalp (car item) x)) graph))
			(print graph)
		)
	)
)

(defun shortest-path (start end net)
 (BFS2 end (list (list start)) net))

(defun BFS2 (end queue net)
  (if (null queue)
      nil
      (expand-queue end (car queue) (cdr queue) net)))
 
(defun expand-queue (end path queue net)
  (let ((node (car path)))
    (if (eql node end)
        (reverse path)
        (BFS2 end (append queue (new-paths path node net)) net)
    )
  ))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))
