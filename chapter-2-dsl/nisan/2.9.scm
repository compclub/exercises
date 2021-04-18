;; Internal representation of a node: (combinator proc original children...)
;; combinator is the combinator that produced the node
;; proc is the procedure that generates the regexp
;; original is the original version of the node, before rewriting the tree, or #f if the node is its own original.

(define (r:dot) (list r:dot (lambda args ".") #f))

(define (r:bol) (list r:bol (lambda args "^") #f))

(define (r:eol) (list r:eol (lambda args "$") #f))

(define (node-type node) (car node))

(define (node-proc node) (cadr node))

(define (node-original node) (list-ref node 2))

(define (node-children node) (drop node 3))

(define (node-not-children node) (take node 3))

(define (node-flatten root)
  (cons root (append-map node-flatten (node-children root))))

(define (node-execute node #!optional root)
  (let ((root (if (default-object? root) node root)))
    ((node-proc node) node root)))

(define (node-execute-children parent #!optional root)
  (let ((root (if (default-object? root) parent root)))
    (map (lambda (child) (node-execute child root))
	 (node-children parent))))

(define (r:group child)
  (list r:group
	(lambda (node root)
	  (string-append "\\("
			 (car (node-execute-children node root))
			 "\\)"))
	#f
	child))

(define (r:seq . children)
  (case (length children)
    ((0) (assert #f))
    ((1) (car children))
    (else
     (append
      (list r:seq
	    (lambda (node root)
	      (apply string-append (node-execute-children node root)))
	    #f)
      children))))

(define (r:quote-char char)
  (list r:quote-char
	(lambda (node root)
	  (if (memv char chars-needing-quoting)
	      (list->string (list #\\ char))
	      (char->string char)))
	#f))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:quote string)
  (apply r:seq (map r:quote-char (string->list string))))

(define (r:back target)
  (assert (eqv? (node-type target) r:group))
  (r:group
   (list r:back
	 (lambda (node root)
	   (let ((nodes (node-flatten root)))
	     (define (find-target nodes n)
	       (if (pair? nodes)
		   (let ((node (car nodes)))
		     (let ((n (if (eqv? (node-type node) r:group) (+ n 1) n)))
		       (if (eqv? (node-original node) target) (values n #t)
			   (find-target (cdr nodes) n))))
		   (values n #f)))
	     (let-values (((n found) (find-target nodes 0)))
	       (assert found)
	       (string-append "\\" (number->string n)))))
	 #f)))

(define (r:alt . children)
  (assert (> (length children) 0))
  (if (= (length children) 1) (car children)
      (append
       (list r:alt
	     (lambda (node root)
	       (let ((exprs (node-execute-children node root)))
		 (if (pair? exprs)
		     (apply string-append
			    (cons (car exprs)
				  (append-map (lambda (expr)
						(list "\\|" expr))
					      (cdr exprs))))
		     ""))))
       #f
       children)))

(define (r:repeat min max child)
  (list r:repeat
	(lambda (node root)
	  (string-append
	   (car (node-execute-children node root))
	   "\\{"
	   (number->string min)
	   (cond ((not max) ",")
		 ((= max min) "")
		 (else (string-append "," (number->string max))))
	   "\\}"))
	#f
	child))

(define (r:char-from string)
  (case (string-length string)
    ((0) (assert #f))
    ((1) (r:quote string))
    (else
     (list r:char-from
	   (lambda (node root)
	     (bracket string
		      (lambda (members)
			(if (lset= eqv? '(#\- #\^) members)
			    '(#\- #\^)
			    (quote-bracketed-contents members)))))
	   #f))))

(define (r:char-not-from string)
  (list r:char-not-from
	(lambda (node root)
	  (bracket string
		   (lambda (members)
		     (cons #\^ (quote-bracketed-contents members)))))
	#f))

(define (bracket string procedure)
	  (list->string
	   (append '(#\[)
		   (procedure (string->list string))
		   '(#\]))))

(define (quote-bracketed-contents members)
	  (define (optional char)
	    (if (memv char members) (list char) '()))
	  (append (optional #\])
		  (remove
		   (lambda (c)
		     (memv c chars-needing-quoting-in-brackets))
		   members)
		  (optional #\^)
		  (optional #\-)))

(define chars-needing-quoting-in-brackets
	  '(#\] #\^ #\-))

(define (r:* child)
  (r:repeat 0 #f child))

(define (r:+ child)
  (r:repeat 1 #f child))

(define loose-to-tight (list r:alt r:seq r:repeat))

(define (looser-eq? x y)
  (let looser? ((lst loose-to-tight))
    (if (pair? lst)
	(if (eqv? (car lst) x) #t
	    (if (eqv? (car lst) y) #f
		(looser? (cdr lst))))
	#t)))

;; Applies proc to every parent and child, and replaces the child with the result, depth-first.
(define (node-apply root proc)
  (append
   (take root 2)
   (list ((lambda (original) (if original original root)) (node-original root)))
   (map (lambda (child) (proc root (node-apply child proc)))
	(node-children root))))

(define (insert-groups root)
  (node-apply root (lambda (parent child)
		     (if (or (looser-eq? (node-type parent) (node-type child))
			     (eqv? (node-type parent) r:group))
			 child
			 (r:group child)))))

(define (compile root)
  (node-execute (insert-groups root)))

(define (display-expr root)
  (display (compile root)))

(define (write-bourne-shell-grep-command tree filename)
	  (display (bourne-shell-grep-command-string tree filename)))

(define (bourne-shell-grep-command-string tree filename)
	  (string-append "grep -e "
			 (bourne-shell-quote-string (compile tree))
			 " "
			 filename))

(define (bourne-shell-quote-string string)
	  (list->string
	   (append (list #\')
		   (append-map (lambda (char)
				 (if (char=? char #\')
				     (list #\' #\\ char #\')
				     (list char)))
			       (string->list string))
		   (list #\'))))

;; grep -e '\([aeiou]\).\{0,5\}\([aeiou]\)\(\2\).\{0,5\}\(\1\)' system3.org
(write-bourne-shell-grep-command
	     (let ((group (lambda ()
			   (r:group (r:char-from "aeiou")))))
	       ((lambda (x y) (r:seq x (r:repeat 0 5 (r:dot)) y (r:back y)
				     (r:repeat 0 5 (r:dot)) (r:back x)))
		(group) (group)))
