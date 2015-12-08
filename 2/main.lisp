(defun split-by-one-x (string)
  (loop for i = 0 then (1+ j)
		as j = (position #\x string :start i)
		collect (parse-integer (subseq string i j))
		while j))
(defun calc-packaging (dim)
  (let* ((w (car dim)) (h (cadr dim)) (d (caddr dim))
					   (s (list (* w h) (* h d) (* d w)))
					   (m (apply 'min s)))
	(+ m (* 2 (apply '+ s)))))
(defun calc-ribbon (dim)
  (+
	(* 2 (- (apply '+ dim) (apply 'max dim)))
	(apply '* dim)))
"Part 1: calculate packaging"
(let ((in (open "dims.txt")) (packaging 0))
  (when in
	(loop for line = (read-line in nil)
		  while line do
		  (setq packaging (+ packaging
							 (calc-packaging (split-by-one-x line)))))
	(close in)
	(print packaging)))
"Part 2: calculate ribbon"
(let ((in (open "dims.txt")) (ribbon 0))
  (when in
	(loop for line = (read-line in nil)
		  while line do
		  (setq ribbon (+ ribbon
						  (calc-ribbon (split-by-one-x line)))))
	(close in)
	(print ribbon)))
