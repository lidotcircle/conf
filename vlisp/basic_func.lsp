; some basic function

(defun xpart (pair / )
  (car pair)
  )
(defun ypart (pair / )
  (cadr pair)
  )
(defun zpart (pair / )
  (caddr pair)
  )

(defun xor (cond1 cond2 / )
  (and
    (not (and cond1 cond2))
    (or cond1 cond2)
    )
  )

(defun swap (sym1 sym2 / temp)
 (setq temp (vl-symbol-value sym1))
 (set sym1 (vl-symbol-value sym2))
 (set sym2 temp)
 )

(defun p_rotate (angle__ point__ / )
  (append (list
    (-
      (* (cos angle__) (xpart point__))
      (* (sin angle__) (ypart point__)))
    (+
      (* (sin angle__) (xpart point__))
      (* (cos angle__) (ypart point__)))
    ) (if (zpart point__) (list (zpart point__)) nil))
  )

(defun p_shift (shift__ point__ / )
  (math:add_points point__ shift__)
  )

(defun p_mirror (line__ point__ / unit_vector module__ t1 sign__)
  (setq sign__ 1)
  (setq unit_vector (math:orthogonal_line_unit_vector line__))
  (setq t1 (math:minus_points (car line__) point__))
  (setq module__ (math:inner_product t1 unit_vector))
  (if (<= module 0)
    (progn
      (setq unit_vector (math:neg_points unit_vector))
      (setq module__ (- module__))
      )
    )
  (setq module__ (* (sqrt module__) 2))
  (setq unit_vector (mapcar '(lambda (x) (* x module__)) unit_vector))
  (setq unit_vector (math:add_points point__ unit_vector))
  )
