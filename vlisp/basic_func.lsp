; some basic function

(defun assert (x / ) 
 (or x (progn (princ "assert fail.") (exit)))
 )

(defun xpart (pair / )
  (assert (type:ispoint pair))
  (car pair)
  )

(defun ypart (pair / )
  (assert (type:ispoint pair))
  (cadr pair)
  )
(defun zpart (pair / )
  (assert (type:is3dpoint pair))
  (caddr pair)
  )

(defun xor (cond1 cond2 / )
  (and
    (not (and cond1 cond2))
    (or cond1 cond2)
    )
  )

;{{{ functioin for elist field
(defun ssubst_elist (field value elist__ / assoc_field) 
 (assert (listp elist__))
 (setq assoc_field (assoc field elist__))
 (assert (and assoc_field (listp assoc_field)))
 (subst (cons field value) assoc_field elist__)
 )

(defun is_entry_in_elist (field elist__ / ) 
 (if (assoc field elist__) T nil)
 )

(defun is_single_entry_in_elist (field elist__ / elem meeted) 
  (assert (listp elist__))
  (setq meeted 0)
  (while (and (< meeted 2) (setq elem (car elist__))) 
         (setq elist__ (cdr elist__))
         (if (= (car elem) field) (setq meeted (1+ meeted)))
         )
  (if (= meeted 1) T nil)
  )
;}}}

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
    ) (if (caddr point__) (list (caddr point__)) nil))
  )

(defun p_rotate_at (angle__ center__ point__ / ) 
 (setq point__ (math:minus_points point__ center__))
 (setq point__ (p_rotate angle__ point__))
 (math:add_points center__ point__)
 )

; transforming a point from cartesian coordinate system to 
; suferical coordinate system;
; when x is 2d point, which transform a point from
; cartesian coordinate system to polar coordiante system,
; except the result is ternary
(defun p_ccs_to_scs (p__ / rho) 
  (assert (type:ispoint p__))
  (setq rho (math:vector_abs p__))
  (cond 
    ((type:is3dpoint p__)
     (list 
       rho
       (arccos (/ (zpart p__) rho)) 
       (ypart (p_ccs_to_pcs (utils:3dpoint->2dpoint p__)))
       ))
    (T (list (car (setq rho (p_ccs_to_pcs p__))) 0 (cadr rho)))
    )
  )
; suferical coordinate system to cartesian coordinate system
(defun p_scs_to_ccs (p__ / ) 
  (assert (type:is3dpoint p__))
  (mapcar '(lambda (x) (with_epsilon x 0)) 
          (list 
            (* (xpart p__) (sin (ypart p__)) (cos (zpart p__)))
            (* (xpart p__) (sin (ypart p__)) (sin (zpart p__)))
            (* (xpart p__) (cos (ypart p__))                  )
            ))
  )

; cartesian coordinate system to polar coordinate system
(defun p_ccs_to_pcs (p__ / rho) 
 (assert (type:is2dpoint p__))
 (setq rho (math:vector_abs p__))
 (list rho
  (cond 
  ((>= (ypart p__) 0) (arccos (/ (xpart p__) rho)))
  (T     (- (* 2 PI) (arccos (/ (xpart p__) rho))))
  ))
 )
; polar coordinate system to cartesian coordinate system
(defun p_pcs_to_ccs (p__ / ) 
 (assert (type:is2dpoint p__))
 (mapcar '(lambda (x) (with_epsilon x 0)) 
         (list 
           (* (xpart p__) (cos (ypart p__)))
           (* (xpart p__) (sin (ypart p__)))
           ))
 )

(defun p_3drotate (phi__ theta__ point__ / suferical_p)
  (assert (numberp theta__))
  (assert (numberp phi__))
  (assert (type:is3dpoint point__))
  (setq suferical_p (p_ccs_to_scs point__))
  (p_scs_to_ccs 
   (list 
    (xpart suferical_p)
    (+ (ypart suferical_p) phi__)
    (+ (zpart suferical_p) theta__)
    ))
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
