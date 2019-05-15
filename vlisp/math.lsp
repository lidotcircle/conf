; inner product
(defun math:inner_product (x y / ret) ;{{{
  (if (= (length x) (length y))
    (progn
      (setq ret 0)
      (while (car x)
             (setq ret (+ ret (* (car x) (car y))))
             (setq x (cdr x))
             (setq y (cdr y))
             )
      (setq ret ret)
      ) nil
    )
  ) ;}}}

(defun math:vector_abs (x / ret) ;{{{
  (setq ret (sqrt (math:inner_product x x)))
  ) ;}}}

(defun math:sum (x / ret) ;{{{
  (while (car x)
         (setq ret (+ ret (car x)))
         (setq x (cdr x))
         )
  (setq ret ret)
  ) ;}}}

(defun math:minus_points (x y / ret) ;{{{
  (if (= (length x) (length y))
    (progn
      (while (car x)
             (setq ret (append ret (list (- (car x) (car y)))))
             (setq x (cdr x))
             (setq y (cdr y))
             )
      (setq ret ret)
      ) nil
    )
  ) ;}}}

(defun math:neg_points (x / ret) ;{{{
  (while (car x)
         (setq ret (append ret (list (- (car x)))))
         (setq x (cdr x))
         )
  (setq ret ret)
  ) ;}}}

(defun math:points_over_cons (x c / ret) ;{{{
  (while (car x)
         (setq ret (append ret (list (/ (car x) c))))
         (setq x (cdr x))
         )
  (setq ret ret)
  ) ;}}}

(defun math:add_points (x y / ret) ;{{{
  (if (= (length x) (length y))
    (progn
      (while (car x)
             (setq ret (append ret (list (+ (car x) (car y)))))
             (setq x (cdr x))
             (setq y (cdr y))
             )
      (setq ret ret)
      ) nil
    )
  ) ;}}}

; math basic func
(defun math:line_unit_vector (line / ret) ;{{{
  (setq ret (math:minus_points (car line) (cadr line)))
  (setq ret (math:points_over_cons ret (math:vector_abs ret)))
  ) ;}}}

(defun math:orthogonal_line_unit_vector (line / ret) ;{{{
  (setq ret (math:minus_points (car line) (cadr line)))
  (setq ret (list (cadr ret) (- (car ret))))
  (setq ret (math:points_over_cons ret (math:vector_abs ret)))
  ) ;}}}

(defun math:orthogonal_line_unit_vector_pd (line point / fake cp) ;{{{
  (setq cp (math:points_over_cons (math:add_points (car line) (cadr line)) 2))
  (setq fake (math:orthogonal_line_unit_vector line))
  (if (> (math:inner_product
           (math:minus_points point cp)
           fake
           ) 0)
    fake
    (math:neg_points fake)
    )
  ) ;}}}

(defun math_local:two_p_dire__ (theta1 theta2 / ret) ;{{{ clockwise - 1, countclockwise - -1
  (cond
    ((> theta2 theta1)
     (cond
       ((< (- theta2 theta1) PI) (setq ret -1))
       ((= (- theta2 theta1) PI) (setq ret 0))
       ((> (- theta2 theta1) PI) (setq ret 1))
       ))
    ((= theta1 theta2)
     (setq ret 0)
     )
    ((< theta2 theta1)
     (cond
       ((< (- theta1 theta2) PI) (setq ret 1))
       ((= (- theta1 theta2) PI) (setq ret 0))
       ((> (- theta1 theta2) PI) (setq ret -1))
       ))
    )
  ret
  ) ;}}}

; checking whether a point locate in a convex area
; that be represented by an LWPOLYLINE entity.
(defun math:point_in_convex_area (point pl_entity / ;{{{
                                        smca pre_angle
                                        order__ invalid
                                        first_angle)
  (setq invalid 0)
  (if (listp pl_entity)
    (setq smca pl_entity)
    (progn
      (if (utils:entity_is pl_entity "LWPOLYLINE") T (setq invalid 1))
      (setq smca (utils:polyline_points pl_entity))
      )
    )
  (if (< (length smca) 3) (setq invalid 1)
    (progn 
      (setq pre_angle (angle point (car smca)))
      (setq first_angle pre_angle)
      (setq smca (cdr smca))
      (setq order__
            (math_local:two_p_dire__ pre_angle (setq pre_angle (angle point (car smca)))))
      (setq smca (cdr smca))
      ))
  (while (and (car smca) (= invalid 0))
         (if (/= order__ (math_local:two_p_dire__ pre_angle first_angle))
           (if (/= order__ (math_local:two_p_dire__ pre_angle (setq pre_angle (angle point (car smca)))))
             (setq invalid 1)
             )
           (if (or
                 (/= order__ (math_local:two_p_dire__ pre_angle (setq pre_angle (angle point (car smca)))))
                 (/= order__ (math_local:two_p_dire__ pre_angle first_angle)))
             (setq invalid 1)
             )
           )
         (setq smca (cdr smca))
         )
  (if (= invalid 0) T nil)
  ) ;}}}
