; utils

(defun utils:entity_is (entity typestr / ) ;{{{
 (and
   (= (type entity) 'ENAME)
   (= (cdr (assoc 0 (entget entity))) typestr))
 ) ;}}}

; f: nil     -> nil
; f: pickset -> list
(defun utils:pickset->list (pickset_ / elem len i ret) ;{{{
  (assert (or (not pickset_) (= (type pickset_) 'PICKSET)))
  (setq i 0)
  (if pickset_ 
    (while (setq elem (ssname pickset_ i))
           (setq i (1+ i))
           (setq ret (append ret (list elem)))
           ))
  ret
  ) ;}}}

; list filter function
(defun filter_list (list__ function__ / ret elem) ;{{{
  (assert (listp list__))
  (foreach elem list__
           (if (apply function__ (list elem))
             (setq ret
                   (append ret (list elem))
                   )
             )
           )
  ret
  ) ;}}}

; pickset filter function
(defun filter_ss (ss__ function__ / ret elem ss_index) ;{{{
  (assert (= (type ss__) 'PICKSET))
  (setq ss_index 0)
  (setq ret (ssadd))
  (while (setq elem (ssname ss__ ss_index))
         (setq ss_index (1+ ss_index))
         (if (apply function__ (list elem))
           (setq ret
                 (ssadd elem ret)
                 )
           )
         )
  ret
  ) ;}}}

(defun min_pair (pair_list / ret point) ;{{{
  (setq ret (nth 0 pair_list))
  (foreach point pair_list
           (if (< (+ (car point) (cadr point)) (+ (car ret) (cadr ret)))
             (setq ret point)
             )
           )
  ret
  ) ;}}}

(defun max_pair (pair_list / ret point) ;{{{
  (setq ret (nth 0 pair_list))
  (foreach point pair_list
           (if (> (+ (car point) (cadr point)) (+ (car ret) (cadr ret)))
             (setq ret point)
             )
           )
  ret
  ) ;}}}

(defun utils:polyline_points (ename / ret ent_data) ;{{{
  (if (setq ent_data (entget ename)) ; entity is present
    (progn
      (setq ret (filter_list ent_data
                             (function (lambda (x) (and (listp x) (= (car x) '10)))))
            )
      (mapcar (function (lambda (x) (cdr x))) ret)
      ) nil
    )
  ) ;}}}

(defun utils:line_points (line_name / ret) ;{{{
  (if (setq ent_data (entget line_name)) ; entity is present
    (progn
      (setq ret (filter_list ent_data
                             (function (lambda (x) (and (listp x) (or (= (car x) '10) (= (car x) '11))))))
            )
      (mapcar (function (lambda (x) (cdr x))) ret)
      ) nil
    )
  ) ;}}}

(defun utils:3dpoint->2dpoint (tuple / ) ;{{{
 (list (car tuple) (cadr tuple))
) ;}}}

(defun utils_local:line_k (line / dx) ;{{{
  (setq dx (- (xpart (car line)) (xpart (cadr line))))
  (if (= dx '0) nil
    (/ (- (ypart (car line)) (ypart (cadr line))) dx)
    )
  ) ;}}}

; we should consider the vertical line ...
(defun utils_local:is_intersecting__ (line1 line2 / theta__ dx1 dx2) ;{{{
  (setq theta__ (- (angle (car line1) (cadr line1))))
  (setq line1 (mapcar '(lambda (x) (p_rotate theta__ x)) line1))
  (setq line2 (mapcar '(lambda (x) (p_rotate theta__ x)) line2))
  (if
    (<= (*
          (- (ypart (car line2)) (ypart (car line1)))
          (- (ypart (cadr line2)) (ypart (car line1)))
          ) 0
        )
    T nil
    )
  ) ;}}}
(defun utils:is_intersecting (line1 line2 / ) ;{{{
  (and
    (utils_local:is_intersecting__ line1 line2)
    (utils_local:is_intersecting__ line2 line1)
    )
  ) ;}}}

(defun utils:point_in_line_x (line__ x / ) ;{{{
  (if (= (xpart (car line__)) (xpart (cadr line__)))
    nil
    (progn
      (
       list x
       (+ (*
            (- x (xpart (car line__)))
            (/
              (- (ypart (cadr line__)) (ypart (car line__)))
              (- (xpart (cadr line__)) (xpart (car line__)))
              )
            ) (ypart (car line__))
          )
       )
      )
    )
  ) ;}}}
(defun utils:point_in_line_y (line__ y / ) ;{{{
  (if (= (ypart (car line__)) (ypart (cadr line__)))
    nil
    (progn
      (
       list
       (+ (*
            (- y (ypart (car line__)))
            (/
              (- (xpart (cadr line__)) (xpart (car line__)))
              (- (ypart (cadr line__)) (ypart (car line__)))
              )
            ) (xpart (car line__))
          ) y
       )
      )
    )
  ) ;}}}
(defun utils:intersecting_point (line1 line2 / theta__ t1) ;{{{
  (if (utils:is_intersecting line1 line2)
    (progn
      (setq theta__ (- (angle (car line1) (cadr line1))))
      (setq line1 (mapcar '(lambda (x) (p_rotate theta__ x)) line1)) ; currently, line1 is horizontal
      (setq line2 (mapcar '(lambda (x) (p_rotate theta__ x)) line2))
      (setq t1 (utils:point_in_line_y line2 (ypart (car line1))))
      (p_rotate (- theta__) t1)
      ) nil
    )
  ) ;}}}
