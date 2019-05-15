; draw tools

(defun drt:current_rectangles ( layer dim_off / ss__) ;{{{
  (setq ss__ (utils:pickset->list (filter_ss (ssget "X"
                                                    (list (cons 0  "LWPOLYLINE")
                                                          (cons 8 layer)
                                                          (cons 70  1)))
                                             'ce:is_aligned_rectangle)))
  (foreach entity__ ss__
           (drt:set_rectangle_dim entity__ dim_off)
           )
  ) ;}}}

(defun drt:align_dim (fp sp ds dir / tp ttt) ;{{{
  (if (/= 0 ds)
    (progn
      (setq ttt (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      (setq tp
            (math:add_points
              (math:points_over_cons (math:add_points fp sp) 2)
              (math:points_over_cons dir (/ 1.0 ds))))
      (command "dimaligned" fp sp tp)
      (setvar "OSMODE" ttt)(princ)
      )
    )
  ) ;}}}

(defun drt:set_rectangle_dim (rec dim_off / cross_line pl_points i line_ent ;{{{
                                  s1 s2 line_points intersecting_point inner_p vect
                                  horizontal_dimed vertical_dimed dir__)
  (setq pl_points (utils:polyline_points rec))
  (setq pl_points (append pl_points (list (car pl_points))))
  (setq inner_p (math:points_over_cons (math:add_points (car pl_points) (cadr pl_points)) 2))
  (setq vect (math:orthogonal_line_unit_vector (list (car pl_points) (cadr pl_points))))
  (if (math:point_in_convex_area (math:add_points inner_p vect) rec)
    (setq inner_p (math:add_points inner_p vect))
    (setq inner_p (math:minus_points inner_p vect))
    )
  (setq inner_p (math:points_over_cons (apply 'math:add_points (ce:rectangle_digonal rec)) 2))
  (setq
    cross_line
    (utils:pickset->list
      (ssget "C"
             (min_pair pl_points)
             (max_pair pl_points)
             '((0 . "LINE")))))
  (foreach
    line_ent cross_line
    (setq line_points (utils:line_points line_ent))
    (setq i 0)
    (setq vertical_dimed 0)
    (setq horizontal_dimed 0)
    (setq s2 (nth i pl_points))
    (while (and (< i 4) (or (= horizontal_dimed 0) (= vertical_dimed 0)))
           (swap 's1 's2)
           (setq s2 (nth (setq i (1+ i)) pl_points))
           (if (= (car s1) (car s2)) (setq dir__ 1) (setq dir__ 0)) ; vert, hori
           (if
             (and
               (setq intersecting_point (utils:intersecting_point line_points (list s1 s2)))
               (cond
                 ((= dir__ 1) (and (= vertical_dimed 0) (setq vertical_dimed 1)))
                 ((= dir__ 0) (and (= horizontal_dimed 0) (setq horizontal_dimed 1))))
               )
             (progn
               (drt:align_dim s1 intersecting_point dim_off
                              (math:neg_points (math:orthogonal_line_unit_vector_pd (list s1 s2) inner_p)))
               (drt:align_dim s2 intersecting_point dim_off
                              (math:neg_points (math:orthogonal_line_unit_vector_pd (list s1 s2) inner_p)))
               )
             )
           ) ;end while
    ) ;end foreach
  ) ;}}}

(defun drt:trim_line_inside_pl (rec line_layer / cross_line pl_points i line_ent ;{{{
                                    s1 s2 line_info intersecting_point inner_p vect
                                    start_p end_p where__)
  (setq pl_points (utils:polyline_points rec))
  (setq pl_points (append pl_points (list (car pl_points))))
  (setq inner_p (math:points_over_cons (math:add_points (car pl_points) (cadr pl_points)) 2))
  (setq vect (math:orthogonal_line_unit_vector (list (car pl_points) (cadr pl_points))))
  (if (math:point_in_convex_area (math:add_points inner_p vect) rec)
    (setq inner_p (math:add_points inner_p vect))
    (setq inner_p (math:minus_points inner_p vect))
    )
  (setq inner_p (math:points_over_cons (apply 'math:add_points (ce:rectangle_digonal rec)) 2))
  (setq
    cross_line
    (utils:pickset->list
      (ssget "C"
             (min_pair pl_points)
             (max_pair pl_points)
             (list
               (cons 0 "LINE")
               (cons 8 line_layer)
               ))))
  (foreach
    line_ent cross_line
    (setq line_info (entget line_ent))
    (setq start_p (cdr (assoc 10 line_info)))
    (setq end_p (cdr (assoc 11 line_info)))
    (setq where__ 10)
    (if
      (xor
        (math:point_in_convex_area start_p rec)
        (math:point_in_convex_area end_p rec)
        )
      (progn
        (if (math:point_in_convex_area start_p rec)
          nil
          (progn
            (setq where__ 11)
            )
          )
        (setq i 0)
        (setq mod__ 0)
        (setq s2 (nth i pl_points))
        (while (and (< i 4) (= mod__ 0))
               (swap 's1 's2)
               (setq s2 (nth (setq i (1+ i)) pl_points))
               (if
                 (and
                   (setq intersecting_point (utils:intersecting_point (list start_p end_p) (list s1 s2)))
                   (= mod__ 0)
                   )
                 (progn
                   (setq mod__ 1)
                   (setq line_info
                         (subst
                           (cons where__ intersecting_point)
                           (assoc where__ line_info)
                           line_info))
                   (entmod line_info)
                   )
                 )
               ) ;end while
        )
      )
    ) ;end foreach
  ) ;}}}

(defun drt:trim_rectangles (layer layer_trim / ss__) ;{{{
  (setq ss__ (utils:pickset->list (filter_ss (ssget "X"
                                                    (list (cons 0  "LWPOLYLINE")
                                                          (cons 8 layer)
                                                          (cons 70  1)))
                                             'ce:is_aligned_rectangle)))
  (foreach entity__ ss__
           (drt:trim_line_inside_pl entity__ layer_trim)
           )
  ) ;}}}

(defun drt:add_cross_to_circle (ent_circle layer_line cross_scale / ;{{{
                                           center_point circle_info i
                                           p1 p2 circle_radius)
  (setq circle_info (entget ent_circle))
  (if circle_info
    (progn
      (setq center_point (cdr (assoc 10 circle_info)))
      (setq circle_radius (cdr (assoc 40 circle_info)))
      (setq i 0)
      (repeat 2
              (setq p1 (p_shift (list
                                  (* (- 1 i) (* (- circle_radius) cross_scale))
                                  (* i (* (- circle_radius) cross_scale))
                                  0)
                                center_point))
              (setq p2 (p_shift (list
                                  (* (- 1 i) (* circle_radius cross_scale))
                                  (* i (* circle_radius cross_scale))
                                  0)
                                center_point))
              (entmake (list
                         (cons 0 "LINE")
                         (cons 100 "AcDbEntity")
                         (cons 100 "AcDbLine")
                         (cons 8 layer_line)
                         (cons 10 p1)
                         (cons 11 p2)
                         )
                       )
              (setq i (1+ i))
              )
      )
    )
  ) ;}}}

(defun drt:circles_cross (layer layer_line __scale / ss__) ;{{{
  (setq ss__ (utils:pickset->list (ssget "X"
                                         (list (cons 0  "CIRCLE")
                                               (cons 8 layer)))))
  (foreach entity__ ss__
           (drt:add_cross_to_circle entity__ layer_line __scale)
           )
  ) ;}}}

(defun drt:brk_line (p1 p2 / MM x1 x2 x3 x4 x5 x6 theta__ shift__) ;{{{
  (if (and (= (length p1) (length p2))
           (/= (math:vector_abs (math:minus_points p1 p2)) 0))
    (progn
      (setq theta__ (angle p1 p2)
            shift__ p1)
      (setq p2 (p_rotate (- theta__) (p_shift (math:neg_points shift__) p2))
            p1 (p_shift (math:neg_points shift__) p1)
            )
      (setq MM (xpart p2))
      (setq x1 (list (* MM -0.2) 0))
      (setq x2 (list (* MM  1.2) 0))
      (setq x3 (list (* MM 0.43) 0))
      (setq x4 (list (* MM 0.57) 0))
      (setq x5 (list (* MM 0.475) (* MM  0.12)))
      (setq x6 (list (* MM 0.525) (* MM -0.12)))
      (new_straight_polyline
        (mapcar '(lambda (x) (p_shift shift__ (p_rotate theta__ (append x '(0)))))
                (list x1 x3 x5 x6 x4 x2)
                )
        (getvar "CLAYER")
        "BYLAYER" 1
        )
      )
    (progn (princ "Input error point.")(princ))
    )
  ) ;}}}
