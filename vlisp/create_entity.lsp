; create autocad entity with (entmake <data>)

; required
(load_vlscript 'anna_utils)
(load_vlscript 'anna_log)
(load_vlscript 'anna_cur_dev)

(defun ce:rectangle_digonal (rec_name / point_data) ;{{{
  (if (ce:is_aligned_rectangle rec_name)
    (progn
      (setq point_data (utils:polyline_points rec_name))
      (list (min_pair point_data) (max_pair point_data))
      )
    nil
    )
  ) ;}}}

; new line
(defun new_line (startpoint endpoint layername linetype scale / valid_args line_data) ;{{{
  (progn
    (setq valid_args 1)
    (if (= (type startpoint) 'LIST) () (setq valid_args 0))
    (if (= (type endpoint) 'LIST) () (setq valid_args 0))
    (if (= (type layername) 'STR) () (setq valid_args 0))
    (if (or (= (type scale) 'REAL) (= (type scale) 'INT)) () (setq valid_args 0))
    (if (= (type linetype) 'STR) () (setq valid_args 0))
    )
  (if (= valid_args 1) 
    (progn ;pass para checking
      (setq line_data (list
                        (cons 0 "LINE")
                        (cons 6 linetype)
                        (cons 8 layername)
                        (cons 10 startpoint)
                        (cons 11 endpoint)
                        (cons 48 scale)
                        (cons 100 "AcDbEntity")
                        ))
      (sc_log line_data)
      (entmake line_data)(princ)
      )
    (progn
      (func_error_msg "new_line" "Arguments error")
      )
    )
  ) ;}}}

(defun ce:test_point_list (test_list / ret list_length index pair_holder) ;{{{
  (setq ret 1)
  (if (not (listp test_list)) (setq ret 0))
  (if (= ret 1) (progn (setq list_length (length test_list))(setq index 0)))
  (while (and (= ret 1) (< index list_length))
         (progn
           (setq pair_holder (nth index test_list))
           (setq index (+ index 1))
           (if (not (and (and (= (length pair_holder) 2) (numberp (car pair_holder)))
                         (numberp (cadr pair_holder)))) (setq ret 0))
           )
         )
  (setq ret ret)
  ) ;}}}

(defun new_straight_polyline (point-list layername linetype scale / ;{{{
                                         pl_data valid_args loop_var point_length)
  (setq valid_args 1)
  (if (not (ce:test_point_list point-list)) (setq valid_args 0))
  (if (not (= (type layername) 'STR)) (setq valid_args 0))
  (if (not (= (type linetype) 'STR)) (setq valid_args 0))
  (if (not (numberp scale)) (setq valid_args 0))
  (if (= valid_args 1)
    (progn
      (setq point_length (length point-list) loop_var 1)
      (setq pl_data
            (list
              (cons 0 "LWPOLYLINE")
              (cons 6 linetype)
              (cons 8 layername)
              (cons 48 scale)
              (cons 100 "AcDbEntity")
              (cons 100 "AcDbPolyline")
              (cons 70 0)
              (cons 90 point_length)
              (cons 10 (nth 0 point-list)) ; add first 2D point
              ))
      (foreach pair__ (cdr point-list)
       (setq pl_data
             (append pl_data
                     (list
                       ; start width, end width, curve with radius 0*<intermediate_length>
                       (cons 40 0) (cons 41 0) (cons 42 0)
                       ; next 2D point
                       (cons 10 pair__)
                       ))))
      (entmake pl_data))
    (setq valid_args nil))
  ) ;}}}

; pass an entity name, check whether it's a polyline and whether composite a closed 
; rectangle.
(defun ce:is_aligned_rectangle (ename / ent_data point_data) ;{{{
  (if (and 
        (setq ent_data (entget ename)) ; entity is present
        (= (cdr (assoc '0 ent_data)) "LWPOLYLINE") ; polyline
        (= (cdr (assoc '70 ent_data)) 1) ; closed
        ; the numbers of points of this polyline entity is 4
        (= (length (setq point_data (utils:polyline_points ename))) '4)
        (or ; the order of points of polyline can be
            ;               1 3   or   1 2
            ;               2 4        3 4
          (and
            (= (car (nth 0 point_data)) (car (nth 1 point_data)))
            (= (car (nth 2 point_data)) (car (nth 3 point_data)))
            (= (last (nth 0 point_data)) (last (nth 3 point_data)))
            (= (last (nth 1 point_data)) (last (nth 2 point_data)))
            )
          (and
            (= (last (nth 0 point_data)) (last (nth 1 point_data)))
            (= (last (nth 2 point_data)) (last (nth 3 point_data)))
            (= (car (nth 0 point_data)) (car (nth 3 point_data)))
            (= (car (nth 1 point_data)) (car (nth 2 point_data)))
            )
          )
        )
    t nil
    )
  ) ;}}}
