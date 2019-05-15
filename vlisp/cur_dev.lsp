; current autolisp develop
(princ "|>_<| load current development file")

(load_vlscript_force 'anna_create_entity)
(load_vlscript_force 'anna_utils)


; determining an area which is represented by points list
; whether or not an convex area
; (defun math:is_convex_area (point_list / len))

(defun drt:trim_line_inside_pl (rec / line_set)
 (setq line_set (ssget "X"))
)

(princ "|<^^>| load current development file finish")
