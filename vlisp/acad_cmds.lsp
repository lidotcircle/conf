; some toggle commands

(defun mcmds:toggle_orthogonal_mode (/)
 (
  if (< (getvar "ORTHOMODE") 1)
  (progn (setvar "ORTHOMODE" 1)(princ "open orthogonal mode."))
  (progn (setvar "ORTHOMODE" 0)(princ "close orthogonal mode."))
  )
 (princ)
 )

(defun C:tgr (/)
 (mcmds:toggle_orthogonal_mode)
 )

(defun mcmds:toggle_object_snap (/)
 (
  if (> (getvar "osmode") 16384)
  (progn (setvar "osmode" (- (getvar "osmode") 16384))(princ "Object snap open."))
  (
   if (= (getvar "osmode") 0)
   (progn (setvar "osmode" 1)(princ "Object snap open, only endPoint."))
   (progn (setvar "osmode" (+ (getvar "osmode") 16384))(princ "Object snap close."))
   )
  )
 )

(defun C:tgs (/)
 (mcmds:toggle_object_snap)
 )

(defun mcmds:toggle_line_width_display (/)
 (
  if (< (getvar "LWDISPLAY") 1)
  (progn (setvar "LWDISPLAY" 1)
         (princ "Line width display open.")
         (princ)
         )
  (progn (setvar "LWDISPLAY" 0)
         (princ "Line width display close.")
         (princ)
         )
  )
 )
(defun C:tgw (/)
 (mcmds:toggle_line_width_display)
 )

(defun mcmds:toggle_menu_bar (/)
 (
  if (< (getvar "MENUBAR") 1)
  (progn (setvar "MENUBAR" 1)(princ "Open menu bar.")(princ))
  (progn (setvar "MENUBAR" 0)(princ "Close menu bar.")(princ))
  )
 )

(defun C:tgv (/)
 (mcmds:toggle_menu_bar)
 )

(defun C:recdim ( / )
 (drt:current_rectangles (getstring "Input layer name:") (getreal "Input dim offset:"))(princ)
 )

(defun C:rectrim ( / )
 (drt:trim_rectangles (getstring "Input layer name:") (getstring "Input trim layer:"))(princ)
 )

(defun C:cccs ( / )
 (drt:circles_cross (getstring "Input layer name circle:")
                    (getstring "Input layer name of line:") 0.66)(princ)
 )

(defun C:brl ( / )
  (drt:brk_line (getpoint "Input first point:")
                (getpoint "Input second point:")
                )
  )

(defun C_aux:epl ( / sel_line ex_length mode__) 
  (setq sel_line (ssget))
  (setq mode__ (getstring "expand mode:"))
  (setq ex_length (getreal "Expanded length or scale:"))
  (drt:expand_line_set sel_line ex_length mode__)
  )

(defun C:epl ( / ) 
  (C_aux:epl)
  )

(setq __scale__XX 1)
(defun C:ascale ( / ) 
  (princ (strcat "Scale:" (vl-princ-to-string __scale__XX)))
  (command "._scale" (ssget) "" (getpoint "base point:") __scale__XX) (princ)
  )

(defun C:cascale ( / ) 
  (setq __scale__XX (getreal "Input scale:"))
  (defun C:ascale ( / ) 
   (princ (strcat "Scale:" (vl-princ-to-string __scale__XX)))
   (command "._scale" (ssget) "" (getpoint "base point:") __scale__XX) (princ)
    ) (princ)
  )

; not implemented
(defun C:entlist ( / ) 
 (princ (entget (car (entsel)))) (princ)
 )

(defun C:mscale ( / selection__ scale__) 
 (setq selection__ (ssget))
 (command "._scale" selection__ "" (getpoint) (setq scale__ (getreal)))
 (drt:scale_width_of_mtext_in_pickset selection__ scale__)
 )
