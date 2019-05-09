; some toggle commands

;{{{ !!! load check !!!
(if ACAD_CMDS_LOAD nil (progn
(setq ACAD_CMDS_LOAD T)
(princ "load acad_cmds.lsp")
;}}}

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


;{{{ !!! end load check !!!
))
;}}}
