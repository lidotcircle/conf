;; Autocad autolisp file
;; set this script autoload in autocad

;{{{ command -- tgr: Toggle orthogonal mode
(
 defun toggle_orthogonal_mode (/)
 (
  if (< (getvar "ORTHOMODE") 1)
   (progn (setvar "ORTHOMODE" 1)(princ "open orthogonal mode."))
   (progn (setvar "ORTHOMODE" 0)(princ "close orthogonal mode."))
 )
 (princ)
)
(
 defun C:tgr (/)
 (toggle_orthogonal_mode)
)
;}}} end command

;{{{ command -- tgs: Toggle object snap
(
 defun toggle_object_snap (/)
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
(
 defun C:tgs (/)
 (toggle_object_snap)
)
;}}} end command

;{{{ command -- tgw: Toggle line width
(
  defun toggle_line_width_display (/)
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
(
 defun C:tgw (/)
 (toggle_line_width_display)
)
;}}} end command

;{{{ command -- tgv: Toggle menu bar
(
 defun toggle_menu_bar (/)
 (
  if (< (getvar "MENUBAR") 1)
   (progn (setvar "MENUBAR" 1)(princ "Open menu bar.")(princ))
   (progn (setvar "MENUBAR" 0)(princ "Close menu bar.")(princ))
 )
)
(
 defun C:tgv (/)
 (toggle_menu_bar)
)
;}}} end command

;{{{ command: reload this file
(
 defun C:reloadrc (/)
  (load (strcat "C:\\Users\\" (getenv "username") "\\acadrc.lsp"))
  (princ "Reload acadrc.lsp file.")
  (princ)
)
;}}} end command

; Prompt load successfully.
(princ "Load cadrc success.\n")(princ)
