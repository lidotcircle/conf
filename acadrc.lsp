;; Autocad autolisp file
;; set this script autoload in autocad

;{{{ command: Toggle orthogonal mode
(
 defun C:tgor (/)
 (
  if (< (getvar "ORTHOMODE") 1)
   (progn (setvar "ORTHOMODE" 1)(princ "open orthogonal mode."))
   (progn (setvar "ORTHOMODE" 0)(princ "close orthogonal mode."))
 )
 (princ)
)
;}}} end command

;{{{ command: Toggle object snap
(
 defun C:tgos (/)
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
;}}} end command


;{{{ commmand: Toggle line width
(
  defun C:tglw (/)
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
;}}} end command

;{{{ command: Toggle menu bar
(
 defun C:tgmb (/)
 (
  if (< (getvar "MENUBAR") 1)
   (progn (setvar "MENUBAR" 1)(princ "Open menu bar.")(princ))
   (progn (setvar "MENUBAR" 0)(princ "Close menu bar.")(princ))
 )
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
