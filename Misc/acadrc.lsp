;; Autocad autolisp file
;; set this script autoload in autocad

;{{{ load utils
; check whether file exist, not a directory
(defun file_exist (filen / ret)
  (if (vl-file-directory-p filen) (setq ret 0))
  (if (= (vl-file-size filen) 0) (setq ret 0))
  (if (= ret 0) nil T)
  )

; filename should be "<base>.<extensioni>"
; the first search directory is %USERPROFILE%/vlisp
(setq SCR_DIR (strcat (getenv "USERPROFILE") "\\vlisp"))
(defun load_vlscript (filename / )
  (if (file_exist (strcat SCR_DIR "\\" filename))
    (load (strcat SCR_DIR "\\" filename))
    nil
    )
  )
;}}}

;{{{ files required loaded
; script list
(setq script_list (list
    "acad_cmds.lsp"
    "log.lsp"
    "utils.lsp"
    "create_entity.lsp"
    "cur_dev.lsp"
    )
  )
(mapcar 'load_vlscript script_list)
;}}}

;{{{ reload this configuration
(defun C:reloadrc (/)
  (load (strcat (getenv "USERPROFILE"  "\\acadrc.lsp")))
  (princ)
  )
;}}}

; Prompt load successfully.
(princ "Load cadrc success.\n")(princ)
