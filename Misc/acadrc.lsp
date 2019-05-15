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
(defun load_vlscript_force (basesym / filename)
  (set basesym T)
  (setq filename (strcat (substr (vl-symbol-name basesym) 6) ".lsp"))
  (if (file_exist (strcat SCR_DIR "\\" filename))
    (progn
      (load (strcat SCR_DIR "\\" filename))
      (princ (strcat "+++++ load <" filename ">"))
      )
    nil
    )
  )

(defun load_vlscript (basesym / filename)
  (if
    (vl-symbol-value basesym)
    T
    (progn
      (set basesym T)
      (setq filename (strcat (substr (vl-symbol-name basesym) 6) ".lsp"))
      (if
        (file_exist (strcat SCR_DIR "\\" filename))
        (progn
          (load (strcat SCR_DIR "\\" filename))
          (princ (strcat "+++++ load <" filename ">"))
          )
        nil
        )
      )
    )
  )
;}}}

;{{{ files required loaded
; script list
(setq script_list (list
    'anna_math
    'anna_basic_func
    'anna_acad_cmds
    'anna_draw_tools
    'anna_log
    'anna_utils
    'anna_draw_tools
    'anna_create_entity
    )
  )
(mapcar 'load_vlscript script_list)
(setq script_reload_force
      (list
        'anna_cur_dev
        )
      )
(mapcar 'load_vlscript_force script_reload_force)
;}}}

;{{{ reload this configuration
(defun C:reloadrc (/)
  (load (strcat (getenv "USERPROFILE")  "\\acadrc.lsp"))
  (princ)
  )

(defun force_load ( / )
  (mapcar 'load_vlscript_force script_list)
  )
;}}}

; Prompt load successfully.
(princ "Load cadrc success.\n")(princ)
