; logs

(defun current_date ( / ) ;{{{
  (menucmd "M=$(edtime, $(getvar, date),#### YYYY-M-D\",\" DDDD)")
  ) ;}}}

(setq windows 0)
(if (= (getenv "OS") "Windows_NT") ; this must be windows.
  (progn 
    (setq windows 1)
    (setq HOME (getenv "USERPROFILE"))
    (setq DEBUG_FILE (strcat HOME "\\alisprc.log"))
    (defun sc_log (msg / fd_log) ; log
      (setq fd_log (open DEBUG_FILE "a"))
      (write-line (current_date) fd_log)
      (write-line  (vl-princ-to-string msg) fd_log)
      (close fd_log)(princ)
      )
    (defun cl_log ( / log_fd) ; truncate log file to 0
      (setq log_fd (open DEBUG_FILE "w"))
      (write-line "" log_fd)
      (close log_fd)(princ)
      )
    )
  ) ;}}}

; error msg
(defun func_error_msg (func_name msg / line_data) ;{{{
  (princ (strcat "Functioin: " function_name "\"" msg "\""))
  (vl-exit-with-error msg)
  (princ)
  ) ;}}}
