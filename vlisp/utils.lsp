;{{{ !!! load check !!!
(if UTILS_LOAD nil (progn
(setq UTILS_LOAD T)
(princ "load utils.lsp")
;}}}

; list filter function
(defun filter_list (list__ function__ / ret elem)
  (if (listp list__) ; the type is 'LIST
    (foreach elem list__
             (if (apply function__ (list elem))
               (setq ret
                     (append ret (list elem))
                     )
               )
             )
    )
  (setq ret ret)
  )

; pickset filter function
(defun filter_ss (ss__ function__ / ret elem ss_index)
  (setq ss_index 0)
  (setq ret (ssadd))
  (if (= (type ss__) 'PICKSET)
    (while (setq elem (ssname ss__ ss_index))
           (setq ss_index (1+ ss_index))
           (if (apply function__ (list elem))
             (setq ret
                   (ssadd elem ret)
                   )
             )
           )
    )
  (setq ret ret)
  )


;{{{ !!! end load check !!!
))
;}}}
