(defun type:is2dpoint ( x / ) 
 (and 
  (listp x)
  (= (length x) 2)
  (numberp (nth 0 x))
  (numberp (nth 1 x))
  )
 )

(defun type:is3dpoint ( x / ) 
 (and 
  (listp x)
  (= (length x) 3)
  (numberp (nth 0 x))
  (numberp (nth 1 x))
  (numberp (nth 2 x))
  )
 )
 
(defun type:ispoint (x / ) 
 (or (type:is2dpoint x) (type:is3dpoint x))
 )

(defun type:isentity (x / ) 
 (= (type x) 'ENAME)
 )

(defun type:isentity__ (entity typestr / ) ;{{{
 (and
   (type:isentity entity)
   (= (cdr (assoc 0 (entget entity))) typestr))
 ) ;}}}

(defun type:isline (x / ) 
  (type:isentity__ x "LINE")
  )

(defun type:iscircle (x / ) 
  (type:isentity__ x "CIRCLE")
  )

(defun type:ismtext (x / ) 
  (type:isentity__ x "MTEXT")
  )
