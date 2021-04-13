(defun gx (g a x) 
    (if x (gx g (funcall g a (car x)) (cdr x)) a))
(defun редукция2 (g a x)
    (if x (gx g a x) nil))