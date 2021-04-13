(defun croot (x)
        (croot-iter 1.00 x))

(defun croot-iter (guess x)
         (if (my-good-enough guess x)
             guess
             (croot-iter (improve guess x)
                        x)))

(defun improve (guess x)
         (/ (+ (/ x (square guess))
               (* 2 guess))
            3))

(defun square (x)
  (* x x))

 (defun cube (x)
         (* x x x))

 (defun my-good-enough (guess x)
         (<= (abs (- (cube guess) x))
            0.001))