;;; -*- coding:utf-8; mode:lisp -*-

(ql:quickload :clgplot)

;; Plot line
(clgp:plot '(1 2 3))

;; Plot sin function
(defparameter *x-list* (loop for i from (- pi) to pi by 0.1 collect i))

(clgp:plot (mapcar #'sin *x-list*))

;; Specify x values
(clgp:plot (mapcar #'sin *x-list*) :x-seq *x-list*)

;; Plot multiple functions
(clgp:plots (list (mapcar #'sin *x-list*)
                  (mapcar #'cos *x-list*))
            :x-seqs (list *x-list* *x-list*))

;; Add tan
(clgp:plots (list (mapcar #'sin *x-list*)
                  (mapcar #'cos *x-list*)
                  (mapcar #'tan *x-list*))
            :x-seqs (list *x-list* *x-list* *x-list*))

;; Add domain
(clgp:plots (list (mapcar #'sin *x-list*)
                  (mapcar #'cos *x-list*)
                  (mapcar #'tan *x-list*))
            :x-seqs  (list *x-list* *x-list* *x-list*)
            :x-range (list (- pi) pi)
            :y-range '(-1 1))

;; Add caption of axis
(clgp:plots (list (mapcar #'sin *x-list*)
                  (mapcar #'cos *x-list*)
                  (mapcar #'tan *x-list*))
            :x-seqs  (list *x-list* *x-list* *x-list*)
            :x-range (list (- pi) pi)
            :y-range '(-1 1)
            :title-list '("sin" "cos" "tan")
            :x-label "x"
            :y-label "f(x)")

;; Output to PNG file
(clgp:plots (list (mapcar #'sin *x-list*)
                  (mapcar #'cos *x-list*)
                  (mapcar #'tan *x-list*))
            :x-seqs  (list *x-list* *x-list* *x-list*)
            :x-range (list (- pi) pi)
            :y-range '(-1 1)
            :title-list '("sin" "cos" "tan")
            :x-label "x"
            :y-label "f(x)"
            :output #P"/home/wiz/tmp/clgp-output2.png")

;; Other format
(clgp:plots (list (mapcar #'sin *x-list*)
                  (mapcar #'cos *x-list*)
                  (mapcar #'tan *x-list*))
            :x-seqs  (list *x-list* *x-list* *x-list*)
            :x-range (list (- pi) pi)
            :y-range '(-1 1)
            :title-list '("sin" "cos" "tan")
            :x-label "x"
            :y-label "f(x)"
            ;; :PDF :EPS :EPS-MONOCHROME :PNG :PNG-1280X1024 :PNG-2560X1024 :PNG-MONOCHROME
            :output-format :eps-monochrome
            :output "/home/wiz/tmp/clgp-output.eps")

;; splot
(clgp:splot (lambda (x y) (+ (sin x) (cos y)))
  *x-list* ; x
  *x-list* ; y
  )

;; splot from another view point
(clgp:splot (lambda (x y) (+ (sin x) (cos y)))
  *x-list* ; x
  *x-list* ; y
  :view-point '(20 45) :z-scale 1.5)

;; splot map
(clgp:splot (lambda (x y) (+ (sin x) (cos y)))
  *x-list* ; x
  *x-list* ; y
  :map t)

;; splot-matrix
(defparameter mat
  (make-array '(20 20)
              :initial-contents
              (loop for i from (- pi) to (- pi 0.1) by (/ pi 10) collect
                (loop for j from (- pi) to (- pi 0.1) by (/ pi 10) collect
                  (+ (sin i) (cos j))))))

(clgp:splot-matrix mat)

;; Histogram

;; Random sampling by Box-Muller method
(defun random-normal (&key (mean 0d0) (sd 1d0))
  (let ((alpha (random 1.0d0))
	(beta  (random 1.0d0)))
    (+ (* sd
	  (sqrt (* -2 (log alpha)))
	  (sin (* 2 pi beta)))
       mean)))

(clgp:plot-histogram (loop repeat 3000 collect (random-normal)) ; samples
                     30 ; number of bin
                     )

;; Plot samples with probability density function
(defun pdf-normal (x &key (mu 0) (sd 1))
  (flet ((square (x) (* x x)))
    (/ (exp (- (/ (square (/ (- x mu) sd)) 2)))
       (* (sqrt (* 2 pi)) sd))))

(clgp:plot-histogram-with-pdf (loop repeat 3000 collect (random-normal)) ; samples
                              30 ; number of bin
                              #'pdf-normal)

;; Style
(clgp:plot (mapcar #'sin *x-list*) :style 'lines)
(clgp:plot (mapcar #'sin *x-list*) :style 'points)
(clgp:plot (mapcar #'sin *x-list*) :style 'impulses)

;; Multiple styles
(let* ((rand-x-list (loop repeat 100 collect (- (random (* 2 pi)) pi)))
       (rand-y-list (mapcar (lambda (x) (+ (sin x) (random-normal :sd 0.1d0))) rand-x-list)))
  (clgp:plots (list (mapcar #'sin *x-list*)
                    rand-y-list)
              :x-seqs (list *x-list* rand-x-list)
              :style '(line point)))

(clgp:multiplot ()
  (clgp:plot (mapcar #'sin *x-list*) :style 'lines)
  (clgp:plot (mapcar #'sin *x-list*) :style 'points)
  (clgp:plot (mapcar #'sin *x-list*) :style 'impulses))

(clgp:multiplot (:layout (2 2))
  (clgp:plot (mapcar #'sin *x-list*) :style 'lines)
  (clgp:plot (mapcar #'sin *x-list*) :style 'points)
  (clgp:plot (mapcar #'sin *x-list*) :style 'impulses))
