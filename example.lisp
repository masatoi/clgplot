;;; -*- coding:utf-8; mode:lisp -*-

;; 01 直線
(clgp:plot-list '(1 2 3))

;; 02 sin関数
(defparameter *x-list* (loop for i from (- pi) to pi by 0.1 collect i))
(clgp:plot-list (mapcar #'sin *x-list*))

;; 03 x軸に定義域を指定
(clgp:plot-list (mapcar #'sin *x-list*) :x-list *x-list*)

;; 04 複数プロット
(clgp:plot-lists (list (mapcar #'sin *x-list*)
                       (mapcar #'cos *x-list*))
                 :x-lists (list *x-list* *x-list*))

;; 05 tanも追加
(clgp:plot-lists (list (mapcar #'sin *x-list*)
                       (mapcar #'cos *x-list*)
                       (mapcar #'tan *x-list*))
                 :x-lists (list *x-list* *x-list* *x-list*))

;; 06 表示範囲を指定
(clgp:plot-lists (list (mapcar #'sin *x-list*)
                       (mapcar #'cos *x-list*)
                       (mapcar #'tan *x-list*))
                 :x-lists (list *x-list* *x-list* *x-list*)
                 :x-range (list (- pi) pi)
                 :y-range '(-1 1))

;; 07 キャプション追加
(clgp:plot-lists (list (mapcar #'sin *x-list*)
                       (mapcar #'cos *x-list*)
                       (mapcar #'tan *x-list*))
                 :x-lists (list *x-list* *x-list* *x-list*)
                 :x-range (list (- pi) pi)
                 :y-range '(-1 1)
                 :title-list '("sin" "cos" "tan")
                 :x-label "x"
                 :y-label "f(x)")

;; PNGファイル出力
(clgp:plot-lists (list (mapcar #'sin *x-list*)
                       (mapcar #'cos *x-list*)
                       (mapcar #'tan *x-list*))
                 :x-lists (list *x-list* *x-list* *x-list*)
                 :x-range (list (- pi) pi)
                 :y-range '(-1 1)
                 :title-list '("sin" "cos" "tan")
                 :x-label "x"
                 :y-label "f(x)"
                 :output "/home/wiz/tmp/clgp-output.png")

;; フォーマット指定
(clgp:plot-lists (list (mapcar #'sin *x-list*)
                       (mapcar #'cos *x-list*)
                       (mapcar #'tan *x-list*))
                 :x-lists (list *x-list* *x-list* *x-list*)
                 :x-range (list (- pi) pi)
                 :y-range '(-1 1)
                 :title-list '("sin" "cos" "tan")
                 :x-label "x"
                 :y-label "f(x)"
                 ;; :PDF :EPS :EPS-MONOCHROME :PNG :PNG-1280X1024 :PNG-2560X1024 :PNG-MONOCHROME
                 :output-format :eps
                 :output "/home/wiz/tmp/clgp-output.eps")

;; 08 splot
(clgp:splot-list (lambda (x y) (+ (sin x) (cos y)))
                 *x-list* ; x-list
                 *x-list* ; y-list
                 )

;; 09 splot map
(clgp:splot-list (lambda (x y) (+ (sin x) (cos y)))
                 *x-list* ; x-list
                 *x-list* ; y-list
                 :map t)

;; 10 splot-matrix
(defparameter mat (make-array '(10 10)
                              :initial-contents
                              (loop for i from (- pi) to (- pi 0.1) by (/ pi 5) collect
                                (loop for j from (- pi) to (- pi 0.1) by (/ pi 5) collect
                                  (+ (sin i) (cos j))))))

(clgp:splot-matrix mat)

;; 11 ヒストグラム

;; Box-Muller法による1次元正規分布のサンプリング
(defun random-normal (&key (mean 0d0) (sd 1d0))
  (let ((alpha (random 1.0d0))
	(beta  (random 1.0d0)))
    (+ (* sd
	  (sqrt (* -2 (log alpha)))
	  (sin (* 2 pi beta)))
       mean)))

(clgp:plot-histogram (loop repeat 100000 collect (random-normal)) ; samples
                     100 ; number of bin
                     )
