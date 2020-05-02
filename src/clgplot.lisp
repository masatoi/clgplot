;;; -*- Coding: utf-8; Mode: Lisp -*-

(in-package :cl-user)
(defpackage :clgplot
  (:use :cl :iter)
  (:nicknames :clgp)
  (:export :*gnuplot-path* :*tmp-dat-file* :*tmp-gp-file* :*default-terminal*
           :seq
   :plot :plots
   :plot-histogram :plot-histogram-with-pdf
   :splot-list :splot :splot-matrix))

(in-package :clgplot)

(defparameter *gnuplot-path* "gnuplot")
(defparameter *tmp-dat-file* "/tmp/clgplot-tmp.dat")
(defparameter *tmp-gp-file* "/tmp/clgplot-tmp.gp")
(defparameter *default-terminal*
  #-windows "x11"
  #+windows "windows")

;;; Utilities

;; named-let
(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (,tag ,@(mapcar #'cadr var-vals))))

(defun last1 (lst)
  (car (last lst)))

(defun seq (start end &optional (by 1))
  (loop for x from start to end by by collect x))

;;;

(defun run ()
  (uiop:run-program `(,*gnuplot-path* "-persist" ,*tmp-gp-file*)))

(defun dump-gp-stream (stream plot-arg-format
		       &key (x-label nil) (y-label nil)
                            (main nil)
                            (aspect-ratio 1.0)
                            (output nil) (output-format :png)
                            (x-logscale nil) (y-logscale nil)
                            (x-range nil) (y-range nil)
                            (x-range-reverse nil) (y-range-reverse nil) (key t))
  ;; Setting for Output to file
  (cond (output
	 (ecase output-format
	   (:pdf (format stream "set term pdf~%"))
	   (:eps (format stream "set term postscript eps enhanced color~%"))
	   (:eps-monochrome (format stream "set term postscript eps enhanced monochrome~%"))
           (:png-400x320 (format stream "set term png size 400,320~%"))
	   (:png (format stream "set term png~%"))
           (:png-640x480 (format stream "set term png~%"))
	   (:png-1280x1024 (format stream "set term png size 1280,1024~%"))
	   (:png-2560x1024 (format stream "set term png size 2560,1024~%"))
	   (:png-monochrome (format stream "set term png monochrome~%")))
	 (format stream "set output \"~A\"~%" output))
	(t (format stream "set term ~A~%" *default-terminal*)))
  ;; Main title
  (when main (format stream "set title \"~A\"~%" main))
  ;; Axis label
  (if x-label (format stream "set xlabel \"~A\"~%" x-label))
  (if y-label (format stream "set ylabel \"~A\"~%" y-label))
  ;; Input range, Increase direction of X
  (if x-range
      (format stream "set xrange [~f:~f] " (car x-range) (cadr x-range))
      (format stream "set xrange [] "))
  (if x-range-reverse
      (format stream "reverse"))
  (format stream "~%")
  (if y-range
      (format stream "set yrange [~f:~f] " (car y-range) (cadr y-range))
      (format stream "set yrange [] "))
  (if y-range-reverse
      (format stream "reverse"))
  (format stream "~%")
  ;; Use of logscale
  (if x-logscale (format stream "set logscale x~%"))
  (if y-logscale (format stream "set logscale y~%"))
  ;; Aspect ratio
  (if aspect-ratio (format stream "set size ratio ~f~%" aspect-ratio))
  ;; Graph legend enable/disable, or its position
  key
  
  (format stream (concatenate 'string "plot " plot-arg-format)))

(defun dump-gp-file (plot-arg-format
		     &key (x-label nil) (y-label nil)
                          (main nil)
                          (aspect-ratio 1.0)
                          (output nil) (output-format :png)
                          (x-logscale nil) (y-logscale nil)
                          (x-range nil) (y-range nil)
                          (x-range-reverse nil) (y-range-reverse nil) (key t))
  (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :supersede)
    (dump-gp-stream gp-file plot-arg-format
                    :x-label x-label :y-label y-label
		    :main main
                    :aspect-ratio aspect-ratio
                    :output output :output-format output-format
                    :x-logscale x-logscale :y-logscale y-logscale
                    :x-range x-range :y-range y-range
                    :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse :key key)))

(defun plot (y-seq
	     &key (x-seq nil) (title " ") (style 'lines)
                  (x-label nil) (y-label nil)
                  (main nil) (aspect-ratio 1.0)
                  (output nil) (output-format :png)
                  (x-logscale nil) (y-logscale nil)
                  (x-range nil) (y-range nil)
                  (x-range-reverse nil) (y-range-reverse nil) (key t)
                  (stream nil))
  (when (null x-seq)
    (setf x-seq (loop for i from 0 below (length y-seq) collect i)))
  (unless (= (length x-seq) (length y-seq))
    (error "sequence length mismatch detected between y-seq and x-seq."))
  ;; Output to DAT file
  (with-open-file (dat-file *tmp-dat-file* :direction :output :if-exists :supersede)
    (iter (for x in-sequence x-seq)
      (for y in-sequence y-seq)
      (format dat-file "~f ~f~%" x y)))

  (let ((plot-arg-string (format nil "\"~A\" using 1:2 with ~A title \"~A\""
				 *tmp-dat-file* (string-downcase (string style)) title)))
    (if stream
	(progn
	  (dump-gp-stream stream
			  plot-arg-string
			  :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			  :main main :output output :output-format output-format
			  :x-logscale x-logscale :y-logscale y-logscale
			  :x-range x-range :y-range y-range
			  :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			  :key key)
	  (finish-output stream))
	;; Output to GP file
	(progn
	  (dump-gp-file plot-arg-string
			:x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			:main main :output output :output-format output-format
			:x-logscale x-logscale :y-logscale y-logscale
			:x-range x-range :y-range y-range
			:x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			:key key)      
	  ;; Call Gnuplot
	  (run)))))

(defun comma-separated-concatenate (string-list)
  (reduce (lambda (s1 s2) (concatenate 'string s1 "," s2))
          string-list))

;; 2つの軸を使いたいときは、
;; (plots (list list1 list2) :axis-list '(x1y1 x1y2))
;; のようにする。それ以上の数、スケールの異なるグラフを重ねて表示する場合は、次で定義する正規化機能付きのplot-lists-with-normalizeで表示する
;; というか、y-listsに何か関数を噛ませればいいのか。normalize-listを定義したので、これをmapcarすればいい。

(defun plots (y-seqs
	      &key (x-seqs nil) (title-list nil) (style 'lines) ; style accepts symbol string and list of symbols and strings
                   (x-label nil) (y-label nil)
                   (main nil) (aspect-ratio 1.0)
                   (output nil) (output-format :png)
                   (x-logscale nil) (y-logscale nil)
                   (x-range nil) (y-range nil)
                   (x-range-reverse nil) (y-range-reverse nil) (key t)
                   (axis-list nil) ; When axis-list is nil, use x1y1 axis for all plots
                   (stream nil))
  
  (when (null x-seqs)
    (setf x-seqs (make-list (length y-seqs))))
  
  (iter (for i from 0) (for y-seq in-sequence y-seqs) (for x-seq in-sequence x-seqs)
    (when (null x-seq)
      (setf x-seq (loop for i from 0 below (length y-seq) collect i)))
    (unless (= (length x-seq) (length y-seq))
      (error "sequence length mismatch detected between y-seq and x-seq."))

    ;; Output to DAT file
    (with-open-file (dat-file (format nil "~A.~A" *tmp-dat-file* i)
                              :direction :output :if-exists :supersede)
      (iter (for x in-sequence x-seq) (for y in-sequence y-seq)
        (format dat-file "~f ~f~%" x y))))

  (if (and (not (null axis-list))
	   (not (= (length y-seqs) (length axis-list))))
      (error "sequence length mismatch detected between y-seqs and axis-list."))

  (if (and (listp style) (not (= (length y-seqs) (length style))))
      (error "list length mismatch detected between y-lists and style."))
  
  ;; Output to GP file
  (let ((plot-arg-string
          (comma-separated-concatenate
           (iter (for i from 0 below (length y-seqs))
             (collect
                 (format nil "\"~A.~A\" using 1:2 with ~A title \"~A\" axis ~A"
                         *tmp-dat-file*
                         i
                         (if (listp style)
                             (string-downcase (string (nth i style)))
                             (string-downcase (string style)))                      
                         (if (null title-list) " " (nth i title-list))
                         (if (null axis-list) "x1y1" (string-downcase (string (nth i axis-list))))))))))
    (if stream
	(progn (dump-gp-stream stream
			       plot-arg-string
			       :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			       :main main :output output :output-format output-format
			       :x-logscale x-logscale :y-logscale y-logscale
			       :x-range x-range :y-range y-range
			       :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			       :key key)
	       (finish-output stream))
	(progn (dump-gp-file plot-arg-string
			     :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			     :main main :output output :output-format output-format
			     :x-logscale x-logscale :y-logscale y-logscale
			     :x-range x-range :y-range y-range
			     :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			     :key key)
	       ;; Call Gnuplot
               (run)))))


;; ;;; normalize list between [0,1]
;; (defun normalize-list (list)
;;   (let ((max-elem (loop for x in list maximize x))
;; 	(min-elem (loop for x in list minimize x)))
;;     (if (> min-elem 0)
;; 	(mapcar (lambda (elem)
;; 		  (/ (- elem min-elem)
;; 		     (abs (- max-elem min-elem))))
;; 		list)
;; 	(mapcar (lambda (elem)
;; 		  (/ (+ elem min-elem)
;; 		     (abs (- max-elem min-elem))))
;; 		list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; histogram ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Separate interval (a,b) equally to n bins, then return index of the bin which x belongs to.
(defun histogram-lem1 (x a b n)
  (if (or (< x a) (< b x))
      nil
      (if (= x b)
	  (1- n)
	  (let ((span (/ (- b a) n)))
	    (nlet itr ((i 1))
	      (if (<= x (+ a (* i span)))
		  (1- i)
		  (itr (1+ i))))))))

(defun search-min-max (list &key (min nil) (max nil))
  (cond ((null list) (values min max))
	((null min) (search-min-max (cdr list) :min (car list) :max (car list)))
	((< (car list) min) (search-min-max (cdr list) :min (car list) :max max))
	((> (car list) max) (search-min-max (cdr list) :min min :max (car list)))
	(t (search-min-max (cdr list) :min min :max max))))

;; サンプルのリストsamplesをrange幅で分割し,それぞれの区間での登場回数を数え上げる.
;; 各区間のサンプル登場回数のリストを返す.
(defun plot-histogram (samples n-of-bin &key (output nil)
                                             (x-range nil) (y-range nil)
                                             (x-logscale nil) (y-logscale nil))
  (multiple-value-bind (a b)
      (search-min-max samples)
    (let ((counter (make-list n-of-bin :initial-element 0))
	  (span (/ (- b a) n-of-bin)))
      ;; 数え上げ
      (dolist (x samples)
	(let ((bin (histogram-lem1 x a b n-of-bin)))
	  (if bin (incf (nth bin counter)))))
      ;; datファイルに出力
      (with-open-file (dat-file *tmp-dat-file* :direction :output :if-exists :supersede)
	(loop for i from 0  to (1- n-of-bin) do
                 (format dat-file "~f ~A~%"
                         (/ (+ (+ a (* i span)) (+ a (* (1+ i) span))) 2.0)
                         (nth i counter))))
      ;; gpファイルに出力
      (dump-gp-file (format nil "\"~A\" using 1:2:(~f) with boxes fs solid 0.2 title \" \"" *tmp-dat-file* span)
		    :output output :x-range x-range :y-range y-range :x-logscale x-logscale :y-logscale y-logscale)
      ;; gnuplotを呼び出し
      (run))))

(defun pdf-normal (x &key (mu 0) (sd 1))
  (flet ((square (x) (* x x)))
    (/ (exp (- (/ (square (/ (- x mu) sd)) 2)))
       (* (sqrt (* 2 pi)) sd))))

(defun plot-histogram-with-pdf (samples n-of-bin pdf &key (output nil)
                                                          (x-range nil) (y-range nil)
                                                          (x-logscale nil) (y-logscale nil))
  (let ((n-of-samples (length samples)))
    (multiple-value-bind (a b)
	(search-min-max samples)
      (let ((counter (make-list n-of-bin :initial-element 0))
	    (span (/ (- b a) n-of-bin)))
	;; 数え上げ
	(dolist (x samples)
	  (let ((bin (histogram-lem1 x a b n-of-bin)))
	    (if bin (incf (nth bin counter)))))
	;; datファイルに出力
	(with-open-file (dat-file *tmp-dat-file* :direction :output :if-exists :supersede)
	  (loop for i from 0  to (1- n-of-bin) do
                   (format dat-file "~f ~f~%"
                           (/ (+ (+ a (* i span)) (+ a (* (1+ i) span))) 2.0)
                           ;;(nth i counter)
                           (/ (nth i counter) (* span n-of-samples))
                           )))
	(with-open-file (dat-file (concatenate 'string *tmp-dat-file* ".pdfdat")
				  :direction :output :if-exists :supersede)
	  (loop for i from a to b by (/ (- b a) 100) do
                   (format dat-file "~f ~f~%"
                           i
                           (funcall pdf i))))
	;; gpファイルに出力
	(dump-gp-file (format nil "\"~A\" using 1:2:(~f) with boxes fs solid 0.2 title \" \", \"~A\" using 1:2 with lines title \" \""
			      *tmp-dat-file* span (concatenate 'string *tmp-dat-file* ".pdfdat"))
		      :output output :x-range x-range :y-range y-range :x-logscale x-logscale :y-logscale y-logscale)
	;; gnuplotを呼び出し
	(run)))))

;;; multiplotのための関数群

(defun dump-gp-file-append (plot-arg-format
			    &key (x-label nil) (y-label nil)
                                 (main nil) (aspect-ratio 1.0)
                                 (output nil) (output-format :png)
                                 (x-logscale nil) (y-logscale nil)
                                 (x-range nil) (y-range nil)
                                 (x-range-reverse nil) (y-range-reverse nil) (key t))
  (declare (ignore output output-format))
  (with-open-file (gp-file *tmp-gp-file* :direction :output
                                         :if-exists :append :if-does-not-exist :create)

    (when main (format gp-file "set title \"~A\"~%" main))
    ;; 軸のラベル
    (if x-label (format gp-file "set xlabel \"~A\"~%" x-label))
    (if y-label (format gp-file "set ylabel \"~A\"~%" y-label))
    
    ;; 範囲指定、軸の向きの指定
    (if x-range
	(format gp-file "set xrange [~f:~f] " (car x-range) (cadr x-range))
	(format gp-file "set xrange [] "))
    (if x-range-reverse
	(format gp-file "reverse"))
    (format gp-file "~%")
    
    (if y-range
	(format gp-file "set yrange [~f:~f] " (car y-range) (cadr y-range))
	(format gp-file "set yrange [] "))
    (if y-range-reverse
	(format gp-file "reverse"))
    (format gp-file "~%")
    
    ;; 対数スケール
    (if x-logscale (format gp-file "set logscale x~%"))
    (if y-logscale (format gp-file "set logscale y~%"))
    
    ;; アスペクト比
    (if aspect-ratio (format gp-file "set size ratio ~f~%" aspect-ratio))

    ;; 凡例の位置、あるいは出すかどうか
    key
    
    ;; プロット用コマンド
    (format gp-file (concatenate 'string "plot " plot-arg-format "~%"))))

;; nlisp-wrapper互換のgnuplot出力用ルーチン
(defun plot-list-for-multiplot (plot-id y-list
				&key (x-list nil) (title " ") (style 'lines)
                                     (x-label nil) (y-label nil)
                                     (main nil) (aspect-ratio 1.0)
                                     (output nil) (output-format :png)
                                     (x-logscale nil) (y-logscale nil)
                                     (x-range nil) (y-range nil)
                                     (x-range-reverse nil) (y-range-reverse nil) (key t))
  (if (null x-list) (setf x-list (loop for i from 0 to (1- (length y-list)) collect i)))
  ;; 長さチェック
  (if (not (= (length x-list) (length y-list)))
      (error "list length mismatch detected between y-list and x-list."))
  ;; datファイルに出力
  (with-open-file (dat-file (format nil "~A.plot-id~A" *tmp-dat-file* plot-id)
			    :direction :output :if-exists :supersede)
    (mapc #'(lambda (x y) (format dat-file "~f ~f~%" x y)) x-list y-list))
  ;; gpファイルに出力
  (dump-gp-file-append (format nil "\"~A.plot-id~A\" using 1:2 with ~A title \"~A\""
			       *tmp-dat-file* plot-id (string-downcase (symbol-name style)) title)
		       :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
		       :main main :output output :output-format output-format
		       :x-logscale x-logscale :y-logscale y-logscale
		       :x-range x-range :y-range y-range
		       :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
		       :key key))

(defun plot-lists-for-multiplot (plot-id y-lists
				 &key (x-lists nil) (title-list nil) (style 'lines)
                                      (x-label nil) (y-label nil)
                                      (main nil) (aspect-ratio 1.0)
                                      (output nil) (output-format :png)
                                      (x-logscale nil) (y-logscale nil)
                                      (x-range nil) (y-range nil)
                                      (x-range-reverse nil) (y-range-reverse nil) (key t))
  (loop for i from 0 to (1- (length y-lists)) do
           (let ((x-list (nth i x-lists))
                 (y-list (nth i y-lists)))
             (if (null x-lists) (setf x-list (loop for i from 0 to (1- (length y-list)) collect i)))
             ;; 長さチェック
             (if (not (= (length x-list) (length y-list)))
                 (error "list length mismatch detected between y-list and x-list."))
             ;; datファイルに出力
             (with-open-file (dat-file (format nil "~A.plot-id~A.~A" *tmp-dat-file* plot-id i)
                                       :direction :output :if-exists :supersede)
               (mapc #'(lambda (x y) (format dat-file "~f ~f~%" x y)) x-list y-list))))
  
  ;; gpファイルに出力
  (dump-gp-file-append (comma-separated-concatenate
			(loop for i from 0 to (1- (length y-lists)) collect
                                 (format nil "\"~A.plot-id~A.~A\" using 1:2 with ~A title \"~A\""
                                         *tmp-dat-file* plot-id i (string-downcase (symbol-name style))
                                         (if (null title-list) " " (nth i title-list)))))
		       :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
		       :main main :output output :output-format output-format
		       :x-logscale x-logscale :y-logscale y-logscale
		       :x-range x-range :y-range y-range
		       :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
		       :key key))

(defmacro multiplot (&body body)
  `(progn
     (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :supersede)
       (format gp-file "set multiplot layout ~A,1~%set format y \"%.3f\"~%" ,(length body)))
     (loop for plot-id from 0 to ,(1- (length body)) do
              (cond ((eq (car (nth plot-id ',body)) 'plot)
                     (apply #'plot-list-for-multiplot (cons plot-id (mapcar #'eval (cdr (nth plot-id ',body))))))
                    ((eq (car (nth plot-id ',body)) 'plots)
                     (apply #'plot-lists-for-multiplot (cons plot-id (mapcar #'eval (cdr (nth plot-id ',body))))))))
     (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :append)
       (format gp-file "unset multiplot~%"))
     ;; gnuplotを呼び出し
     (run)))

;;; 3-dimension plot
(defun dump-gp-file-3d
    (plot-arg-format
     &key
     (x-label nil) (y-label nil) (z-label nil)
     (main nil) (aspect-ratio 1.0)
     (output nil) (output-format :png)
     (x-logscale nil) (y-logscale nil) (z-logscale nil)
     (x-range nil) (y-range nil) (z-range nil)
     (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
     (view-point '(60 30)) (magnification 1) (z-scale 1)
     (palette 'jet)
     (key t) (map nil))
  (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :supersede)
    ;; 図の設定
    ;; 画像出力する場合の設定
    (cond (output
	   (ecase output-format
	     (:pdf (format gp-file "set term pdf~%"))
	     (:eps (format gp-file "set term postscript eps enhanced color~%"))
	     (:eps-monochrome (format gp-file "set term postscript eps enhanced monochrome~%"))
             (:png-400x320 (format gp-file "set term png size 400,320~%"))
             (:png (format gp-file "set term png~%"))
             (:png-640x480 (format gp-file "set term png~%"))
	     (:png-1280x1024 (format gp-file "set term png size 1280,1024~%"))
	     (:png-2560x1024 (format gp-file "set term png size 2560,1024~%"))
	     (:png-monochrome (format gp-file "set term png monochrome~%")))
	   (format gp-file "set output \"~A\"~%" output))
	  (t (format gp-file "set term ~A~%" *default-terminal*)))

    (when main (format gp-file "set title \"~A\"~%" main))
    ;; 軸のラベル
    (if x-label (format gp-file "set xlabel \"~A\"~%" x-label))
    (if y-label (format gp-file "set ylabel \"~A\"~%" y-label))
    (if z-label (format gp-file "set zlabel \"~A\"~%" z-label))
    
    ;; 範囲指定、軸の向きの指定
    (if x-range
	(format gp-file "set xrange [~f:~f] " (car x-range) (cadr x-range))
	(format gp-file "set xrange [] "))
    (if x-range-reverse
	(format gp-file "reverse"))
    (format gp-file "~%")
    
    (if y-range
	(format gp-file "set yrange [~f:~f] " (car y-range) (cadr y-range))
	(format gp-file "set yrange [] "))
    (if y-range-reverse
	(format gp-file "reverse"))
    (format gp-file "~%")

    (if z-range
	(format gp-file "set zrange [~f:~f] " (car z-range) (cadr z-range))
	(format gp-file "set zrange [] "))
    (if z-range-reverse
	(format gp-file "reverse"))
    (format gp-file "~%")
    
    ;; 対数スケール
    (if x-logscale (format gp-file "set logscale x~%"))
    (if y-logscale (format gp-file "set logscale y~%"))
    (if z-logscale (format gp-file "set logscale z~%"))
    
    ;; アスペクト比
    (if aspect-ratio (format gp-file "set size ratio ~f~%" aspect-ratio))

    ;; 視点指定
    (format gp-file "set view ~A, ~A, ~A, ~A~%"
            (car view-point) (cadr view-point) magnification z-scale)
    
    ;; 凡例の位置、あるいは出すかどうか
    key
    
    ;; カラースキーム指定
    (ecase palette
      (:greys (format gp-file "set palette defined ( 0 0 0 0, 1 1 1 1 )~%"))
      (:greys-invert (format gp-file "set palette defined ( 1 1 1 1, 0 0 0 0 )~%"))
      (:jet (format gp-file "set palette defined ( 0 '#000090',1 '#000fff',2 '#0090ff',3 '#0fffee',4 '#90ff70',5 '#ffee00',6 '#ff7000',7 '#ee0000',8 '#7f0000')~%")))

    (if map
	(progn
	  (format gp-file "set size square~%")
	  (format gp-file "set pm3d map~%"))
	(progn
	  (format gp-file "set ticslevel 0~%")
	  (format gp-file "set pm3d~%")))

    ;; プロット用コマンド
    (format gp-file (concatenate 'string "splot " plot-arg-format))))

(defun splot-list (z-func x-list y-list
		   &key (title " ") (style 'lines)
                        (x-label nil) (y-label nil) (z-label nil)
                        (main nil) (aspect-ratio 1.0)
                        (output nil) (output-format :png)
                        (x-logscale nil) (y-logscale nil) (z-logscale nil)
                        (x-range nil) (y-range nil) (z-range nil)
                        (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
                        (view-point '(60 30)) (magnification 1) (z-scale 1)
                        (palette :jet) (key t) (map nil))
  ;; Output to DAT file
  (with-open-file (dat-file *tmp-dat-file* :direction :output :if-exists :supersede)
    (mapc #'(lambda (x)
              (mapc #'(lambda (y)
                        (format dat-file "~f ~f ~f~%" x y (funcall z-func x y)))
                    y-list)
              (format dat-file "~%")) ; xの値が変わるごとに改行を入れることでグリッドデータとして認識される
          x-list))
  ;; Output to DAT file
  (dump-gp-file-3d
   (if map
       (format nil "\"~A\" using 1:2:3 title \"~A\""
	       *tmp-dat-file* title)
       (format nil "\"~A\" using 1:2:3 with ~A title \"~A\""
	       *tmp-dat-file* (string-downcase (symbol-name style)) title))
   :x-label x-label :y-label y-label :z-label z-label
   :main main :aspect-ratio aspect-ratio
   :output output :output-format output-format
   :x-logscale x-logscale :y-logscale y-logscale :z-logscale z-logscale
   :x-range (if map
		(list (car x-list) (last1 x-list))
		x-range)
   :y-range (if map
		(list (car y-list) (last1 y-list))
		y-range)
   :z-range z-range
   :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse :z-range-reverse z-range-reverse
   :view-point view-point :magnification magnification :z-scale z-scale
   :palette palette :key key :map map)
  (run))

(defun splot (z-func x-seq y-seq
              &key (title " ") (style 'lines)
                   (x-label nil) (y-label nil) (z-label nil)
                   (aspect-ratio 1.0)
                   (output nil) (output-format :png)
                   (x-logscale nil) (y-logscale nil) (z-logscale nil)
                   (x-range nil) (y-range nil) (z-range nil)
                   (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
                   (view-point '(60 30)) (magnification 1) (z-scale 1)
                   (palette :jet) (key t) (map nil))
  (splot-list z-func (coerce x-seq 'list) (coerce y-seq 'list)
              :title title :style style
              :x-label x-label :y-label y-label :z-label z-label
              :aspect-ratio aspect-ratio
              :output output :output-format output-format
              :x-logscale x-logscale :y-logscale y-logscale :z-logscale z-logscale
              :x-range x-range :y-range y-range :z-range z-range
              :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse :z-range-reverse z-range-reverse
              :view-point view-point :magnification magnification :z-scale z-scale
              :palette palette :key key :map map))

(defun splot-matrix (matrix &key (title " ") (style 'lines)
                                 (x-label nil) (y-label nil) (z-label nil)
                                 (output nil) (output-format :png)
                                 (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
                                 (palette :jet) (key t))
  (flet ((seq-row (start end)
	   (nlet iteration ((i start) (product nil))
	     (if (> i end)
                 (reverse product)
                 (iteration (1+ i) (cons (+ i 0.999) (cons (+ i 0.999) (cons i (cons i product))))))))
	 (seq-col (start end)
	   (nlet iteration ((i start) (product nil))
	     (if (> i end)
                 (reverse product)
                 (iteration (1+ i) (cons (+ i 0.999) (cons i (cons (+ i 0.999) (cons i product)))))))))
    (splot-list (lambda (x y)
		  (aref matrix (truncate x) (truncate y)))
		(seq-row 0 (1- (array-dimension matrix 0)))
		(seq-col 0 (1- (array-dimension matrix 1)))
		:title title :style style
		:x-label x-label :y-label y-label :z-label z-label
		:output output :output-format output-format
		:x-range-reverse x-range-reverse :y-range-reverse y-range-reverse :z-range-reverse z-range-reverse
		:aspect-ratio 1.0 :palette palette :map t :key key)))
