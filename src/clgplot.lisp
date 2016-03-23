;;; -*- Coding: utf-8; Mode: Lisp -*-

(in-package :cl-user)
(defpackage :clgplot
  (:use :cl)
  (:nicknames :clgp)
  (:export :plot-list :plot-lists
           :plot-histogram :plot-histogram-with-pdf
           :splot-list :splot-matrix
   ))

(in-package :clgplot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface to GNUPLOT ;;;
;;;  gnuplot 4 required  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gnuplot-path* "gnuplot")
(defparameter *tmp-dat-file* "/tmp/gnuplot-tmp.dat")
(defparameter *tmp-gp-file* "/tmp/gnuplot-tmp.gp")

;;; Utilities

;; named-let
(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (declare (optimize (speed 3))) ; for tail recursion optimization
     (,tag ,@(mapcar #'cadr var-vals))))

;;; 
(defun dump-gp-file (plot-arg-format
		     &key (x-label nil) (y-label nil)
		     (aspect-ratio 1.0)
		     (output nil) (output-format :png)
		     (x-logscale nil) (y-logscale nil)
		     (x-range nil) (y-range nil)
		     (x-range-reverse nil) (y-range-reverse nil) (key t))
  (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :supersede)
    ;; 図の設定
    ;; 画像出力する場合の設定
    (cond (output
	   (ecase output-format
	     (:pdf (format gp-file "set term pdf~%"))
	     (:eps (format gp-file "set term postscript eps enhanced color~%"))
	     (:eps-monochrome (format gp-file "set term postscript eps enhanced monochrome~%"))
	     (:png (format gp-file "set term png~%"))
	     (:png-1280x1024 (format gp-file "set term png size 1280,1024~%"))
	     (:png-2560x1024 (format gp-file "set term png size 2560,1024~%"))
	     (:png-monochrome (format gp-file "set term png monochrome~%")))
	   (format gp-file "set output \"~A\"~%" output))
	  (t (format gp-file "set term x11~%")))
    
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
    (format gp-file (concatenate 'string "plot " plot-arg-format))))

(defun dump-gp-stream (stream plot-arg-format
		       &key (x-label nil) (y-label nil)
		       (aspect-ratio 1.0)
		       (output nil) (output-format :png)
		       (x-logscale nil) (y-logscale nil)
		       (x-range nil) (y-range nil)
		       (x-range-reverse nil) (y-range-reverse nil) (key t))
  ;; 図の設定
  ;; 画像出力する場合の設定
  (cond (output
	 (ecase output-format
	   (:pdf (format stream "set term pdf~%"))
	   (:eps (format stream "set term postscript eps enhanced color~%"))
	   (:eps-monochrome (format stream "set term postscript eps enhanced monochrome~%"))
	   (:png (format stream "set term png~%"))
	   (:png-1280x1024 (format stream "set term png size 1280,1024~%"))
	   (:png-2560x1024 (format stream "set term png size 2560,1024~%"))
	   (:png-monochrome (format stream "set term png monochrome~%")))
	 (format stream "set output \"~A\"~%" output))
	(t (format stream "set term x11~%")))
  
  ;; 軸のラベル
  (if x-label (format stream "set xlabel \"~A\"~%" x-label))
  (if y-label (format stream "set ylabel \"~A\"~%" y-label))
  
  ;; 範囲指定、軸の向きの指定
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
  
  ;; 対数スケール
  (if x-logscale (format stream "set logscale x~%"))
  (if y-logscale (format stream "set logscale y~%"))
  
  ;; アスペクト比
  (if aspect-ratio (format stream "set size ratio ~f~%" aspect-ratio))

  ;; 凡例の位置、あるいは出すかどうか
  key
  
  ;; プロット用コマンド
  (format stream (concatenate 'string "plot " plot-arg-format)))

(defun plot-list (y-list
		  &key (x-list nil) (title " ") (style 'lines)
		    (x-label nil) (y-label nil)
		    (aspect-ratio 1.0)
		    (output nil) (output-format :png)
		    (x-logscale nil) (y-logscale nil)
		    (x-range nil) (y-range nil)
		    (x-range-reverse nil) (y-range-reverse nil) (key t)
		    (stream nil))
  (if (null x-list) (setf x-list (loop for i from 0 to (1- (length y-list)) collect i)))
  ;; 長さチェック
  (if (not (= (length x-list) (length y-list)))
    (error "list length mismatch detected between y-list and x-list."))
  ;; datファイルに出力
  (with-open-file (dat-file *tmp-dat-file* :direction :output :if-exists :supersede)
    (mapc #'(lambda (x y) (format dat-file "~f ~f~%" x y)) x-list y-list))

  (let ((plot-arg-string (format nil "\"~A\" using 1:2 with ~A title \"~A\""
				 *tmp-dat-file* (string-downcase (symbol-name style)) title)))
    (if stream
      (progn
	(dump-gp-stream stream
			plot-arg-string
			:x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			:output output :output-format output-format
			:x-logscale x-logscale :y-logscale y-logscale
			:x-range x-range :y-range y-range
			:x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			:key key)
	(finish-output stream))
      ;; gpファイルに出力
      (progn
	(dump-gp-file plot-arg-string
		      :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
		      :output output :output-format output-format
		      :x-logscale x-logscale :y-logscale y-logscale
		      :x-range x-range :y-range y-range
		      :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
		      :key key)      
	;; gnuplotを呼び出し
	(external-program:run *gnuplot-path* (list "-persist" *tmp-gp-file*))))))

(defun comma-separated-concatenate (string-list)
  (apply #'concatenate 'string
	 (nlet iter ((string-list string-list)
		     (product nil))
	   (if (null (cdr string-list))
	       (nreverse (cons (car string-list) product))
	       (iter (cdr string-list) (cons "," (cons (car string-list) product)))))))

;; 2つの軸を使いたいときは、
;; (plot-lists (list list1 list2) :axis-list '(x1y1 x1y2))
;; のようにする。それ以上の数、スケールの異なるグラフを重ねて表示する場合は、次で定義する正規化機能付きのplot-lists-with-normalizeで表示する
;; というか、y-listsに何か関数を噛ませればいいのか。normalize-listを定義したので、これをmapcarすればいい。

;; 線ごとにスタイルを変えたいときのために、styleにlistを指定することもできるようにする
(defun plot-lists (y-lists
		   &key (x-lists nil) (title-list nil) (style 'lines)
		     (x-label nil) (y-label nil)
		     (aspect-ratio 1.0)
		     (output nil) (output-format :png)
		     (x-logscale nil) (y-logscale nil)
		     (x-range nil) (y-range nil)
		     (x-range-reverse nil) (y-range-reverse nil) (key t)
		     (axis-list nil) ; nilにすると全部x1y1にする
		     (stream nil))
  (loop for i from 0 to (1- (length y-lists)) do
    (let ((x-list (nth i x-lists))
	  (y-list (nth i y-lists)))
      (if (null x-lists) (setf x-list (loop for i from 0 to (1- (length y-list)) collect i)))
      ;; 長さチェック
      (if (not (= (length x-list) (length y-list)))
	(error "list length mismatch detected between y-list and x-list."))
      ;; datファイルに出力
      (with-open-file (dat-file (format nil "~A.~A" *tmp-dat-file* i)
				:direction :output :if-exists :supersede)
	(mapc #'(lambda (x y) (format dat-file "~f ~f~%" x y)) x-list y-list))))

  (if (and (not (null axis-list))
	   (not (= (length y-lists) (length axis-list))))
    (error "list length mismatch detected between y-lists and axis-list."))

  (if (and (listp style) (not (= (length y-lists) (length style))))
    (error "list length mismatch detected between y-lists and style."))
  
  ;; gpファイルに出力
  (let ((plot-arg-string (comma-separated-concatenate
			  (loop for i from 0 to (1- (length y-lists)) collect
			    (format nil "\"~A.~A\" using 1:2 with ~A title \"~A\" axis ~A"
				    *tmp-dat-file* i (if (symbolp style)
						       (string-downcase (symbol-name style))
						       (string-downcase (symbol-name (nth i style))))
						     (if (null title-list) " " (nth i title-list))
						     (if (null axis-list) "x1y1" (string-downcase (symbol-name (nth i axis-list)))))))))
    (if stream
      (progn (dump-gp-stream stream
			     plot-arg-string
			     :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			     :output output :output-format output-format
			     :x-logscale x-logscale :y-logscale y-logscale
			     :x-range x-range :y-range y-range
			     :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			     :key key)
	     (finish-output stream))
      (progn (dump-gp-file plot-arg-string
			   :x-label x-label :y-label y-label :aspect-ratio aspect-ratio
			   :output output :output-format output-format
			   :x-logscale x-logscale :y-logscale y-logscale
			   :x-range x-range :y-range y-range
			   :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
			   :key key)
	     ;; gnuplotを呼び出し
	     (external-program:run *gnuplot-path* (list "-persist" *tmp-gp-file*))))))

;;; listを[0,1]の範囲に正規化する
(defun normalize-list (list)
  (let ((max-elem (general-max/min #'> list))
	(min-elem (general-max/min #'< list)))
    (if (> min-elem 0)
	(mapcar (lambda (elem)
		  (/ (- elem min-elem)
		     (abs (- max-elem min-elem))))
		list)
	(mapcar (lambda (elem)
		  (/ (+ elem min-elem)
		     (abs (- max-elem min-elem))))
		list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; histogram ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 区間(a,b)をn分割したとき,xがどの区間に入るかを返す
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
      (external-program:run *gnuplot-path* (list "-persist" *tmp-gp-file*))
      )))

(defun pdf-normal (x &key (mu 0) (sd 1))
  (/ (exp (- (/ (square (/ (- x mu) sd)) 2)))
     (* (sqrt (* 2 pi)) sd)))

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
	(external-program:run *gnuplot-path* (list "-persist" *tmp-gp-file*))
	))))

;;; multiplotのための関数群

(defun dump-gp-file-append (plot-arg-format
			    &key (x-label nil) (y-label nil)
			    (aspect-ratio 1.0)
			    (output nil) (output-format :png)
			    (x-logscale nil) (y-logscale nil)
			    (x-range nil) (y-range nil)
			    (x-range-reverse nil) (y-range-reverse nil) (key t))
  (declare (ignore output output-format))
  (with-open-file (gp-file *tmp-gp-file* :direction :output
			   :if-exists :append :if-does-not-exist :create)
    
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
				(aspect-ratio 1.0)
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
		       :output output :output-format output-format
		       :x-logscale x-logscale :y-logscale y-logscale
		       :x-range x-range :y-range y-range
		       :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
		       :key key))

(defun plot-lists-for-multiplot (plot-id y-lists
				 &key (x-lists nil) (title-list nil) (style 'lines)
				 (x-label nil) (y-label nil)
				 (aspect-ratio 1.0)
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
		       :output output :output-format output-format
		       :x-logscale x-logscale :y-logscale y-logscale
		       :x-range x-range :y-range y-range
		       :x-range-reverse x-range-reverse :y-range-reverse y-range-reverse
		       :key key))

(defmacro multiplot (&body body)
  `(progn
     (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :supersede)
       (format gp-file "set multiplot layout ~A,1~%set format y \"%.3f\"~%" ,(length body)))
     (loop for plot-id from 0 to ,(1- (length body)) do
	  (cond ((eq (car (nth plot-id ',body)) 'plot-list)
		 (apply #'plot-list-for-multiplot (cons plot-id (mapcar #'eval (cdr (nth plot-id ',body))))))
		((eq (car (nth plot-id ',body)) 'plot-lists)
		 (apply #'plot-lists-for-multiplot (cons plot-id (mapcar #'eval (cdr (nth plot-id ',body))))))))
     (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :append)
       (format gp-file "unset multiplot~%"))
     ;; gnuplotを呼び出し
     (external-program:run *gnuplot-path* (list "-persist" *tmp-gp-file*))))


;;; 二次元プロットをアニメーションで表示するときに利用する、非同期バージョン
;;; dump-gp-fileをファイルではなくストリームに出力するようにして、with-plot-streamマクロの中でそれを呼ぶ
(defmacro with-plot-stream (stream &body body)
  (let ((proc (gensym)))
    `(let* ((,proc (external-program:start *gnuplot-path* nil :input :stream :output :stream))
	    (,stream (external-program:process-input-stream ,proc)))
       (handler-case
	   (progn ,@body
		  (format ,stream "~%quit~%")
		  (finish-output ,stream))
	 (simple-error (c)
	   (declare (ignore c))
	   (external-program:signal-process ,proc :quit)))
       ,proc)))

;;; 三次元プロット
(defun dump-gp-file-3d
    (plot-arg-format
     &key
     (x-label nil) (y-label nil) (z-label nil)
     (aspect-ratio 1.0)
     (output nil) (output-format :png)
     (x-logscale nil) (y-logscale nil) (z-logscale nil)
     (x-range nil) (y-range nil) (z-range nil)
     (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
     (key t) (map nil))
  (with-open-file (gp-file *tmp-gp-file* :direction :output :if-exists :supersede)
    ;; 図の設定
    ;; 画像出力する場合の設定
    (cond (output
	   (ecase output-format
	     (:pdf (format gp-file "set term pdf~%"))
	     (:eps (format gp-file "set term postscript eps enhanced color~%"))
	     (:eps-monochrome (format gp-file "set term postscript eps enhanced monochrome~%"))
	     (:png (format gp-file "set term png~%"))
	     (:png-1280x1024 (format gp-file "set term png size 1280,1024~%"))
	     (:png-2560x1024 (format gp-file "set term png size 2560,1024~%"))
	     (:png-monochrome (format gp-file "set term png monochrome~%")))
	   (format gp-file "set output \"~A\"~%" output))
	  (t (format gp-file "set term x11~%")))
    
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

    ;; 凡例の位置、あるいは出すかどうか
    key

    (format gp-file "set palette rgbformulae 22,13,-31~%")

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
		   (aspect-ratio 1.0)
		   (output nil) (output-format :png)
		   (x-logscale nil) (y-logscale nil) (z-logscale nil)
		   (x-range nil) (y-range nil) (z-range nil)
		   (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
		   (key t)
		   (map nil))
  ;; 長さチェック
  (if (not (= (length x-list) (length y-list)))
    (error "list length mismatch detected between y-list and x-list."))
  
  ;; datファイルに出力
  (with-open-file (dat-file *tmp-dat-file* :direction :output :if-exists :supersede)
      (mapc #'(lambda (x)
		(mapc #'(lambda (y)
			  (format dat-file "~f ~f ~f~%" x y (funcall z-func x y)))
		      y-list)
		(format dat-file "~%")) ; xの値が変わるごとに改行を入れることでグリッドデータとして認識される
	    x-list))
    
  ;; gpファイルに出力
  (dump-gp-file-3d
   (if map
       (format nil "\"~A\" using 1:2:3 title \"~A\""
	       *tmp-dat-file* title)
       (format nil "\"~A\" using 1:2:3 with ~A title \"~A\""
	       *tmp-dat-file* (string-downcase (symbol-name style)) title))
   :x-label x-label :y-label y-label :z-label z-label
   :aspect-ratio aspect-ratio
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
   :key key :map map)
  ;; gnuplotを呼び出し
  (external-program:run *gnuplot-path* (list "-persist" *tmp-gp-file*)))

(defun splot-matrix (matrix &key (title " ") (style 'lines)
			      (x-label nil) (y-label nil) (z-label nil)
			      (output nil) (output-format :png)
			      (x-range-reverse nil) (y-range-reverse nil) (z-range-reverse nil)
			      (key t))
  (flet ((seq-row (start end)
	   (nlet iter ((i start) (product nil))
	     (if (> i end)
	       (reverse product)
	       (iter (1+ i) (cons (+ i 0.999) (cons (+ i 0.999) (cons i (cons i product))))))))
	 (seq-col (start end)
	   (nlet iter ((i start) (product nil))
	     (if (> i end)
	       (reverse product)
	       (iter (1+ i) (cons (+ i 0.999) (cons i (cons (+ i 0.999) (cons i product)))))))))
    (splot-list (lambda (x y)
		  (aref matrix (truncate x) (truncate y)))
		(seq-row 0 (1- (array-dimension matrix 0)))
		(seq-col 0 (1- (array-dimension matrix 0)))	      
		:title title :style style
		:x-label x-label :y-label y-label :z-label z-label
		:output output :output-format output-format
		:x-range-reverse x-range-reverse :y-range-reverse y-range-reverse :z-range-reverse z-range-reverse
		:aspect-ratio 1.0 :map t :key key)))
