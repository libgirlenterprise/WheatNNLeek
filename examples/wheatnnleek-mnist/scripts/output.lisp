(ql:quickload :wheatnnleek-mnist)
(defvar test-data (wheatnnleek-mnist::get-mnist-testing-data))
(defvar label-data (wheatnnleek-mnist::get-mnist-testing-label))

(defun to-svg (array &key (w "50px") (h "50px"))
  (with-output-to-string (s)
    (format s "<svg width=~S height=~S>~%" w h)
    (loop with d0 = (array-dimension array 0)
          with d1 = (array-dimension array 1)
          with height = (/ 100d0 d0)
          with width = (/ 100d0 d1)
          for i below d0
          nconc(loop for j below d1
                     collect (format s "<rect x=\"~f%\" y=\"~f%\" width=\"~f%\" height=\"~f%\" style=\"fill:rgb(~A,~:*~A,~:*~A);\"/>~%"
                                     (* j width) (* i height)
                                     width height
                                     (aref array i j))))
    (format s "</svg>~%")))

(defun line (&rest r)
  (format nil "~{<td>~a</td>~}" r))

(defun table (&rest r)
  (format nil "<table>~%~{<tr>~%~A~%</tr>~%~}</table>" r))

(defun html (&key (body "") (head "") file)
  (flet ((f (o)
           (format o "<html>~%  <head>~A~%  </head>~%  <body>~A~%  </body>~%</html>~%"
                   head body)))
    (if file
        (with-open-file (o file :direction :output :if-exists :supersede)
          (f o))
        (with-output-to-string (o)
          (f o)))))

#+(and how to use)
(html
 :file (ensure-directories-exist
        (merge-pathnames "output/hoge.html"
                         (asdf:system-source-directory :wheatnnleek-mnist)))
 :body (table
        (line (to-svg (mnist-database:image test-data 0))
              (mnist-database:label label-data 0))))
