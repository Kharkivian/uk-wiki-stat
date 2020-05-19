;; нам цікаво поглянути на статистику за весь 2019, за 1 кв. 2020, і за квітень.
;; можна поступово, не все зразу.

;; я створю підсторінку куди складати результати.

;; по всім показникам - було би бажано топ-200. щоби мати релевантні
;; дані. і розширений список.

(ql:quickload '(:fare-csv
		:iterate
		:split-sequence
		:parse-number
		:cl-mediawiki-util
		:cl-json
		:cl-fad))

(defpackage :kharkiv
  (:use :cl :iterate :split-sequence :parse-number)
  (:export :run))

(in-package :kharkiv)

(defparameter *pages-csv* "Вікіпроєкт-Харків. 14.05.2020.csv")

(defun get-page-names ()
  (with-open-file (in *pages-csv*
		      :direction :input)
    (fare-csv:READ-CSV-LINE in)
    (iter
      (for line = (fare-csv:READ-CSV-LINE in))
      (while line)
      (format t "~a~%" (first line))
      (collect (second line)))))
;; -

(defun generate-links (articles start end)
  (let ((links-file (format nil "links-~a_~a.txt" start end)))
    (unless (probe-file links-file)
      (with-open-file (out links-file
		       :direction :output
		       :if-exists :supersede)
	(dolist (article articles)
	  (format out "~a~%" (mw-util:get-query-url "uk.wikipedia" article start end)))))
    links-file))
;; -

(defun generate-report-using-mwutil (articles start end)
  (with-open-file (out (format nil "report-~a_~a.csv" start end)
		       :direction :output
		       :if-exists :supersede)
    (fare-csv:write-csv-line '("name" "total" "min" "max" "median") out)

    (dolist (article articles)
      (fare-csv:write-csv-line
       (concatenate 'list
		    `(,article)
		    (mw-util:get-article-page-views article :start start :end end))
       out))))
;; -

(defun parse-json-files (json-txt)
  (let ((articles))

    (cl-fad:walk-directory
     "js"
     #'(lambda (json-file)
	 (with-open-file (json-in json-file :direction :input)
	   (let ((json (cl-json:decode-json json-in)))
	     (cond
	       ((or (string= (cdr (assoc :type json))
			     "https://restbase.org/errors/query_error")
		    (string= (cdr (assoc :type json))
			     "https://mediawiki.org/wiki/HyperSwitch/errors/not_found"))
		(format t "query error or article not found ")
		nil)

	       ((string= (cdr (assoc :type json))
			 "https://restbase.org/errors/not_found")
		;; very likely this article didn't exist yet in the
		;; specified time period, just report as no views at all
		(format t "article  not found")
		nil)

	       (t
		;; article data is present, do the math
		(let* ((items (cdar json))
		       (article (map 'list
				     #'(lambda (item)
					 (cdr (find :article item :key #'car)))
				     items))
		       (daily-views (map 'list
					 #'(lambda (item)
					     (cdr (find :views item :key #'car)))
					 items)))
		  ;; collect and then calculate sum and median
		  (push (list (first article)
			      (reduce #'+ daily-views)
			      (apply #'min daily-views)
			      (apply #'max daily-views)
			      (float (alexandria:median daily-views)))
			articles))))))))
    (sort articles #'> :key #'second)))

;; -

(defun generate-report-using-wget (start end)

  (with-open-file (out (format nil "report-~a_~a.csv" start end)
		       :direction :output
		       :if-exists :supersede)
    (fare-csv:write-csv-line '("name" "total" "min" "max" "median") out)

    (dolist (article-stats (parse-json-files (format nil "json_~a.txt" end)))
      (format t "~a~%" (first article-stats))
      (fare-csv:write-csv-line article-stats out))))

;; -

(defun generate-dates (year)
  (iter
    (for month from 1 to 12)
    (collect
	(cond
	  ((= month 1) ;; january
           (format nil "~d~2,'0D0100" year month))
	  (t
	   (format nil "~d~2,'0D0100" year month))))))

;; -

(defun process-period (start end)
  (let* ((articles (get-page-names))
	 (links-file (generate-links articles start end)))

    (generate-report-using-wget start end)))

;; -

(defun run ()
  (let ((periods
	  (iter
	    (for end in (generate-dates 2019))
	    (for start previous end)
	    (when start
	      (collect `(,start ,end))))))

    ;;(process-period start end)


    ))
;; -

;; (end "2020050100")
;; wget -i -r
;; mkdir js
;; find wikimedia.org/ -type f -exec cp --backup=t '{}' js ';'
;; (kharkiv:run)
