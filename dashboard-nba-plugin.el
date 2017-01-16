(require 'json)

(add-to-list 'dashboard-item-generators  '(nbas . dashboard-insert-nba))
(add-to-list 'dashboard-items '(nbas) t)

(defun empty-string-p (string)
  "Return true if the string is empty or nil. Expects string."
  (or (null string)
      (zerop (length string))))

(defun dashboard-insert-nba-list (title list)
  "Render NBA-LIST title and items of LIST."
  (when (car list)

    (insert title )
    (insert "\n\n\t")
    (mapc (lambda (el)
	    (setq url (nth 1 (split-string el "__")) )
	    (setq desc (nth 0 (split-string el "__")) )
	    (insert desc "\n\n\t\t")

            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
				      (browse-url ,url))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix "["
                           :button-suffix "]"
                           :format "%[%t%]"	    
			   "Watch Highlights"
			   )
	    (insert "\n")
	    )

          list)))


(defun dashboard-read-nba-file (file-path)
  (setq nba-list (mapcar (lambda (entry)
			   (format "\n\t%s\n\t\t%s (%s/%s)[%s%%] : %s\n\t\t%s (%s/%s)[%s%%] : %s__https://duckduckgo.com/?q=%s+vs+%s+highlights+%s&ia=videos"
				   (let-alist entry .nugget.text)
					 
				   (let-alist entry .vTeam.triCode)
				   (let-alist entry .vTeam.win)
				   (let-alist entry .vTeam.loss)
				   (fceiling (* 100.0 (/ (string-to-number (concat (let-alist entry .vTeam.win) ".0")) (+ (string-to-number (let-alist entry .vTeam.win)) (string-to-number (let-alist entry .vTeam.loss))))))

				   
				   (let-alist entry .vTeam.score)

				   (let-alist entry .hTeam.triCode)
				   (let-alist entry .hTeam.win)
				   (let-alist entry .hTeam.loss)
				   (fceiling (* 100.0 (/ (string-to-number (concat (let-alist entry .hTeam.win) ".0")) (+ (string-to-number (let-alist entry .hTeam.win)) (string-to-number (let-alist entry .hTeam.loss))))))				   
				   (let-alist entry .hTeam.score)
				   
				   (let-alist entry .vTeam.triCode)
				   (let-alist entry .hTeam.triCode)
				   (format-time-string "%Y-%m-%d")
				   ))
        				;(concat (let-alist entry .data.title ) (concat " - " (let-alist entry .data.url ))))
			 (let-alist (json-read-file  file-path) .games)))
  )


(defun dashboard-highlight-nba ()
  (interactive)
  (setq file-path "/tmp/dashboard_nba.json")
  (mapcar (lambda (entry)
	    (progn
	      (if (not (empty-string-p (let-alist entry .nugget.text))) (highlight-phrase (let-alist entry .nugget.text) "hi-blue-b"))
	      (setq hteam-win (> (string-to-int (let-alist entry .hTeam.score))  (string-to-int (let-alist entry .vTeam.score))))
	      (if hteam-win
		  (progn
		    (highlight-phrase (let-alist entry .hTeam.triCode) "hi-green")
		    (highlight-phrase (let-alist entry .vTeam.triCode) "hi-red-b")
		    )
		(highlight-phrase (let-alist entry .vTeam.triCode) "hi-green")
		(highlight-phrase (let-alist entry .hTeam.triCode) "hi-red-b")			       
		)	      
	      )
	    )
	  (let-alist (json-read-file  file-path) .games))  
  )


(defun dashboard-insert-nba (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (if (> list-size 0 )
      (progn
	
	(setq file-path "/tmp/dashboard_nba.json")
	(condition-case nil
	    (delete-file file-path)
	  (error nil))
	
	(setq str-date (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1))))
	(url-copy-file (format "http://data.nba.net/data/10s/prod/v1/%s/scoreboard.json" str-date)  file-path)
	
	(setq nba-list (dashboard-read-nba-file file-path))
	
	(when (dashboard-insert-nba-list
	       (format "NBA scores for %s " (format-time-string "%A , %D" (time-subtract (current-time) (days-to-time 1))))
	       (dashboard-subseq nba-list 0 list-size)))	 
	;(dashboard-insert--shortcut "q" "NBA Scores:")


	
	)
    ))

					;(time-subtract


