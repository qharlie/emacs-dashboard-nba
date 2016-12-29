(require 'json)
(require 'request)

(add-to-list 'dashboard-item-generators  '(nbas . dashboard-insert-nba))
(add-to-list 'dashboard-items '(nbas) t)

(defun dashboard-insert-nba-list (title list)
  "Render NBA-LIST title and items of LIST."
  (when (car list)

    (insert title )
    (mapc (lambda (el)
	    (setq url (nth 1 (split-string el "__")) )
	    (setq title (nth 0 (split-string el "__")) )
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
				      (browse-url ,url))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"	    
			   title
			   ))
          list)))

(defun dashboard-insert-nba (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (if (> list-size 0 )
      (progn
	
	(setq file-path "/tmp/dashboard_nba.json")
	(condition-case nil
	    (delete-file file-path)
	  (error nil))
	

	(url-copy-file (format "http://data.nba.net/data/10s/prod/v1/%s/scoreboard.json" (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1))))  file-path)
	
	(setq nba-list (mapcar (lambda (entry)
				 (format "\n\t%s\n\t\t%s (%s/%s) : %s\n\t\t%s (%s/%s) : %s__https://duckduckgo.com/?q=NBA+%s+vs+%s+highlights+%s&ia=videos"
					 (let-alist entry .nugget.text)
					 
					 (let-alist entry .vTeam.triCode)
					 (let-alist entry .vTeam.win)
					 (let-alist entry .vTeam.loss)
					 (let-alist entry .vTeam.score)

					 (let-alist entry .hTeam.triCode)
					 (let-alist entry .hTeam.win)
					 (let-alist entry .hTeam.loss)
					 (let-alist entry .hTeam.score)

					 (let-alist entry .vTeam.triCode)
					 (let-alist entry .hTeam.triCode)
					 (format-time-string "%Y-%m-%d")
					 ))
        				;(concat (let-alist entry .data.title ) (concat " - " (let-alist entry .data.url ))))
			       (let-alist (json-read-file  file-path) .games)))


	
	(when (dashboard-insert-nba-list
	       "Last nights NBA scores:"
	       (dashboard-subseq nba-list 0 list-size)))	 
	(dashboard-insert--shortcut "q" "NBA Scores:")
	)
    
    ))

					;(time-subtract

