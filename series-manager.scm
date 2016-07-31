#!/usr/bin/guile -s
-*- coding: utf-8 -*-
!#
;;;; Copyright (C) 2016 Nikolay Merinov <nikolay.merinov@member.fsf.org>
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-9)     ; define-record-type
	     (srfi srfi-9 gnu) ; set-record-type-printer!
	     (ice-9 format)    ; format
	     (srfi srfi-10)    ; define-reader-ctor
	     (srfi srfi-26)    ; cut
	     (srfi srfi-1)     ; map, fold, etc. list library
	     (srfi srfi-2)     ; and-let*
	     (ice-9 regex)     ; make-regex
	     (ice-9 pretty-print)
	     (ice-9 ftw)       ; ftw
	     (ice-9 match)     ; match
	     (ice-9 readline))

;;; Define series specific data and functions.
;;; This data will be stored between runs

;; Series defined with `name',
;; `regex' match name of episode. It used to obtain all files of every episode.
;; Substring matched by `regex' will be used as episode name.
;; and list of viewed videos
(define-record-type <series>
  (make-series name regex viewed)
  series?
  (name series-name set-series-name!)
  (regex series-regex set-series-regex!)
  (viewed series-viewed set-series-viewed!))

;; This allow to serialize <series> record with (write series)
(set-record-type-printer!
 <series>
 (lambda (series port)
   (display "#,(series " port)
   (pretty-print (series-name series) port)
   (pretty-print (series-regex series) port)
   (pretty-print (series-viewed series) port)
   (display ")" port)
   #;(format port "#,(series ~s ~s ~s)"
	   (series-name series) (series-regex series) (series-viewed series))))

;; This allow to deserialize <series> record with (read series-file-port)
(define-reader-ctor 'series
  (lambda (name regex viewed)
    (make-series name regex viewed)))


;; Functions to write and read series list to database
(define %series-filename (string-append (getenv "HOME") "/.series.scm"))

(define* (write-series-list series-list #:optional (filename %series-filename))
  (call-with-output-file filename (cute write series-list <>)))

(define* (read-series-list #:optional (filename %series-filename))
  (if (access? filename R_OK)
      (call-with-input-file filename (cute read <>))
      '()))			       ; return empty list if filename not exist


;;; define episode-specific data

;; Each episode has `name', list of `videos', list of `subtitles' and
;; flag to understand we viewed it before of not.
(define-record-type <episode>
  (make-episode name videos subtitles audios viewed)
  episode?
  (name episode-name)
  (videos episode-videos set-episode-videos!)
  (subtitles episode-subtitles set-episode-subtitles!)
  (audios episode-audios set-episode-audios!)
  (viewed episode-viewed? set-episode-viewed!))

(define video-regex (make-regexp "(\\.mkv|\\.avi|\\.mp4|\\.webm)$"))
(define audio-regex (make-regexp "(\\.mp3|\\.aac|\\.flac)$"))
(define subtitle-regex (make-regexp "(\\.ass|\\.ssa|\\.srt)$"))
(define (add-file-to-episode! episode filename)
  (cond
   ((regexp-exec video-regex filename)
    (set-episode-videos! episode (cons filename (episode-videos episode))))
   ((regexp-exec audio-regex filename)
    (set-episode-audios! episode (cons filename (episode-audios episode))))
   ((regexp-exec subtitle-regex filename)
    (set-episode-subtitles! episode (cons filename (episode-subtitles episode))))))


;;; Base datatypes and functions used at runtime

;; <series-status> contain storable information about <series> and runtime data
;; about found <episodes>
(define-record-type <series-status>
  (make-series-status series compiled-regex episodes)
  series-status?
  (series series-status-series)
  (compiled-regex series-status-regex)
  (episodes series-status-episodes set-series-status-episodes!))

(define (series-status-series-name series-status)
  (series-name (series-status-series series-status)))
(define (series-status-series-viewed series-status)
  (series-viewed (series-status-series series-status)))

;;; Add series basepath (where we search with regex)
(define %series-base-path (getenv "HOME"))

(define* (make-series-status-list series-list
				  #:optional (base-directory %series-base-path))
  "Load `stored-list' from config file,
and search through `series-base-path' for existent episodes.
Return sorted list of <series-status> structures"
  ;; Make series list
  (define series-status-list
    (map (lambda (s) (make-series-status s (make-regexp (series-regex s)) '()))
	 series-list))

  (define episodes-hash (make-hash-table))
  (define (add-file-to-series series filename)
    (and-let* ((episode-match (regexp-exec (series-status-regex series)
					   (basename filename)))
	       (episode-name (match:substring episode-match)))
      (or (and-let* ((episode (hash-ref episodes-hash episode-name)))
	    (add-file-to-episode! episode filename))
	  (let* ((viewed
		  (and (member episode-name (series-status-series-viewed series))
		       #t))
		 (episode (make-episode episode-name '() '() '() viewed)))
	    (add-file-to-episode! episode filename)
	    (hash-set! episodes-hash episode-name episode)
	    (set-series-status-episodes!
	     series (cons episode (series-status-episodes series)))))))

  (define (process-series-path filename statinfo flag)
    (case flag
      ('regular (for-each (cut add-file-to-series <> filename) series-status-list)
		#t)			;continue processing
      (else #t)))			;skip other filetypes

  (define (episodes-less e1 e2)
    (string<? (episode-name e1) (episode-name e2)))
  (define (sort-episodes-in-series! series-status)
    (set-series-status-episodes! series-status
       (sort-list! (series-status-episodes series-status) episodes-less)))
  (define (series-status-less ss1 ss2)
	  (string<? (series-status-series-name ss1)
		    (series-status-series-name ss2)))


  ;; Fill `series-status-list' based on `base-directory'
  (ftw base-directory process-series-path)
  ;; Sort episodes insode `series-status-list'
  (for-each sort-episodes-in-series! series-status-list)
  ;; Return sorted `series-status-list'
  (sort-list! series-status-list series-status-less))

(define (series-status-list-series series-status-list)
  (map series-status-series series-status-list))

(define (series-status-list-series-by-name series-status-list name-of-series)
  (find (compose (cut string=? name-of-series <>) series-status-series-name)
	series-status-list))


;; Load and store data
(define* (save-series-status-list series-status-list
				  #:optional (filename %series-filename))
  "Write to file `filename' list of series stored in `series-status-list'"
  (define series-list (map series-status-series series-status-list))
  (with-output-to-file filename (cut begin (pretty-print %series-base-path)
				     (pretty-print series-list))))

(define* (restore-series-status-list #:optional (filename %series-filename))
  "Read %series-base-path and series-list from file,
then generate new series-status-list"
  (make-series-status-list
   (if (access? filename R_OK)
       (with-input-from-file filename (cut begin (set! %series-base-path (read))
					   (read)))
       '())			    ;use empty series-list if file can't be read
   %series-base-path))


;;; Functions to manipulate runtime data in sane way

;; This basic structure that was used for all this functions
(define %series-status-list (restore-series-status-list))
;; save/restore stubs
(define (save-status) (save-series-status-list %series-status-list))
(define (restore-status) (set! %series-status-list (restore-series-status-list)))

(define* (add-series name regex
		     #:optional (series-status-list %series-status-list))
  (let* ((series-list (series-status-list-series series-status-list))
	 (new-series (make-series name regex '()))
	 (new-series-status-list
	  (make-series-status-list (cons new-series series-list))))
    (if (eq? series-status-list %series-status-list)
	(set! %series-status-list new-series-status-list)
	new-series-status-list)))

(define* (remove-series name-of-series
			#:optional (series-status-list %series-status-list))
  (let* ((series-list (series-status-list-series series-status-list))
	 (new-series-status-list
	  (make-series-status-list
	   (remove (compose (cut equal? name-of-series <>) series-name)
		   series-list))))
    (if (eq? series-status-list %series-status-list)
	(set! %series-status-list new-series-status-list)
	new-series-status-list)))

(define* (update-series-status #:optional (series-status-list %series-status-list))
  (let* ((series-list (series-status-list-series series-status-list))
	 (new-series-status-list (make-series-status-list series-list)))
    (if (eq? series-status-list %series-status-list)
	(set! %series-status-list new-series-status-list)
	new-series-status-list)))

(define* (remove-series-by-pred
	  predicate #:optional (series-status-list %series-status-list))
  (let* ((series-list (series-status-list-series series-status-list))
	 (new-series-status-list
	  (make-series-status-list (remove predicate series-list))))
    (if (eq? series-status-list %series-status-list)
	(set! %series-status-list new-series-status-list)
	new-series-status-list)))

(define* (remove-series-by-name
	  name #:optional (series-status-list %series-status-list))
  (define (series-status-with-name series-status)
    (string=? name (series-status-series-name series-status)))
  (remove-series-by-pred series-status-with-name series-status-list))

(define* (mark-viewed! name-of-series name-of-episode
		       #:optional (series-status-list %series-status-list)
		       #:key (viewed #t))
  (and-let*
      ((series-status
	(series-status-list-series-by-name series-status-list name-of-series))
       (series (series-status-series series-status))
       (episode
	(find (compose (cut string=? name-of-episode <>) episode-name)
	      (series-status-episodes series-status))))
    (set-episode-viewed! episode viewed)
    (let ((ep-without-proceed (remove! (cut equal? name-of-episode <>)
				       (series-viewed series))))
      (if viewed
	  (set-series-viewed! series (cons name-of-episode ep-without-proceed))
	  (set-series-viewed! series ep-without-proceed))))
  ;; return new series-status-list
  (unless (eq? series-status-list %series-status-list) series-status-list))

(define* (mark-all-viewed! name-of-series
			   #:optional (series-status-list %series-status-list)
			   #:key (viewed #t))
  "Mark all episodes of series viewed.
If `name-of-series' is #f mark all episodes viewed."
  (define (series-mark-all-viewed! series-status)
    (for-each (cut mark-viewed! (series-status-series-name series-status)
		                <> ; name-of-episode
				series-status-list
				#:viewed viewed)
	      ;; mark only unviewed episodes
	      (map episode-name (partition (negate episode-viewed?)
					   (series-status-episodes series-status)))))
  (if name-of-series
      (series-mark-all-viewed!
       (series-status-list-series-by-name series-status-list name-of-series))
      (for-each series-mark-all-viewed! series-status-list))
  ;; return new series-status-list
  (unless (eq? series-status-list %series-status-list) series-status-list))

(define* (list-new-series #:optional (series-status-list %series-status-list))
  (define (has-new? series-status)
    (find (negate episode-viewed?) (series-status-episodes series-status)))
  (define (add-if-new series-status list)
    (if (has-new? series-status)
	(cons (series-status-series-name series-status) list)
	list))
  (fold add-if-new '() series-status-list))

(define* (list-new-episodes name-of-series
			   #:optional (series-status-list %series-status-list))
  (define (add-if-new episode list)
    (if (episode-viewed? episode)
	list
	(cons (episode-name episode) list)))
  (and-let* ((series-status
	      (series-status-list-series-by-name series-status-list
						 name-of-series)))
    (fold add-if-new '() (series-status-episodes series-status))))

(define* (list-new-series-episodes #:optional
				   (series-status-list %series-status-list))
  (let ((new-series (list-new-series)))
    (zip new-series (map list-new-episodes new-series))))

(define* (episode-files name-of-series name-of-episode
			#:optional (series-status-list %series-status-list))
  (and-let* ((series-status (series-status-list-series-by-name series-status-list
							       name-of-series))
	     (episodes (series-status-episodes series-status))
	     (episode (find (compose (cut equal? name-of-episode <>) episode-name)
			    episodes)))
    episode))

(define* (name-of-series-by-episode name-of-episode #:optional
				   (series-status-list %series-status-list))
  (and-let* ((series-status (find (compose (cut regexp-exec <> name-of-episode)
					   series-status-regex)
				  series-status-list))
	     (name-of-series (series-status-series-name series-status)))
    name-of-series))

(define* (last-episodes #:optional (series-status-list %series-status-list))
  (define (get-last-episode series-status)
    (episode-name (last (series-status-episodes series-status))))
  (zip (map series-status-series-name series-status-list)
       (map get-last-episode series-status-list)))

;;; Code to call player for series

(define %player "mpv --sub-codepage=enca:ru")
(define %subtitle " --sub-file ")
(define %audio " --audio-file ")
(define %after-command " 2>/dev/null >/dev/null ")

(define (call-player episode)
  (let* ((videos-list (episode-videos episode))
	 (video-file (if (null-list? videos-list)
			 ""
			 (string-append "'" (first videos-list) "'")))
	 (audios-list (map (cut string-append "'" <> "'")
			   (episode-audios episode)))
	 (audio-files (string-join audios-list %audio 'prefix))
	 (subtitles-list (map (cut string-append "'" <> "'")
			      (episode-subtitles episode)))
	 (subtitle-files (string-join subtitles-list %subtitle 'prefix))

	 (player-command
	  (string-join (list %player video-file audio-files subtitle-files
			     %after-command))))
    (= 0 (system player-command))))

(define* (play name-of-series name-of-episode
	       #:optional (series-status-list %series-status-list))
  (when (call-player (episode-files name-of-series name-of-episode
				    series-status-list))
    (mark-viewed! name-of-series name-of-episode series-status-list)))

(define* (play* name-of-episode
		#:optional (series-status-list %series-status-list))
  (and-let* ((name-of-series (name-of-series-by-episode name-of-episode
							series-status-list)))
    (play name-of-series name-of-episode series-status-list)))

(define* (play-next name-of-series
		   #:optional (series-status-list %series-status-list))
  (and-let* ((episodes (list-new-episodes name-of-series series-status-list))
	     (next-episode (last episodes)))
    (play name-of-series next-episode)))

;; Save status before exit
(add-hook! exit-hook save-status)

;; Readline based interface

(define (main args)
  (define (read-eval-command-loop)
    (call/cc
     (lambda (return)
       (and-let* ((command-object (readline))
		  (command (cond
			    ((eof-object? command-object) "exit")
			    ((string? command-object) command-object)
			    (#t #f)))  ;return #f if it's neither string nor eof
		  (command-word (first (string-split command #\ )))
		  (command-args
		   (if (= (string-length command-word)
			  (string-length command))
		       "" ; if there is no text after command return empty string
		       (substring command (1+ (string-length command-word))))))
	 (match command-word
	   ("exit" (return))
	   ("list" (pretty-print (list-new-series-episodes)))
	   ("play*" (play* command-args))
	   ("play-next" (play-next command-args))
	   (_ (display "Incorrent command!") (newline))))
       (read-eval-command-loop))))

  (activate-readline)
  (set-readline-prompt! "series-manager> ")
  (and-let* ((new-series (list-new-series %series-status-list))
	     (new-episodes
	      (append-map (cut list-new-episodes <> %series-status-list)
			  new-series))
	     (complete-command
	      (make-completion-function
	       (append '("list" "play*" "play-next" "exit")
		       new-episodes new-series))))

    (with-readline-completion-function complete-command read-eval-command-loop)))
