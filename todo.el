;; todo.el - handle to-do lists with GNU Emacs
;; Copyright (C) 1996 Francisco J. Ballesteros <nemo@gsyc.inf.uc3m.es>
;;
;;     with a little cut & paste from diary-lib.el :-) for `#include's 
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;;
;; -------------------------------------------------------------- 
;; DISCLAIMER:
;;
;;   Beware that this is rather a kludge I've arranged to organize my
;;   to-do list. I've not extensively tested it nor cleaned it up.
;;   It works for me.
;;   If it does not work for you email me and I may fix it up.
;; 
;;   In case you fix it yourself, please post your fixed version to 
;;   gnu.emacs.sources or email me.
;;
;; -------------------------------------------------------------- 

;; Put this into your ~/.emacs to use this utility automagically.
;;
;; (autoload 'tdl "todo" "View things to be done." t)
;; (autoload 'fancy-diary-display-tdl "todo" 
;;      "To be Hooked so that make-fancy-diary-buffer includes a to-do list" t)
;; (setq tdl-file "path-to-main-to-do-things-file")
;;
;; You may want to customize other variables below.
;;
;; -------------------------------------------------------------- 

;; A tdl (to-do-list) file is a tree of things to be done 
;; documents:
;;  usr-man:
;;   -chap 1.
;;   -spell it.
;;  usr-ref:
;;   -write it
;; code:
;;  -fix it
;;  -wipe it
;;  -smell it
;; 
;;
;; The lines `word:' start a new (sub)list of things to be done.
;; The number of blanks on the left determine the level of the list
;; (e.g. documents: is lvl 0, usr-man is lvl 1, etc). You can mix both
;; white space and tab but the number of blanks before an item must match
;; its level --I'm too lazy to write more lisp now :-)
;; 
;; Entries of the form `- such and such and such' are 
;; the things to be done. Only the first line of each entry will be included
;; in the tdl routine output. Remaining lines may detail the thing to be done.
;; 
;; You can keep multiple files with such list just by using 
;; #include "path-to-other-file"
;;
;; tdl trees found in the set of files included from the `tdl-file' 
;; will be folded. So you can keep `doc:' entries in different directories
;; for different projects. 
;;
;; Entries (those starting with `-') can be marked as done and can also
;; be given a priority. To do it just write at the end of the line
;; `[priority-number]' or `[priority-number,done]' or simply `[done]'
;; Those entries already done will not be printed.
;;
;; See customization variables below for other features
;;
;; -------------------------------------------------------------- 

(defvar tdl-file "~/private/diary/todo" 
  "* file with to-do-list entries. Variable used by function tdl")

(defvar tdl-buffer "*tdl*" 
  "* Buffer to mess up with tdl")

(defvar tdl-header "\nTO DO list\n==========\n" 
  "* String used as tdl heading when printing things to be done")

(defvar tdl-include-string "#include" 
  "* String used for tdl file inclusion")

(defvar tdl-base-priority 5 
  "* Basic priority assigned by default to entries. 
The lower the number the higher the priority. 
Use values between 0 and tdl-pri-floor")

(defvar tdl-busy-count nil "* If non nil (N>0) set max # of items to be done.
tdl output will stop when this many items have been already printed")

(defvar tdl-pri-floor 10 
 "* Do not output tdl items with a lower (higher number) priority")

;; -------------------------------------------------------------- 
;; No more usr variables nor options below.
;; -------------------------------------------------------------- 

(require 'cl)

(defun tdl ( &optional buffer )
  "View things to be done. With a valid BUFFER  insert the to-do list on it
(rather than in the tdl-buffer)."
  (interactive)
  (let ((file (substitute-in-file-name tdl-file)))
    (if (and file (file-exists-p file))
	(if (file-readable-p file)
	       (tdl-list-entries file buffer)
	  (error "tdl file not readable!"))
      (error "tdl file unspecified or does not exist!"))))



(defun tdl-list-entries (file &optional buffer)
  "List todo list entries from FILE at end of BUFFER or in tdl-buffer"
  (let* ((tdl-file-buffer (find-file-noselect file))
	 (view-buffer (if (buffer-live-p buffer)
			  buffer
			(get-buffer tdl-buffer))))
    (if (null view-buffer)
	(progn
	  (set-buffer tdl-file-buffer)
	  (set-buffer (get-buffer-create tdl-buffer))
	  (toggle-read-only 1))
      (set-buffer view-buffer))
    (let ((ro-flag buffer-read-only))
      (toggle-read-only -1)
      (if (null buffer)			;we are (re)using our own *tdl* buf.
	  (progn (erase-buffer) 
		 (switch-to-buffer-other-window (current-buffer))) 
	(goto-char (point-max)))
      (save-excursion
	(let ((first (point)))
	  (insert-buffer tdl-file-buffer)
	  (include-other-tdl-files)
	  (delete-hash-comments)
	  (let ((last (point-max))
		(ents (scan-tdl-buffer)))
	    (delete-region first last)
	    (insert-string tdl-header)
	    (print-tdl ents 0)
	    (set-buffer-modified-p nil)))
	(if ro-flag (toggle-read-only 1))))))

(defun fancy-diary-display-tdl ()
  "To be Hooked so that make-fancy-diary-buffer includes a to-do list"
  (tdl (get-buffer fancy-diary-buffer)))

(defun delete-hash-comments ()
  "Delete commented lines"
  (while (re-search-forward  "^#[^i].*$" nil t)
    (replace-match "" nil nil)))

(defun include-other-tdl-files ()
  "Include the tdl entries from other tdl files with those of tdl-file.
(adapted from fancy diary inclusion utility)
The files included are specified in the diaryfile by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `tdl-include-string'."
    (goto-char (point-min))
    (while (re-search-forward
	    (concat
	     "\\(\\`\\|\^M\\|\n\\)"
	     (regexp-quote tdl-include-string)
	     " \"\\([^\"]*\\)\"")
	    nil t)
      (let ((tdl-file (expand-file-name (substitute-in-file-name
					 (buffer-substring-no-properties
					  (match-beginning 2) (match-end 2))))))
	(replace-match "" nil nil)
	(if (file-exists-p tdl-file)
	    (if (file-readable-p tdl-file)
		(insert-file-contents tdl-file)
	      (message "Can't read included diary file %s" tdl-file))
	  (message "Can't find included diary file %s" tdl-file))
	(goto-char (point-min)))))


(defun scan-tdl-buffer ()
  "Return the list with tdl entries for the current buffer"
  (save-excursion
    (goto-char (point-min))
    (build-tdl-list (scan-tdl-buffer-entries 
		(scan-tdl-buffer-lines (point-max))))))



(defun print-tdl (tdl i)
  "Pretty print a tdl list starting with i indentation"
  (if (null tdl) t
    (if (stringp (car tdl)) 
	  (print-tdl-leave-items 
	  (setq xxx (sort-item-list (preprocess-item-list tdl)))
	  (make-string (* 3 i) ?  ))
      (insert-string (concat (make-string (* 3 i) ?  ) "*"))
      (let ((now (point)))
	(insert-string (symbol-name (car tdl)))
	(add-text-properties now (point) '(face  bold)))
      (insert-string  (concat ":\n"))
      (print-tdl (cadr tdl) (1+ i))
      (print-tdl (cddr tdl) i))))
	   

(defun sort-item-list (list)
  "Sort items according with priorities. Omit entries will priority less than
tdl-pri-floor. Leave only at most tdl-busy-count entries if not nil"
  (let* ((sorted (sort list (lambda (e1 e2) (< (nth 1 e1) (nth 1 e2)))))
	 (nents 0)
	 (chopped (if tdl-busy-count 
		      (append (list-prefix sorted tdl-busy-count) 
			      (list (list "..." tdl-pri-floor)))
			      sorted)))
    chopped))

(defun preprocess-item-list (list &optional more)
  "Explode each item to a list with explicit options."
  (if (null list) 
      (if more
	  (list (list "..." tdl-pri-floor))
	nil)
    (let* ((ent (car list))
	   (ent-opts (tdl-get-item-opts ent))
	   (pri-enough (<= (car ent-opts) tdl-pri-floor))
	   (ent-text (tdl-get-item-text ent)))
      (if (and pri-enough (not (member 'done ent-opts)))
	  (cons (cons ent-text ent-opts)
		(preprocess-item-list (cdr list) (or more (not pri-enough))))
	(preprocess-item-list (cdr list) (or more (not pri-enough)) )))))


(defun print-tdl-leave-items (list  tabbing)
  "Pretty print tdl leave items using tabbing
Will ignore done entries (those marked with a final [done] text)."
  (if (null list) t
    (insert-string (concat tabbing 
			   (if (string= (caar list) "...") "  " "- ")
			   (caar list) "\n"))
    (print-tdl-leave-items (cdr list) tabbing)))

(defun tdl-get-item-opts (item)
  (if (not (string-match "\\[\\(-?[0-9]+\\)?,?\\(done\\)?\\][ \t]*$" item))
      (list tdl-base-priority)
    (append
     (if (match-beginning 1) 
	 (list 
	  (string-to-int (substring item (match-beginning 1) (match-end 1))))
       (list tdl-base-priority))
     (if (match-beginning 2)
	 '(done)
       nil))))
	 

(defun tdl-get-item-text (item)
  (if (not (string-match "\\[\\([0-9]*\\),?\\(done\\)?\\][ \t]*$" item))
      item
    (substring item 0 (1- (match-beginning 0)))))


(defun tdl-string-to-entry (str)
  "Convert a tdl string to a single entry structure"
  (let* ((colon (string-match ":$" str))
	 (slash (string-match "/"  str))
	 (dash  (string-match "^-" str))
	 (single-ent (and colon (not dash) (not slash)))
	 (multi-ent  (and colon  slash (not dash)))
	 (leave      dash))
    (cond (single-ent (list (read (substring str 0  (1- (length str)) ))))
	  (leave      (substring str 1))
	  (multi-ent  (append (list (read 
				     (substring str 0 slash)))
			      (tdl-string-to-entry 
			       (substring str (1+ slash))))))))

(defun build-tdl-list (list)
  "Build a tdl list from a list of tdl file entries" 
  (let ((llist list)
	(tdllist nil)
	(prefix nil))
    (while llist
      (let ((lvl  (caar llist))
	    (text (cadar llist)))
	(cond ((tdl-leave-ent-p text)
	       (setq tdllist (insert-tdl-entry tdllist prefix text)))
	      (t (setq prefix (new-tdl-level lvl text prefix)))))
      (setq llist (cdr llist)))
    tdllist))
	       

(defun new-tdl-level (nlvl nprefix oprefix)
  "Set the new prefix according to a new prefix entry"
  (append (list-prefix oprefix nlvl) nprefix))

(defun list-prefix (list n)
  "Return the prefix of a list"
  (if (or (<= n 0) (null list))  nil
    (cons (car list) (list-prefix (cdr list) (1- n)))))


(defun scan-tdl-buffer-entries (ents)
  "Return a tdl file entries list for the current buffer"
  (if (null ents) 
      nil
    (cons (list (caar ents) (tdl-string-to-entry (cadar ents)))
	  (scan-tdl-buffer-entries (cdr ents)))))

	 
(defun build-tdl-regexp ()
  "Build a regexp for a single tdl entry"
  (let* ((sublevel-re   "\\([^-\n\t ]+[^-\n\t ]*\\):[ \t]*")
	(multilevel-re "\\([^-\n\t ]+[^-\n\t ]*/[^-\n\t ]+\\):[ \t]*")
	(leave-re      "-\\([^\n]*\\)+\\(\\[[^\n]+\\]\\)?"))
    (concat "^\\([\t ]*\\)" "\\(" sublevel-re "$\\|" multilevel-re "$\\|"
	    leave-re "$\\)" )))

(defun scan-tdl-buffer-lines (bound)
  "Get a list of strings with tdl entries"
  (let* ((found (re-search-forward (build-tdl-regexp) bound t ))
	(indent (length (buffer-substring (match-beginning 1) (match-end 1))))
	(item (buffer-substring (match-beginning 2) (match-end 2))))
    (if found 
	(cons (list indent item)
	      (scan-tdl-buffer-lines bound ))
      nil)))

(defun tdl-leave-ent-p (entry)
  "Is this tdl entry a leave one?"
  (stringp entry))

(defun tdl-multilevel-ent-p (entry)
  "Is this tdl entry a new level entry?"
  (listp entry))


(defun tdl-ent-level-name (entry)
  "Is this a multilevel entry with a single level?"
  (and (listp  entry) (= (length entry) 1)))

(defun insert-tdl-entry (entries prefix entry)
  "Insert a new ENTRY rooted at PREFIX in ENTRIES
This is used to build a tdl list from a tdl file entries list"
  (if (null prefix) entries
    (if (null (cdr prefix))
	(plist-put entries (car prefix) 
		   (let ((old (plist-get entries (car prefix))))
		     (if (or (null old) (stringp (car old)))
			 (append old (list entry))
		       (plist-put old 'misc: (list entry)))))
      (plist-put entries (car prefix) 
		 (let ((old (plist-get entries (car prefix))))
		   (if (stringp (car old))
		       (insert-tdl-entry (list 'misc: old) (cdr prefix) entry)
		     (insert-tdl-entry old (cdr prefix) entry)))))))




