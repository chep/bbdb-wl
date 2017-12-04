;;; bbdb-wl-wl.el --- 
;; 
;; Filename: bbdb-wl-wl.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: dom feb 19 18:08:59 2012 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; This file add functions to work with Wanderlust. 
;; For example, getting data from wanderlust's buffers, adding keymaps or hooks, etc.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(require 'wl)
(require 'bbdb)

(require 'bbdb-wl-bbdb)
(require 'bbdb-wl-ignore)

(defcustom bbdb-wl-ignore-addr-regexp "\\(no-?reply\\|invlid\\)"
  "Content of the sender address that has to be ignored.

Any sender address that match this regexp anywhere in his mail or name string should be ignored.

Used in  function `bbdb-wl-should-ignore-addr'."
  :group 'bbdb-wl-ignore
  :type 'regexp
  )

(defcustom bbdb-wl-regexp-remove '("$\"" "\"^" "$'" "'^")
  "Characters that match these regexps will be removed from the name. 

See `bbdb-wl-get-sender-data' and `bbdb-wl-remove-characters'."
  :group 'bbdb-wl
  :type '(set regexp)
  )
  

(defun bbdb-wl-should-ignore-addr (addr)
  "ADDR should be ignored?

In other words, one of this criterias becomes t:

* matchs `bbdb-wl-ignore-addr-regexp'? 
* is in the ignore entry?

ADDR must be a string with complente name and email address. Use `bbdb-wl-should-ignore' for retrieving and checking."

  (if (string-match bbdb-wl-ignore-addr-regexp addr)
      t
    (bbdb-wl-ignored-addr (wl-address-header-extract-address addr))
    )  
  )

(defun bbdb-wl-should-ignore ()
  "Retrieve all the information from the current mail and check if this mail shouldn't be scanned for BBDB entries(not even for showing!).

This means, it matchs `bbdb-wl-ignore-addr-regexp'."

  (bbdb-wl-should-ignore-addr (bbdb-wl-get-sender-data))
  )

(defun bbdb-wl-remove-characters (string)
  "Characters unnecessary will be removed. 

These characters are selected according to `bbdb-wl-regexp-remove'. All regexps in this variable will be tested and any match will be removed."

  (dolist (re bbdb-wl-regexp-remove)
    (when (string-match re string)
      (setq string (replace-match "" nil nil string))
      )
    )
  string
  )

(defun bbdb-wl-get-sender-data ()
  "Get all the data located in the header about the sender.
Also, some characters will be removed according to `bbdb-wl-remove-characters' and `bbdb-wl-regexp-remove'."
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^From:[[:blank:]]*" nil t) 
    (search-forward-regexp "[^[:blank:]].*$" nil t)
    (bbdb-wl-remove-characters (match-string-no-properties 0))
    )
  )

(defun bbdb-wl-get-sender-name ()
  "Return the sender name(just the name)"
  ;; Search for sender mail...
  (let ((usrmail (bbdb-wl-get-sender-data)))
    ;; Extract real name
    (wl-address-header-extract-realname usrmail)
    )
  )

(defun bbdb-wl-get-sender-address ()
  "Return the sender petname(just that)"
  (let ((usrmail (bbdb-wl-get-sender-data)))
    (wl-address-header-extract-address usrmail)
    )
  )

(defun bbdb-wl-query-create-sender ()
  "Query the user if we can create the sender. If we can, create the sender with its data.
If we cannot... just ignore this mail."  
  (let ((selected ?a))
    (while (not (or
		 (= selected ?n)
		 (= selected ?y)
		 (= selected ?p)
		 (= selected ?c)
		 (= selected ?i)))
      (setq selected (read-char
		      (format "Record %s<%s> doesn't exists.  Create it?\nPress:
'y' for yes
'n' or 'p' for no
'c' for create with name
'i' for ignore and don't ask anymore." 
			      (bbdb-wl-get-sender-name) (bbdb-wl-get-sender-address))))
      )
    (cond ((= selected ?y)
	   ;; Answers "y"... create it.
	   (bbdb-wl-create-sender))
	  ((= selected ?c)
	   (bbdb-wl-create-sender (read-string "Name?" (bbdb-wl-get-sender-name))))
	  ((= selected ?i)
	   (bbdb-wl-ignore-add (bbdb-wl-get-sender-address)))
	  )
    
    )
  )

(defun bbdb-wl-create-sender (&optional name)
  "Create the sender BBDB record and add it to BBDB.
If NAME is given, use that name instead of the sender name."
  (unless name
    (setq name (bbdb-wl-get-sender-name))    
    )    
  (when (string= "" name)
      (setq name "NO NAME")
    )
  ;; Avoid duplication!
  ;;(let ((bbdb-no-duplicates t))  
    (if
	(bbdb-create-internal :name name ;; name
			              :mail (cons (bbdb-wl-get-sender-address) nil);; mail
			              )
	(message "%s" "Record Added: Done")
      (message "%s" "Record not added... Possibly it is already in the database")
      )
	
  ;;  )
  )

(defun bbdb-wl-get-update-record ()
  "Function ideally of `wl-message-redisplay-hook'.
Find all data from the sender and reciever(the 'get' part) and query to update if necessary(the 'update' part).

* If not founded then query if we should create it.
* If founded then:
  * If it is modified then try to update.
  * If it is the same: just show it.
"  
  (interactive) 
  (unless (bbdb-wl-should-ignore) 
    ;; Find names!
    (let ((record (bbdb-wl-get-record-from-name))) ;; look for the name.	
      (if record
	  (bbdb-wl-check-update record) ;; There is a name => check if we have to update.
	(progn ;; There is no name! look for the email.

	  (setq record (bbdb-wl-get-record-from-mail))
	  (if record
	      (bbdb-wl-check-update record) ;; There it is! => check if we have to update.
	    (progn ;; No record from the sender => add it
	      (bbdb-wl-query-create-sender)
	      (bbdb-wl-find-and-show (bbdb-wl-get-sender-address))
	      )
	    )
	  
	  )
	
	)
      )
    )
  )

(defun bbdb-wl-get-record-from-name ()
  "Function complement of  `bbdb-wl-get-update-record'.
This function is inteded to search for the sender data in BBDB, 

Returns:
* the BBDB record if founded 
* nil if nothing has been found."
  (let ((sender-name (bbdb-wl-get-sender-name)))
    ;; is it in BBDB?    
    (setq records (bbdb-wl-find-and-show sender-name))
    (if records
	;; yes, exists...	
	records      
      ;;Nop, doesn't exists...
      nil
      )
    )
  )


(defun bbdb-wl-get-record-from-mail ()
  "Function complement of  `bbdb-wl-get-update-record'.
This function is inteded to search for the sender data in BBDB.

Returns:
* the BBDB record if founded.
* nil if nothing has been found."
  (let ((sender-mail (bbdb-wl-get-sender-address)))
    (setq records (bbdb-wl-find-and-show-mail sender-mail))
    (if records
	;; yes, exists...	
	records
      ;;Nop, doesn't exists...
      nil
      )
    )
  )


(defun bbdb-wl-check-update (record)
  "Check if this BBDB record and the information in the mail are different.
If they are, query the user if she/he want to update the BBDB record.
If she/he wants, update it.
If not, well, do nothing!"
  ;; TODO
  )



(provide 'bbdb-wl-wl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-wl-wl.el ends here
