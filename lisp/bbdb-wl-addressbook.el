;;; bbdb-addressbook.el --- 
;; 
;; Filename: bbdb-addressbook.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: sáb feb 18 02:20:59 2012 (-0300)
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
;; 
;; Functions for work with the integration of the wanderlast's addressbook 
;; and BBDB Version 3.
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

(require 'wl-addrmgr)

(require 'bbdb-wl-addrmgr)
(require 'bbdb-wl-syncbuffer)

(defvar bbdb-wl-compare-string-var ""
  "This variable has to be used with let as a temporary variable.
See `bbdb-wl-compare-strings-with-tmp-var' for more information.")

(defun bbdb-wl-compare-strings-with-tmp-var (str)
  "Compare the string \"str\" with the string in the temporary variable `bbdb-wl-compare-string-var'.

Use like this:
 (let ((bbdb-wl-compare-string-var \"my string\"))
     (bbdb-wl-compare-strings-with-tmp-var variabl))

Or you can use with `position-if' or `find-if' like this:
 (let ((bbdb-wl-compare-string-var \"my string\"))
     (find-if bbdb-wl-compare-strings-with-tmp-var a-list-of-strings))
"
  (string= str bbdb-wl-compare-string-var)
  )



(defun bbdb-wl-compare-petnames (lst-petnames petname)
  "Compare with all the petnames with the petname given.

If lst-petname is a string the compare both strings, if it is a list, compares all elements in list with the string."
  (if (listp lst-petnames)
      (let ((bbdb-wl-compare-string-var petname)) ;; lst-petnames is a list... look for "petname".
	(if (position-if 'bbdb-wl-compare-strings-with-tmp-var lst-petnames)
	    t ;; founded!
	  nil ;; not founded!
	  ))
    (string= lst-petnames petname) ;; lst-petnames is a string... compare both parameters
    )
  )

;; `bbdb-wl-update-if-necessary' cannot be used here: it goes from Addrmgr to BBDB!
(defun bbdb-wl-update-addrmgr-if-necessary (new-addrmgr-entry)
  "Try to look into the address manager if the email entry exists:
* If it doesn't => add it!
* If it exists and it differs => Ask to update!
* If it exists and doesn't differs => do nothing!

new-addrmgr-entry is a typical address manager entry(is a list with three elements: *email*, *petname*, *realname*)."

  ;; Chequear la entrada
  
  (let ((entry-pos (bbdb-wl-addrmgr-search-email (car new-addrmgr-entry)))
	(new-petname (nth 1 new-addrmgr-entry))
	(new-name (nth 2 new-addrmgr-entry))
	)
    
    ;; if petname is nil for some reason, petname must be "".
    (unless new-petname     
      (setq new-petname ""))
    
    ;; if realname is nil for some reason, petname must be "".
    (unless new-name     
      (setq new-name ""))
      


    (let* ((actual-addrmgr-entry (and entry-pos
		      (nth entry-pos wl-address-list)))
	  (actual-petname (nth 1 actual-addrmgr-entry))
	  (actual-name (nth 2 actual-addrmgr-entry))
	  )

      (unless actual-petname 
	(setq actual-petname ""))
      (unless actual-name
	(setq actual-name ""))

      ;; If exists then compare if differs; else add it!
      (if actual-addrmgr-entry
	  (unless (and (string-equal actual-petname new-petname)
		       (string-equal actual-name new-name))
	    ;; Exists but is different! Ask if I have to update!
	    (let ((update-entry (bbdb-wl-ask-to-update-addrmgr-entry new-addrmgr-entry actual-addrmgr-entry))
		  )
	      (cond ((and update-entry (listp update-entry))
		     (bbdb-wl-addrmgr-edit-in-file entry-pos update-entry)	;; User request to update something
		     (bbdb-wl-address-list-replace update-entry entry-pos)		  
		     )		     
		    (update-entry ;; Anything else is t!...
		     (bbdb-wl-insert-into-file (list new-addrmgr-entry)) ;; User requested to insert new-entry as new
		     (bbdb-wl-address-list-add new-addrmgr-entry)
		     )
		    )
	      )
	    )
	(progn 
	  (bbdb-wl-syncbuffer-show "Adding because it doesn't exists.")
	  (bbdb-wl-insert-into-file (list new-addrmgr-entry)) ;; it doesn't exists: add it!
	  (bbdb-wl-address-list-add new-addrmgr-entry)
	  )
	)
      )
    )
  )

(defun bbdb-wl-ask-to-update-addrmgr-entry (new-entry old-entry)
  "Ask the user if the old-entry must be updated with the new entry or just add a new one.

This return the following depending on the user's choice:

* nil if the user has pressed any other key(user want to skip).
* t if it has to be added as a new one.
* An entry if it has to be updated. This entry is how the user want's to update the old one."
  
  ;; Ask if he wants to add it as new or update.  
  (let ((msg (concat 
	      "I found a contact with the same email.\n
Field - New Entry - Old Entry"
	      "\nEmail: " (car new-entry) " - " (car old-entry)
	      "\nPetname: " (nth 1 new-entry) " - " (nth 1 old-entry)
	      "\nName: " (nth 2 new-entry) " - " (nth 2 old-entry)
"\n\nWhat should I do? 
SPACE: add it as new.
p: update the old petname only.
n: update the old name only.
a: update the old petname and name.
other key: skip."))
	)
    (let ((choice (read-key msg)))
      (cond ((= choice 32)
	     (bbdb-wl-syncbuffer-show "User wants to add it as new!")
	     t)
	    ((= choice ?a)
	     (bbdb-wl-syncbuffer-show "User wants to update name and petname.")
	     new-entry)
	    ((= choice ?p)
	     (bbdb-wl-syncbuffer-show "User wants to update *petname* only.")
	     (list (car old-entry) (nth 1 new-entry) (nth 2 old-entry)))
	    ((= choice ?n)	     
	     (bbdb-wl-syncbuffer-show "User wants to update *real-name* only.")
	     (list (car old-entry) (nth 1 old-entry) (nth 2 new-entry)))     
	    (t
	     nil)
	    )      
      )
    )
  )

(provide 'bbdb-wl-addressbook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-addressbook.el ends here
