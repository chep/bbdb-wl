;;; bbdb-wl-addrmgr.el --- 
;; 
;; Filename: bbdb-wl-addrmgr.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: dom feb 19 18:14:52 2012 (-0300)
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
;; This file adds more functionallity to Wandelust's address manager.
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

(defconst bbdb-wl-addrmgr-temp-buffer " *bbdb-wl-addrmgr-tmp*"
  "Name of the temporary buffer for this module.")

(defun bbdb-wl-addrmgr-open-file ()
  "Open the file in the `bbdb-wl-addrmgr-temp-buffer' temporary buffer for use with the functions of this library.
Then, insert the contents in the buffer(if the buffer doesn't exists!).

Return the temporary buffer with all the necessary contents.

Usually you don't need to call this function because is called when needed."

  (let ((buff (get-buffer bbdb-wl-addrmgr-temp-buffer)))
    (unless buff
      ;; Buffer desn't exists!
      (setq buff (get-buffer-create bbdb-wl-addrmgr-temp-buffer))
      (with-current-buffer buff
	(if (file-exists-p wl-address-file)
	    (insert-file-contents wl-address-file))
	)
      )
    buff
    )
  )

(defun bbdb-wl-insert-into-file (entries &optional save-after-insert)
  "Insert all the new entries in the `wl-address-file'. This insertion is temporary until you call `bbdb-wl-addrmgr-save' or you pass a t value into the \"save-after-insert\" parameter.

Each entry, as usual, is a list composed by three elements in this order: the email address, the petname and the realname.

Remember that the parameter \"entries\" is a list of entries.

If no-init is set to t, then don't call `wl-address-init'. This is useful when you want to save multiple entries faster: you don't initialize
the address list every time you insert each entry!

If save-after-insert is t, then save the buffer after inserting each entry. Also, you can use `bbdb-wl-addrmgr-save' to save the file."

  ;; Part of this code was extracted from wl-address.el, a file that comes with Wanderlust.
  (let ((output-coding-system
	 (mime-charset-to-coding-system wl-mime-charset)))
    (with-current-buffer (bbdb-wl-addrmgr-open-file)
      (dolist (entry entries)
	;; Avoid nils in the entry!
	(let ((address (nth 0 entry))
	      (petname (nth 1 entry))
	      (realname (nth 2 entry))
	      )
	  (unless petname
	    (setq petname "")
	    )
	  (unless realname
	    (setq realname "")
	    )      
	  
	  (save-excursion
	    (goto-char (point-max))
	    (insert (format "%s\t%s\t%s\n"
			    address ;; address
			    (prin1-to-string petname) ;; petname
			    (prin1-to-string realname) ;; realname
			    )
		    )
	    )
	  )
	)
      
      (when save-after-insert
	(bbdb-wl-addrmgr-save)
	(wl-address-init)
	)
      )
    
    )
  )

(defun bbdb-wl-addrmgr-save ()
  "Save the temporary buffer that contains the contents of the .address file.

This file is the container of all information of Wanderlust's Address Manager. 
If you inserted some entry with the function `bbdb-wl-insert-into-file' you can save what you inserted with this function.

Once saved, I delete temporary buffer! and reinitialize the `wl-address-list' by using `wl-address-init'.
"

  (let ((buff (get-buffer bbdb-wl-addrmgr-temp-buffer)))
    (when buff
      (with-current-buffer buff
  	(write-region (point-min) (point-max)
		      wl-address-file nil 'no-msg)
	)
      (kill-buffer buff)
      (wl-address-init)
      )
    )
  )


(defvar bbdb-wl-addrmgr-test-email-var nil
  "This variable is used internally. 
You need to set this variable to use `bbdb-wl-addrmgr-test-email' function.
This email will be compared with the entry parameter in that function.")

(defun bbdb-wl-addrmgr-test-email (entry)
  "This is an internal function for use with `find-if'.
This function test if the email in the entry is equal to the email stored in `bbdb-wl-addrmgr-test-email-var'.
Please, set that variable first using let to give the email to compare with."
  (string-equal (car entry) bbdb-wl-addrmgr-test-email-var)
  )

(defun bbdb-wl-addrmgr-search-email (email)
  "Return the position where is the email given in the variable `wl-address-list'.

I need the `bbdb-wl-addrmgr-test-mail' function."
  (unless wl-address-list
    (wl-address-init)
    )
  (let ((bbdb-wl-addrmgr-test-email-var email))
    (position-if 'bbdb-wl-addrmgr-test-email wl-address-list)
    )
  )

(defun bbdb-wl-addrmgr-edit-in-file (pos new-entry)
  "Change in the position designed by \"pos\" the entry for the \"new-entry\".

Use the temporary buffer `bbdb-wl-addrmgr-temp-buffer'.

After using this command you should use for saving permanently into the .address file the functino `bbdb-wl-addrmgr-save'."
  (let ((output-coding-system
	 (mime-charset-to-coding-system wl-mime-charset)))
    (with-current-buffer (bbdb-wl-addrmgr-open-file)
      (save-excursion
	(goto-char (point-min))
	(goto-line pos)
	(delete-region (point) (point-at-eol))

	(let ((address (nth 0 new-entry))
	      (petname (nth 1 new-entry))
	      (realname (nth 2 new-entry))
	      )
	  (unless petname
	    (setq petname "")
	    )
	  (unless realname
	    (setq realname "")
	    )      


	  (insert (format "%s\t%s\t%s"
			  address ;; address
			  (prin1-to-string petname) ;; petname
			  (prin1-to-string realname) ;; realname
			  )
		  )	
	  )
	)
      )
    )
  )

(defun bbdb-wl-address-list-add (entry)
  "Add entry into `wl-address-list' initializing and doing whatever is necessary."
  (if wl-address-list
      (setq wl-address-list (push entry wl-address-list)) 
    (setq wl-address-list (list entry)) ;; `wl-address-list' is nil... set it properly
    )
  )

(defun bbdb-wl-address-list-replace (entry num-pos)
  "Replace entry NUM-POS with given ENTRY. Also do necessary checks."
  (if wl-address-list
      (bbdb-wl-replace-nth wl-address-list num-pos entry)
    (setq wl-address-list (list entry))
    )
  )

(provide 'bbdb-wl-addrmgr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-wl-addrmgr.el ends here
