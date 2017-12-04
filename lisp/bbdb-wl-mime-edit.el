;;; bbdb-wl-mime-edit.el --- 
;; 
;; Filename: bbdb-wl-mime-edit.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: sáb mar  3 22:38:00 2012 (-0300)
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
;; This file is intended for storing functions related to the mime-edit 
;; mode.
;;
;; For example, here you can find the function for adding a person email
;; into a draft.
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


(defun bbdb-wl-search-To ()
  "Return the mails that are in the \"To:\" word as a string."
  (wl-addrmgr-address-entry-list "to")
  )

(defun bbdb-wl-search-CC ()
  "Return the mails that are in the \"Cc:\" word as a string."
  (wl-addrmgr-address-entry-list "cc")
  )

(defun bbdb-wl-search-BCC ()
  "Return the mails that are in the \"Bcc:\" word as a string."
  (wl-addrmgr-address-entry-list "bcc")
  )

(defun bbdb-wl-insert-person-To (name)
  "Insert into the \"To:\" field in a MIME-edit buffer a person finded by the name given.

If two records are founded then just show the two of them."
  (interactive "MName?")
  
  (let ((record (bbdb-wl-find-and-show name)))    
    ;; Check if they are various records... 
    (if (= (length record) 1)
	(let ((wl-addrmgr-draft-buffer (current-buffer)))
	  (wl-addrmgr-apply-exec (list
				  (bbdb-wl-make-email-list (bbdb-wl-search-To) (car (bbdb-record-mail (car record))))
				  (bbdb-wl-make-email-list (bbdb-wl-search-CC))
				  (bbdb-wl-make-email-list (bbdb-wl-search-BCC))
				  ))
	  )
      (message "There are various records!")
      )    
    )
  )

(defun bbdb-wl-insert-person-CC (name)
  "Insert into the \"CC:\" field in a MIME-edit buffer a person finded by the name given.

If two records are founded then just show the two of them."
  (interactive "MName?")
  
  (let ((record (bbdb-wl-find-and-show name)))    
    ;; Check if they are various records... 
    (if (= (length record) 1)
	(let ((wl-addrmgr-draft-buffer (current-buffer)))
	  (wl-addrmgr-apply-exec (list 
				  (bbdb-wl-make-email-list (bbdb-wl-search-To))
				  (bbdb-wl-make-email-list (bbdb-wl-search-CC) (bbdb-record-mail (car record)))
				  (bbdb-wl-make-email-list (bbdb-wl-search-BCC))))
	  )
      (message "There are various records!")
      )
    
    )
  )

(defun bbdb-wl-insert-person-BCC (name)
  "Insert into the \"BCC:\" field in a MIME-edit buffer a person finded by the name given.

If two records are founded then just show the two of them."
  (interactive "MName?")
  
  (let ((record (bbdb-wl-find-and-show name)))    
    ;; Check if they are various records... 
    (if (= (length record) 1)
	(let ((wl-addrmgr-draft-buffer (current-buffer)))
	  (wl-addrmgr-apply-exec (list 
				  (bbdb-wl-make-email-list (bbdb-wl-search-To))
				  (bbdb-wl-make-email-list (bbdb-wl-search-CC))
				  (bbdb-wl-make-email-list (bbdb-wl-search-BCC) (bbdb-record-mail (car record)))))
	  )
      (message "There are various records!")
      )
    
    )
  )
  

(defun bbdb-wl-send-To (name)
  "Create a new draft as wanderlust does, and add in the \"To:\" field the person founded in BBDB:"
  (interactive "MName?")
  
  (let ((record (bbdb-wl-find-and-show name)))    
    ;; Check if they are various records... 
    (if (= (length record) 1)
	(let ((wl-addrmgr-draft-buffer nil))
	  (wl-addrmgr-apply-exec (list 
				  (bbdb-record-mail (car record))
				  nil 	
				  nil))
	  (switch-to-buffer wl-addrmgr-draft-buffer)
	  )
      (message "There are various records!")
      )    
    )    
  )

(provide 'bbdb-wl-mime-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-wl-mime-edit.el ends here
