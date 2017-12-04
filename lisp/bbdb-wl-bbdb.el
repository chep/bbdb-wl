;;; bbdb-wl-bbdb.el --- 
;; 
;; Filename: bbdb-wl-bbdb.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: dom feb 19 18:11:41 2012 (-0300)
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
;; This file acts as interface between BBDBV3-WL and BBDB itself. 
;; So, add more functionallity to BBDB so we can work comfortable.
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

(require 'bbdb)

(defconst bbdb-wl-ignore-name "ignore"
  "This name is stored in the BBDB database with all the email address that has to be ignored.")

(defun bbdb-wl-insert-ignore (data-list)
  "Insert the data into BBDB into the ignore record. 

For inserting (and more explanation about the DATA-LIST parameter) see `bbdb-wl-insert-if-not-exists'.

This function changes the name inside DATA-LIST for the `bbdb-wl-ignore-name' constant."
  (setf (cdr (assoc 'name data-list)) bbdb-wl-ignore-name)
  (bbdb-wl-insert-if-not-exists data-list)
  )

(defun bbdb-wl-change-name-to-email (data-list)
  "Change the name of DATA-LIST into to its email.

The data is a alist of elements:
name        => A String
affix       
aka         => A list of Strings
org         => A list of Strings
mail        => A comma separated list or a list of strings
phones      => A list of vectors with the form [\"label\" areacode prefix suffix extension-or-nil]
addresses   => A list of vectors with the form [\"label\" (\"line1\" \"line2\" ...) \"City\" \"State\" \"Postcode\" \"Country\"]
notes       => A list of associating symbols with strings
"

  (let ((aux (assq-delete-all 'name data-list))
	)
    (cons (cons 'name (cdr (assoc 'mail aux))) ;; => (name . mail-string)
	  aux) ;; => ((name . mail-string) ... (mail . mail-string) ...)
    ))
  



(defun bbdb-wl-insert-if-not-exists (data-list &optional update)
  "Insert the data into BBDB. 

If no-update is t then don't update if there is another record with the same name.

The data is a alist of elements:
name        => A String
affix       
aka         => A list of Strings
org         => A list of Strings
mail        => A comma separated list or a list of strings
phones      => A list of vectors with the form [\"label\" areacode prefix suffix extension-or-nil]
addresses   => A list of vectors with the form [\"label\" (\"line1\" \"line2\" ...) \"City\" \"State\" \"Postcode\" \"Country\"]
notes       => A list of associating symbols with strings

If name already exists, it only updates the information depending if the \"update\" parameter is t.

If name is empty, it uses the email as name."

  ;; If name field in data-list is null or empty then create the entry...
  ;; If the name has something search it in the BBDB.
  (if (or (string= "" (cdr (assoc 'name data-list)))
	  (null (cdr (assoc 'name data-list))))
      (bbdb-wl-insert-if-not-exists  
       (bbdb-wl-change-name-to-email data-list)) ;; delete the name cons and add the email as a name cons
    (let ((record (bbdb-wl-find-name (cdr (assoc 'name data-list)))))
      (when (= length record 1)
	(if update ;; If there are only one record and update is t then update...
	    (bbdbl-wl-update-record record data-list) 	;; Record founded... update it.
	  (bbdb-create-internal :name (cdr (assoc 'name data-list))      ;; name
				:affix (cdr (assoc 'affix data-list))     ;; affix
				:aka (cdr (assoc 'aka data-list))       ;; akas
				:organization(cdr (assoc 'org data-list))       ;; organizations
				:mail (cdr (assoc 'mail data-list))      ;; mails
				:phone (cdr (assoc 'phones data-list))    ;; phones
				:address (cdr (assoc 'addresses data-list)) ;; addresses
				)
	  )
	)
      )
    )
  )


(defun bbdb-wl-return-list (string-or-list)
  "Return a list no matter if the parameter is string or list.
If is a String return a list containing only the string.
If is a list just return it."
  (if (stringp string-or-list)
      (list string-or-list)
    string-or-list)
  )

(defun bbdb-wl-update-record (record data-list)
  "Update the information from the data-list to the record.

\"Update\" in this case means replace all!

Data list is an alist like this:
name        => A String
affix       
aka         => A list of Strings
org         => A list of Strings
mail        => A comma separated list or a list of strings
phones      => A list of vectors with the form [\"label\" areacode prefix suffix extension-or-nil]
addresses   => A list of vectors with the form [\"label\" (\"line1\" \"line2\" ...) \"City\" \"State\" \"Postcode\" \"Country\"]
notes       => A list of associating symbols with strings"
  (let ((name (cdr (assoc 'name data-list)))      ;; name
	(affix (cdr (assoc 'affix data-list)))    ;; affix
	(aka (cdr (assoc 'aka data-list)))       ;; akas	     
	(org (cdr (assoc 'org data-list)))       ;; organizations
	(mail (cdr (assoc 'mail data-list)))      ;; mails
	(phones (cdr (assoc 'phones data-list)))    ;; phones
	(addresses (cdr (assoc 'addresses data-list))) ;; addresses
	(notes (cdr (assoc 'notes data-list))))     ;; notes
    (let ((name (bbdb-wl-return-list name))
	  (affix (bbdb-wl-return-list affix))
	  (aka (bbdb-wl-return-list aka))
	  (org (bbdb-wl-return-list org))
	  (mail (bbdb-wl-return-list mail))
	  (phones (bbdb-wl-return-list phones))
	  (addresses (bbdb-wl-return-list addresses))
	  (notes (bbdb-wl-return-list notes))
	  )
		     
      (when name ;; there is a name
	(bbdb-record-set-name name))
      (when affix
	(bbdb-record-set-affix affix))
      (when aka
	(bbdb-record-set-aka aka))
      (when mail
	(bbdb-record-set-mail mail))
      (when phones 
	(bbdb-record-set-phone phones))
      (when addresses
	(bbdb-record-set-address addresses))
      (when notes
	(bbdb-record-set-notes notes))  
      )
    )
  )

(defun bbdb-wl-addr-in-ignore (addr)
  "Check if this ADDR is in the ignore record. ADDR must be the email address only."
  (let ((record (bbdb-wl-find-mail addr))
	)
    (if record
	(if (equal (bbdb-record-name record) bbdb-wl-ignore-name)
	    t
	  nil
	  )
      nil
      )
    )
  )

(defun bbdb-wl-find-name (name)
  "Find this name in the BBDB. 
If it does exists return the record.
If nothing founds, return nil."
  (bbdb-search (bbdb-records) name)
  )

(defun bbdb-wl-find-and-show-mail (mail)
  "Search for the mail in the BBDB and show the record.
Return all the records associated to that mail and that are in the BBDB."
  (bbdb-wl-find-mail mail t)
  )

(defun bbdb-wl-find-mail (mail &optional show)
  "Look for the mail in the BBDB.

If show is t then show it!
 
Return the records of the BBDB found.
If no records are found, just return nil.

If mail is an empty string, to avoid showing all the people in BBDB this function will return nil."  
  (if (string= mail "")      
      (progn
	(bbdb-display-records nil) ;; Display nothing, erase last display.
	nil
	)
    (progn
      (let ((records (bbdb-search (bbdb-records) nil nil mail)))
	(when (and show
		   records)
	  (bbdb-display-records records)
	  )
	records
	)
      )
    )
  )


(defun bbdb-wl-find-and-show (name)
  "Look for the name in the BBDB and show it! 
Return the records of the BBDB found.
If no records are found, just return nil.

If name is an empty string, to avoid showing all the people in BBDB this function will return nil."
  (bbdb-wl-find name t)
  )

(defun bbdb-wl-find (name &optional show)
  "Look for the name in the BBDB.

If show is t then show it!
 
Return the records of the BBDB found.
If no records are found, just return nil.

If name is an empty string, to avoid showing all the people in BBDB this function will return nil."
  (if (string= name "")      
      (progn
	(bbdb-display-records nil) ;; Display nothing, erase last display.
	nil
	)
    (progn
      (let ((records (bbdb-search (bbdb-records) name)))
	(when (and show
		   records)
	  (bbdb-display-records records)
	  )
	records
	)
      )
    )
  )




(provide 'bbdb-wl-bbdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-wl-bbdb.el ends here
