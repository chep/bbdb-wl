;;; bbdb-wl-common.el ---
;;
;; Filename: bbdb-wl-common.el
;; Description:
;; Author: Christian
;; Maintainer:
;; Created: sáb feb 18 02:38:01 2012 (-0300)
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
;; Common functions for manipulating bbdb, and addrbook together.
;; Also add more functionallity to bbdb and addrbook by requiring two
;; libraries: bbdb-wl-bbdb and bbdb-wl-addrmgr.
;;
;; Here you can find those functions that need both, BBDB and AddrMgr.
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

(require 'bbdb-wl-bbdb)
(require 'bbdb-wl-addressbook)
(require 'bbdb-wl-syncbuffer)


;; This is function still under development... In later versions will change!
(defun bbdb-wl-sinchronize-addressbook ()
  "Sinchronize BBDB with Wanderlust's Addressbook."
  ;;(interactive)
  ;; We need that the address buffer has been visited...
  (let ((buff (get-buffer "Address")))
    (when buff
      ;; it has been visited...
      (with-current-buffer buff
	;; For each item in the addressbook add it into the BBDB(if don't exists)
	(dolist (person wl-addrmgr-list)
	  (unless (bbdb-wl-find-name (nth 2 person))
	    ;; It doesn't exists... create it
	    (message "%s %s %s %s" "Adding into BBDB: " (nth 0 person) (nth 1 person) (nth 2 person))
	    (bbdb-create-internal :name (nth 2 person) ;; name
				              :aka (cons (nth 1 person) nil) ;; aka
				              :mail (cons (nth 0 person) nil);; mail
				              )
	    )
	  )
	)
      )
    )
  )

(defun bbdb-wl-update-record-with-addrbook (record addrbook-entry)
  "Update the information from the addrbook-entry to the record.
Update aka and mail information checking if they are not there already.

This will be saved temporally in the BBDB. For saving to the database files use `bbdb-save'.
"
  (let ((aka (nth 1 addrbook-entry))
	(mail (nth 0 addrbook-entry)))
    ;; The record exists(and is only one!), update data...
    (unless (member aka (bbdb-record-aka record))
      ;; The petname is not in the record! add it...
      (bbdb-record-set-aka record
			   (cons aka (bbdb-record-aka record)))

      )
    (unless (member mail (bbdb-record-mail record))
      ;; The mail is not there! add it...
      (bbdb-record-set-mail record
			    (cons mail (bbdb-record-mail record)))
      )
    record
    )
  )

					; ****************************************
					; Getting information from the Addressbook


(defun bbdb-wl-take-data-from-addressbook ()
  "Sinchronize in one way: from the addressbook to the BBDB."
  (interactive)

  (bbdb-wl-syncbuffer-init)
  (bbdb-wl-syncbuffer)
  (message "Starting Syncrhonization from the Wanderlust's Addressbook...
Please wait...")
  (bbdb-wl-syncbuffer-show "Starting Syncrhonization from the Wanderlust's Addressbook...")

  ;; Get the list of people
  (let ((lst (wl-addrmgr-local-list t)))
    ;; process each element adding to the BBDB if necessary
    (dolist (i lst)

      (bbdb-wl-syncbuffer-addrmgr-taking i)

      (bbdb-wl-update-if-necessary i)
      )
    )

  (message "Synchronization done. Check *Sync buffer* to see results.")
  (bbdb-wl-syncbuffer-show "Synchronization done.")
  )



(defun bbdb-wl-clean-name (name)
  "Make the name a \"clean name\" means that it will erase all rare characters like double or single quotes, etc."
  (let ((res name)
	)
    (setq res (replace-regexp-in-string "\"" "" res))
    (setq res (replace-regexp-in-string "'" "" res))


    res))

(defun bbdb-wl-update-if-necessary (addrbook-entry)
  "Insert or update this addrbook-entry into BBDB if it is not there.
If the addrbook-entry is not in BBDB, insert the name, aka(petname) and mail.

Checks emails and aka
If the addrbook-entry is in the BBDB but hasn't that email insert it
If the addrbook-entry is in the BBDB but hasn't that aka insert it

If already exists all that information, do nothing."
  ;; Look for the contacts, if exists check the info.
  (let ((records (bbdb-wl-find (nth 2 addrbook-entry))))
    (cond ((eq (length records) 1) ;; Exists and is the only one contact
	   (progn
	     (bbdb-wl-syncbuffer-show "One matching record founded... updating info.")
	     (bbdb-wl-update-record-with-addrbook (car records) addrbook-entry))
	   )


	  ((eq (length records) 0) ;; Doesn't exists.. create it
	  (progn
	    (bbdb-wl-syncbuffer-show "Record doesnt exists... creating it!")
	    (bbdb-wl-insert-if-not-exists (list
					   (cons 'mail (nth 0 addrbook-entry))
					   (cons 'aka (nth 1 addrbook-entry))
					   (cons 'name (bbdb-wl-clean-name (nth 2 addrbook-entry))))))
	  )

	  ((> (length records) 1) ;; There are more records to update!!! Ask user!
	   (progn
	     (bbdb-wl-syncbuffer-show "There are various matches for record... asking user.")
	     (let ((selected (-
			      (bbdb-wl-ask-what-to-update records addrbook-entry)
			      1)))
	       (when selected
		 ;; User selected one... update it!
		 (let ((record-selected (nth selected records)))
		   (bbdb-wl-syncbuffer-show "User selects "
					    (bbdb-record-name record-selected))
		   (bbdb-wl-update-record-with-addrbook record-selected
							addrbook-entry)))))
	   )

	  )
    )
  )

(defun bbdb-wl-ask-what-to-update (records addrbook-entry)
  "Ask the user what record tu update with the addrbook-entry.
Show the data to the user so he will understand which record to choice and show the options available.

Return the position number of the selected record or nil if user doesn't selected one. "
  (let ((msg
	 (concat "I have found two candidate to update, both with simmilar names.
Wich one I have to update?
What that will be updated:\n"
		 "Email: " (nth 0 addrbook-entry)
		 " - Petname: " (nth 1 addrbook-entry)
		 "\nThe candidates are\n"
		 (bbdb-wl-get-records-names records "\n" t)
		 "\nPlease, select one by its number."))
	(selected 0))
    (while (or (> selected (length records))
	       (< selected 1))
      (setq selected (read-number msg ))
      )
    selected
    )
  )

(defun bbdb-wl-get-records-names (records &optional separator count)
  "Return a string with all the names taken from the records.

Use separator for divide between names, if it is not given or is nil then use \"\n\".

If count is t then put a number before the name."
  (let ((aux "")
	(num 0)
	(sep (if separator
		 separator
	       "\n")))
    (dolist (i records)
      (setq num (+ 1 num))
      (setq aux (concat
		 aux
		 ;; Number first
		 (when count
		   (number-to-string num))
		 ": "
		 ;; Name in the record
		 (bbdb-record-name i)
		 ;; Separator
		 sep))
      )
    aux
    )
  )



					; ****************************************
					; Giving information to the addressbook

(defun bbdb-wl-give-data-to-addressbook ()
  "Sinchronize in one way: from the BBDB to the addressbook."
  (interactive)
  (bbdb-wl-syncbuffer-init)
  (let ((list (bbdb-records)))
    ;; For each element save info
    (dolist (elt list)
      (bbdb-wl-syncbuffer-bbdb-taking elt)
      (let ((name (bbdb-record-name elt))
	    (petname (bbdb-record-aka elt))
	    (lst-emails (bbdb-record-mail elt)) ;; Take all emails!
	    )

	;; if petname is a list just take only one petname!
	(when (listp petname)
	    (setq petname (car petname))
	  )

	;; For each email address save info...
	(dolist (email lst-emails)
	  ;; Update or add it when necessary...
	  (bbdb-wl-update-addrmgr-if-necessary (list email petname name))
	  )
	)
      )
    (bbdb-wl-addrmgr-save)
    (bbdb-wl-syncbuffer-ending)
    )
  )


(provide 'bbdb-wl-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-wl-common.el ends here
