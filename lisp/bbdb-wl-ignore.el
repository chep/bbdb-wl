;;; bbdb-wl-ignore.el --- 

;; Copyright 2013 Giménez, Christian
;;
;; Author: Giménez, Christian
;; Version: $Id: bbdb-wl-ignore.el,v 0.0 2013/10/29 00:35:14 christian Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.




;;; Commentary:

;; This library manages the ignore file.
;;
;; The ignore file stores email address that shouldn't be added to the
;; BBDB.
;; The format is simple: one address per line. You can use
;; `bbdb-wl-ignored-addr' to test if an email address is in the file
;; and `bbdb-wl-ignore-add' to add one.



;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'bbdb-wl-ignore)

;;; Code:

(provide 'bbdb-wl-ignore)

(defgroup bbdb-wl-ignore nil
  "Criteria and variables that affect the method for ignoring addresses."
  :group 'bbdb-wl
  :tag "Ignoring Address Criteria and Variables."
  )

(defcustom bbdb-wl-ignore-file "~/.emacs.d/bbdb-wl-ignore.txt"
  "This is where all the ignore data is stored. When the user wants to ignore some email address, in this file will be stored."
  :group 'bbdb-wl-ignore
  :type 'file)

(defconst bbdb-wl-ignore-buffer " *bbdb-wl-ignore-file*"
  "Name of the `bbdb-wl-ignore-file' buffer"
  )

(defun bbdb-wl-ignore-load-if-needed ()
  (unless (get-buffer bbdb-wl-ignore-buffer)
    (with-current-buffer (get-buffer-create bbdb-wl-ignore-buffer)
      (delete-region (point-min) (point-max))
      (when (file-exists-p bbdb-wl-ignore-file)
	(insert-file-contents bbdb-wl-ignore-file)
	)
      )
    )
  )

(defun bbdb-wl-ignored-addr (address)
  "Check if this ADDR is in the ignore record. ADDR must be the email address only."
  (bbdb-wl-ignore-load-if-needed)

  (with-current-buffer bbdb-wl-ignore-buffer
    (goto-char (point-min))
    (if (search-forward-regexp (concat "^" address "$") nil t)
	t
      nil
      )
    )
  )

(defun bbdb-wl-ignore-add (address)
  "Add a new email ADDRESS into the ignore file. ADDRESS must be the email address only."
  (bbdb-wl-ignore-load-if-needed)

  (unless (bbdb-wl-ignored-addr address)
    (with-current-buffer bbdb-wl-ignore-buffer
      (goto-char (point-max))
      (insert address "\n")
      (write-region nil nil bbdb-wl-ignore-file)
      )
    )
  )

(defun bbdb-wl-ignore-edit ()
  "Show the ignore file for the user."
  (interactive)
  (find-file bbdb-wl-ignore-file)
  (message "This is the bbdb-wl ignore file. This addresses are not searched nor showed.
Once finished, just save and close (C-x C-s and C-x k ENTER).")
  )

;;; bbdb-wl-ignore.el ends here
