;;; bbdb-attach.el --- attach bbdb records to mail buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: mail, convenience
;; Created: 2013-08-21

;; This program is free software; you can redistribute it and/or modify
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

;; Attach bbdb records as vCard, HTML, QR code image etc.

;; (define-key bbdb-mode-map "H" 'bbdb-attach-html)
;; (define-key bbdb-mode-map "V" 'bbdb-attach-vcard)
;; (define-key bbdb-mode-map "Z" 'bbdb-attach-qrcode)

;;; Code:

(require 'bbdb-com)
(require 'gnus-msg)
(require 'message)
(require 'mml)

(require 'qrencode nil t)
(require 'bbdb-vcard-export nil t)

(defun bbdb-attach-ensure-mail-buffer ()
  (let ((bufs (message-buffers)))
    (if (and bufs (y-or-n-p "Attach to existing mail buffer? "))
        (switch-to-buffer
         (cond ((= 1 (length bufs))
                (car bufs))
               (t (completing-read
                   (format "Which mail buffer (%d in total): " (length bufs))
                   bufs nil t))))
      ;; Set up a new mail buffer.
      (gnus-setup-message 'message (message-mail))))
  (unless (message-in-body-p)
    (goto-char (point-max))))

;;;###autoload
(defun bbdb-attach-vcard (records)
  "Attach RECORDS as vCards to a mail buffer."
  (interactive (list (bbdb-do-records)))
  (or (featurep 'bbdb-vcard-export)
      (error "Library http://www.emacswiki.org/emacs/bbdb-vcard-export.el required"))
  (bbdb-attach-ensure-mail-buffer)
  ;; Without the 'name' property some MUAs (eg gmail) may display the
  ;; attachment as 'noname'. See also (info "(emacs-mime)MML
  ;; Definition").
  (dolist (record records)
    (mml-insert-tag 'part
                    'type "text/directory"
                    'charset "utf-8"
                    'disposition "attachment"
                    'description (concat (bbdb-record-name record) ".vcf")
                    'name (concat (bbdb-record-name record) ".vcf"))
    (bbdb-vcard-export-record-insert-vcard record))
  ;; Close the 'part tag (taken from `mml-insert-empty-tag')
  (insert "<#/part>\n"))

;;;###autoload
(defun bbdb-attach-qrcode (records)
  "Attach RECORDS as QRcode images to a mail buffer."
  (interactive (list (bbdb-do-records)))
  (or (featurep 'qrencode)
      (error "Library https://github.com/leoliu/qrencode.el required"))
  (bbdb-attach-ensure-mail-buffer)
  (dolist (record records)
    (let* ((vcard (with-temp-buffer
                    (bbdb-vcard-export-record-insert-vcard record)
                    (buffer-string)))
           (image (qrencode-string vcard :size 2 :margin 2))
           (type (symbol-name (image-type-from-data image)))
           (mimetype (mailcap-extension-to-mime type))
           (name (concat (bbdb-record-name record) "." type))
           buffer action)
      (when (and mimetype image)
        (with-current-buffer (generate-new-buffer " *bbdb-attach*")
          (set-buffer-multibyte nil)
          (insert image)
          (setq buffer (current-buffer)))
        (mml-insert-tag 'part
                        'type mimetype
                        'disposition "attachment"
                        'buffer (buffer-name buffer)
                        'name name)
        (setq action `(and (buffer-live-p ,buffer) (kill-buffer ,buffer)))
        (push action message-send-actions)
        (push action message-kill-actions))))
  (insert "<#/part>\n"))

;;;###autoload
(defun bbdb-attach-html (records)
  "Attach RECORDS as vCard to a mail buffer."
  (interactive (list (bbdb-do-records)))
  (bbdb-attach-ensure-mail-buffer)
  (dolist (record records)
    (mml-insert-tag 'part
                    'type "text/html"
                    'disposition "inline")
    (bbdb-attach-insert-html record))
  (insert "<#/part>\n"))

(defun bbdb-attach-insert-html (record)
  "Insert bbdb RECORD as a html table."
  (insert "<table border=\"2\" rules=\"groups\" frame=\"hsides\">\n")
  (insert "<col width=\"100px\"></col>\n")
  (let ((name (bbdb-record-name record))
        (organizations (bbdb-record-organization record))
        (emails (bbdb-record-mail record))
        (phones (bbdb-record-phone record))
        (addresses (bbdb-record-address record)))
    (insert "<tr><td>Name:</td><td>" name "</td></tr>\n")
    (dolist (org organizations)
      (insert "<tr><td>Organisation:</td><td>" org "</td></tr>\n"))
    ;; emails
    (when emails
      (insert "<tr><td>Email:</td><td>" (pop emails) "</td></tr>\n")
      (dolist (email emails)
        (insert "<tr><td></td><td>" email "</td></tr>\n")))
    ;; phones
    (dolist (phone phones)
      (insert "<tr>")
      (insert "<td>" (aref phone 0) ":</td>")
      (insert "<td>" (aref phone 1) "</td>")
      (insert "</tr>\n"))
    ;; addresses
    (dolist (addr addresses)
      (insert "<tr>")
      (insert "<td>" (aref addr 0) ":</td>")
      (insert "<td>"
              (replace-regexp-in-string "\n" "<br>"
                                        (bbdb-format-address addr 2))
              ":</td>")
      (insert "</tr>\n")))
  (insert "</table>\n"))

(provide 'bbdb-attach)
;;; bbdb-attach.el ends here
