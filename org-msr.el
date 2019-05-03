;;; org-msr.el --- summary -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4"))
;; Homepage: https://kisaragi-hiu.com/projects/org-msr
;; Keywords: convenience org


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Minimal spaced repetition setup.

;;; Code:
(require 'org)
(eval-when-compile
  (require 'pcase))

(defgroup org-msr nil
  "Minimal spaced repetition setup in Org."
  :group 'org
  :prefix "org-msr-")

(defcustom org-msr-frequency-alist
  '(("NOMEMORY/5h" . "5h")
    ("DAILY/1d" . "1d")
    ("HARD/3d" . "3d")
    ("UNFAMILIAR/1w" . "1w")
    ("SOMEWHAT/2w" . "2w")
    ("FAMILIAR/1m" . "1m")
    ("EASY/6m" . "6m")
    ("CONFIDENT/1y" . "1y")
    ("MEMORIZED/3y" . "3y"))
  "Alist mapping TODO keywords in the memory file to repeater frequencies."
  :group 'org-msr
  :type '(alist :key-type string :value-type string))

(defcustom org-msr-setup-heading-name "Org-msr Setup"
  "Heading name for Org-msr setup code."
  :group 'org-msr
  :type 'string)

(defcustom org-msr-filetags '("org-msr")
  "Filetags added when setting up org-msr with `org-msr-set-up-file'."
  :group 'org-msr
  :type '(repeat string))

(defun org-msr--refresh-org ()
  "Refresh Org exactly like what \\[org-ctrl-c-ctrl-c] in a #+TODO line does."
  (let ((org-inhibit-startup-visibility-stuff t)
        (org-startup-align-all-tables nil))
    (when (boundp 'org-table-coordinate-overlays)
      (mapc #'delete-overlay org-table-coordinate-overlays)
      (setq org-table-coordinate-overlays nil))
    (org-save-outline-visibility 'use-markers (org-mode-restart))))

(defun org-msr-set-up-file ()
  "Add file-local setup for org-msr in this file.

Will only do anything if a heading named by
`org-msr-setup-heading-name' (default \"Org-msr Setup\") doesn't
already exist.

- Add TODO keyword definitions
- Add FILETAGS according to `org-msr-filetags'
- Tell Emacs to start org-msr-mode in this file"
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (if (search-forward (concat "* " org-msr-setup-heading-name) nil t)
        (message "%s" "Org-msr is already set up")
      (setf (point) (point-min))
      (insert "\n* " org-msr-setup-heading-name "\n"
              ;; TODO keywords
              (mapconcat (lambda (pair)
                           (format "#+TODO: %s | DONE(d)\n" (car pair)))
                         org-msr-frequency-alist
                         "\n")
              ;; FILETAGS
              (format "#+FILETAGS: :%s:\n"
                      (mapconcat #'identity org-msr-filetags ":")))
      (add-file-local-variable 'eval '(org-msr-mode 1))
      (org-msr--refresh-org)
      (message "%s" "Org-msr has been set up"))))

;;;###autoload
(define-minor-mode org-msr-mode
  "Minor mode to update repeater based on todo keywords."
  (if org-msr-mode
      (advice-add 'org-todo :after #'org-msr-update-repeater)
    (advice-remove 'org-todo #'org-msr-update-repeater)))

;;;###autoload
(defun org-msr-update-repeater (&rest _)
  "Update repeater for each org-msr item based on their familiarity."
  (interactive)
  (save-excursion
    (pcase-dolist (`(,keyword . ,frequency) org-msr-frequency-alist)
      (setf (point) (point-min))
      (while (search-forward (format "* %s" keyword) nil t)
        (if (org-get-scheduled-time (point))
            (progn (re-search-forward "SCHEDULED: <\\(.*\\) .*>" nil t)
                   (replace-match (format "SCHEDULED: <\\1 .+%s>" frequency)))
          (end-of-line)
          (insert "\n" (format "SCHEDULED: <%s .+%s>"
                               (format-time-string "%Y-%m-%d")
                               frequency)))))))

(provide 'org-msr)

;;; org-msr.el ends here
