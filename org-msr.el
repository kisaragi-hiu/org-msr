;;; org-msr.el --- summary -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.3.0
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

(defcustom org-msr-keyword-frequency-alist
  ;; The "/1d" "/5h" suffixes are not magic, they simply help you
  ;; choose a new frequency in `org-todo'.
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

(defun org-msr--refresh-org ()
  "Refresh Org exactly like what \\[org-ctrl-c-ctrl-c] in a #+TODO line does."
  (let ((org-inhibit-startup-visibility-stuff t)
        (org-startup-align-all-tables nil))
    (when (boundp 'org-table-coordinate-overlays)
      (mapc #'delete-overlay org-table-coordinate-overlays)
      (setq org-table-coordinate-overlays nil))
    (org-save-outline-visibility 'use-markers (org-mode-restart))))

(defvar org-msr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c _") 'org-msr-update-repeater)
    map)
  "Keymap for `org-msr-mode'.")

;;;###autoload
(defun org-msr-set-up-file ()
  "Set this file up for org-msr.

Will only do anything if a heading named by
`org-msr-setup-heading-name' (default \"Org-msr Setup\") doesn't
already exist.

- Add TODO keyword definitions according to `org-msr-keyword-frequency-alist'
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
                           (format "#+TODO: %s | DONE(d)" (car pair)))
                         org-msr-keyword-frequency-alist
                         "\n"))
      (add-file-local-variable 'eval '(org-msr-mode 1))
      (org-msr--refresh-org)
      (message "%s" "Org-msr has been set up"))))

;;;###autoload
(define-minor-mode org-msr-mode
  "Org-msr (Minimal Spaced Repetition) minor mode.

Every TODO heading will get a repeating schedule based on their
TODO keyword, as defined in `org-msr-keyword-frequency-alist'.

The repeaters are updated everytime `org-todo' is called.

\\<org-msr-mode-map>
\\[org-msr-update-repeater] to explicitly update repeaters.
\\[org-msr-set-up-file] to set up file for Org-msr, which see.

\\{org-msr-mode-map}"
  :group 'org-msr :init-value nil :lighter " Org-msr"
  :keymap org-msr-mode-map
  (if org-msr-mode
      (advice-add 'org-todo :after #'org-msr-update-repeater)
    (advice-remove 'org-todo #'org-msr-update-repeater)))

;;;###autoload
(defun org-msr-update-repeater (&rest _)
  "Update repeater for each org-msr item based on their familiarity."
  (interactive)
  (save-excursion
    (pcase-dolist (`(,keyword . ,frequency) org-msr-keyword-frequency-alist)
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
