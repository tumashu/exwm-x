;;; exwmx-sendstring.el --- Send string to application

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 1.0
;; Keywords: window-manager, exwm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * exwmx-sendstring manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)
(require 'exwmx-appconfig)
(require 'counsel)

(defvar exwmx-sendstring-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'exwmx-sendstring-finish)
    (define-key keymap "\C-c\C-q" 'exwmx-sendstring-ignore)
    keymap)
  "Keymap for `exwmx-sendstring-mode'")

(define-minor-mode exwmx-sendstring-mode
  "Minor for exwmx-sendstring."
  nil " exwmx-sendstring" exwmx-sendstring-mode-map)

(defvar exwmx-sendstring-buffer " *exwmx-sendstring*")
(defvar exwmx-sendstring-default-paste-key "C-v")

(defun exwmx-sendstring--send (string)
  "Send `string' to clipboard and then send paste key to
application to trigger paste operation, `string' will be
inserted into the application."
  (if (derived-mode-p 'exwm-mode)
      (let ((paste-key
             (or (plist-get (exwmx-appconfig--search
                             `((:class ,exwm-class-name)
                               (:instance ,exwm-instance-name)))
                            :paste-key)
                 exwmx-sendstring-default-paste-key)))
        (kill-new string)
        (dolist (key (string-to-list (kbd paste-key)))
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))

(defun exwmx-sendstring ()
  "Pop up a buffer and let user input, edit and send string to application."
  (interactive)
  (let ((buffer (get-buffer-create exwmx-sendstring-buffer)))
    (with-current-buffer buffer
      (text-mode)
      (exwmx-sendstring-mode)
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\<exwmx-sendstring-mode-map>"
              "Sendstring: "
              "Finish with `\\[exwmx-sendstring-finish]', "
              "Ignore with `\\[exwmx-sendstring-ignore]'. "))))
    (pop-to-buffer buffer)))

(defun exwmx-sendstring-finish ()
  "Send the string in buffer and delete window."
  (interactive)
  (if exwmx-sendstring-mode
      (let ((string (buffer-string)))
        (delete-window)
        (kill-buffer exwmx-sendstring-buffer)
        (exwmx-sendstring--send string))
    (message "EXWM-X: exwmx-sendstring-mode is not enabled.")))

(defun exwmx-sendstring-ignore ()
  "Ignore send string to application."
  (interactive)
  (if exwmx-sendstring-mode
      (progn
        (delete-window)
        (kill-buffer exwmx-sendstring-buffer))
    (message "EXWM-X: exwmx-sendstring-mode is not enabled.")))

(defun exwmx-sendstring-from-minibuffer ()
  "Read a string with minibuffer and send it to application."
  (interactive)
  (exwmx-sendstring--send
   (read-from-minibuffer "EXWM-X: please input: ")))

(defun exwmx-sendstring-from-kill-ring ()
  "Show `kill-ring' with ivy, and send selectd to application."
  (interactive)
  (if (featurep 'counsel)
      (if (not (derived-mode-p 'exwm-mode))
          (call-interactively 'counsel-yank-pop)
        (let* ((ivy-format-function #'counsel--yank-pop-format-function)
               (ivy-height 5)
               (cands (mapcar #'ivy-cleanup-string
                              (cl-remove-if
                               (lambda (s)
                                 (or (< (length s) 3)
                                     (string-match "\\`[\n[:blank:]]+\\'" s)))
                               (delete-dups kill-ring))))
               (string (completing-read "kill-ring: " cands)))
          (exwmx-sendstring--send string)))
    (message "EXWM-X: counsel is required.")))

(provide 'exwmx-sendstring)

;;; exwmx-sendstring.el ends here
