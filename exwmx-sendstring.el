;;; exwmx-sendstring.el --- Send string to application

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.3
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
(require 'exwm)
(require 'exwmx-core)
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

(defun exwmx-sendstring--send (string)
  "Send `string' to clipboard and then send paste key to
application to trigger paste operation, `string' will be
inserted into the application."
  (if (derived-mode-p 'exwm-mode)
      (let ((paste-key exwmx-send-paste-key)
            (paste-key-alist exwmx-send-paste-key-alist)
            x)
        (while paste-key-alist
          (setq x (pop paste-key-alist))
          (when (or (string-match-p (car x) exwm-instance-name)
                    (string-match-p (car x) exwm-class-name))
            (setq paste-key (cdr x))
            (setq paste-key-alist nil)))
        (kill-new string)
        (dolist (key (string-to-list (kbd paste-key)))
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))

(defun exwmx-sendstring ()
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
              "Finish with `\\[exwmx-sendstring-finish]', "
              "Ignore with `\\[exwmx-sendstring-ignore]'. "))))
    (pop-to-buffer buffer)))

(defun exwmx-sendstring-finish ()
  (interactive)
  (let ((string (buffer-string)))
    (delete-window)
    (kill-buffer exwmx-sendstring-buffer)
    (exwmx-sendstring--send string)))

(defun exwmx-sendstring-ignore ()
  (interactive)
  (delete-window)
  (kill-buffer exwmx-sendstring-buffer))

(defun exwmx-sendstring-from-minibuffer ()
  (interactive)
  (exwmx-sendstring--send
   (read-from-minibuffer "Exwm-X: please input: ")))

(defun exwmx-sendstring-from-kill-ring ()
  (interactive)
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
      (exwmx-sendstring--send string))))

(provide 'exwmx-sendstring)

;;; exwmx-sendstring.el ends here
