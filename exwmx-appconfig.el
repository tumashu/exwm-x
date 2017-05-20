;;; exwmx-appconfig.el --- Send string to application

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

;; * exwmx-appconfig manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)

(defvar exwmx-appconfig-file (locate-user-emacs-file "exwm-x/exwmx-appconfig")
  "Application's Exwm-X configure file, Which will be used by
 many Exwm-X's commands.")

(defvar exwmx-appconfig-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'exwmx-appconfig-finish)
    (define-key keymap "\C-c\C-q" 'exwmx-appconfig-ignore)
    keymap)
  "Keymap for `exwmx-appconfig-mode'")

(define-minor-mode exwmx-appconfig-mode
  "Minor for exwmx-appconfig."
  nil " exwmx-appconfig" exwmx-appconfig-mode-map)

(defvar exwmx-appconfig-buffer " *exwmx-appconfig*")

;; Fix compile warn
(defvar exwmx-sendstring-default-paste-key)

(defun exwmx-appconfig--search (string search-prop return-prop &optional equal)
  (when (and string (stringp string))
    (let ((file (expand-file-name exwmx-appconfig-file))
          appconfigs search-result)
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (ignore-errors
            (while (not (eobp))
              (push (read (current-buffer)) appconfigs)))))
      (dolist (x appconfigs)
        (let ((search-string (plist-get x search-prop))
              (value (plist-get x return-prop)))
          (if equal
              (when (equal search-string string)
                (setq search-result value)
                (setq appconfig nil))
            (when (or (exwmx--string-match-p string search-string)
                      (exwmx--string-match-p search-string string))
              (setq search-result value)
              (setq appconfig nil)))))
      search-result)))

(defun exwmx-appconfig ()
  (interactive)
  (if (not (derived-mode-p 'exwm-mode))
      (message "Exwm-X: No app is found.")
    (unless (file-readable-p exwmx-appconfig-file)
      (append-to-file "" nil exwmx-appconfig-file))
    (let ((buffer (get-buffer-create exwmx-appconfig-buffer))
          (string (format "%S" (list :alias exwm-instance-name
                                     :paste-key exwmx-sendstring-default-paste-key
                                     :class exwm-class-name
                                     :instance exwm-instance-name
                                     :title exwm-title))))
      (with-current-buffer buffer
        (emacs-lisp-mode)
        (exwmx-appconfig-mode)
        (erase-buffer)
        (insert string)
        (goto-char (point-min))
        (setq header-line-format
              (substitute-command-keys
               (concat
                "\\<exwmx-appconfig-mode-map>"
                "Finish with `\\[exwmx-appconfig-finish]', "
                "Ignore with `\\[exwmx-appconfig-ignore]'. "))))
      (pop-to-buffer buffer))))

(defun exwmx-appconfig-finish ()
  (interactive)
  (if exwmx-appconfig-mode
      (let ((string (buffer-string)))
        (delete-window)
        (kill-buffer exwmx-appconfig-buffer)
        (append-to-file string nil exwmx-appconfig-file))
    (message "Exwm-X: exwmx-appconfig-mode is not enabled.")))

(defun exwmx-appconfig-ignore ()
  (interactive)
  (if exwmx-appconfig-mode
      (progn
        (delete-window)
        (kill-buffer exwmx-appconfig-buffer))
    (message "Exwm-X: exwmx-appconfig-mode is not enabled.")))

(provide 'exwmx-appconfig)

;;; exwmx-appconfig.el ends here
