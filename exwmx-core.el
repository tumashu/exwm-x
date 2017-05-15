;;; exwmx-core.el --- Core functions used by exwmx

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.3
;; Keywords: window-manager, exwm, exwmx

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

;; * exwmx-core manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)

(defvar exwm--keyboard-grabbed)
(declare-function exwmx--update-mode-line "exwmx-button" nil)

(defvar exwmx-prefer-name-alist
  '(("navigator" . "Firefox")
    ("virtual[ ]*box" . "VirtualBox")
    ("gimp" . "Gimp")
    ("default-terminal" . "Term"))
  "Dict used by `exwmx--get-prefer-name'")

(defvar exwmx-send-paste-key "C-v")

(defvar exwmx-send-paste-key-alist
  '(("Icecat" . "C-v")
    ("Xfce4-terminal" . "C-S-v"))
  "`exwmx--send-string' will send app's paste keybinding to
trigger paste operation, some apps use special paste keybinding,
user should declare in this variable.")

(defvar exwmx-terminal-emulator "xterm"
  "exwmx default terminal emulator")

(defun exwmx--switch-window ()
  (when (featurep 'switch-window)
    (switch-window--then
     "Run command in window: "
     #'(lambda () (other-window 1))
     nil nil 1)))

(defun exwmx--get-prefer-name ()
  "Get a prefer name of a application, based on its class-name, instance-name
and title."
  (let* ((dict-alist exwmx-prefer-name-alist)
         (prefer-name
          (or (exwmx--replace-string exwm-title dict-alist)
              (exwmx--replace-string exwm-instance-name dict-alist)
              (exwmx--replace-string exwm-class-name dict-alist))))
    (cond ((and (> (length exwm-title) 0)
                (< (length exwm-title) 10)) exwm-title)
          (prefer-name prefer-name)
          (exwm-instance-name exwm-instance-name)
          (exwm-class-name exwm-class-name))))

(defun exwmx--replace-string (string dict-alist)
  "If the `string' match the car of element in `dict-alist',
return its cdr value."
  (let ((case-fold-search t)
        new-string)
    (dolist (x dict-alist)
      (when (exwmx--string-match-p (car x) string)
        (setq dict-alist nil)
        (setq new-string (cdr x))))
    new-string))

(defun exwmx--string-match-p (regexp string)
  "A wrap of `string-match-p', it can work when `string' is not a
string."
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string)))

(defun exwmx--next-exwm-buffer ()
  "Switch to next exwm buffer."
  (let ((buffer
         (car (cl-remove-if-not
               #'(lambda (buf)
                   (with-current-buffer buf
                     (eq major-mode 'exwm-mode)))
               (buffer-list)))))
    (when buffer
      (exwm-workspace-switch-to-buffer buffer))))

(defun exwmx--send-string (string)
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
    (insert-string string)))

(defun exwmx-toggle-keyboard (&optional id)
  "Toggle between 'line-mode' and 'char-mode'."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (if id
      (with-current-buffer (exwm--id->buffer id)
        (if exwm--keyboard-grabbed
            (progn
              (message "Switch to `char-mode', application will take up your keyboard.")
              (exwm-input-release-keyboard id))
          (message
           (substitute-command-keys
            (concat
             "\\<exwm-mode-map>Reset to `line-mode', "
             "`\\[exwm-input-send-next-key]' -> send next key to application.")))
          (exwm-reset)))
    (message "Exwm-x: No application is actived."))
  (exwmx--update-mode-line))

;; Override exwm's exwm-floating-toggle-floating
(defun exwm-floating-toggle-floating ()
  "Toggle the current window between floating and non-floating states."
  (interactive)
  (with-current-buffer (window-buffer)
    (if exwm--floating-frame
        (progn
          (exwm-floating--unset-floating exwm--id)
          (exwm-layout--refresh))
      (exwm-floating--set-floating exwm--id)
      (exwm-layout--refresh))))

;; * Footer
(provide 'exwmx-core)

;;; exwmx-core.el ends here
