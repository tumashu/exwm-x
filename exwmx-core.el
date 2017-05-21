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

(defvar exwmx-terminal-emulator "xterm"
  "exwmx default terminal emulator")

(defvar exwm--keyboard-grabbed)
(declare-function exwmx--update-mode-line "exwmx-button" nil)
(declare-function exwmx-appconfig--search "exwmx-appconfig"
                  '(string search-prop return-prop &optional equal))

(defun exwmx--string-match-p (regexp string)
  "A wrap of `string-match-p', it can work when `string' is not a
string."
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string)))

(defun exwmx--switch-window ()
  (when (featurep 'switch-window)
    (switch-window--then
     "Run command in window: "
     #'(lambda () (other-window 1))
     nil nil 1)))

(defun exwmx--get-pretty-name ()
  "Get a pretty name of a application, based on its class-name, instance-name
and title."
  (let ((prefer-name (exwmx-appconfig--search exwm-class-name :class :pretty-name t)))
    (cond ((and (> (length exwm-title) 0)
                (< (length exwm-title) 10)) exwm-title)
          (prefer-name prefer-name)
          (exwm-instance-name exwm-instance-name)
          (exwm-class-name exwm-class-name))))

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
