;;; exwmx-core.el --- Core functions used by exwmx

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 1.0
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
(require 'cl-lib)
(require 'exwm)
(require 'switch-window)
(require 'ivy)
(require 'bind-key)

(defvar exwmx-terminal-emulator "xterm"
  "EXWM-X default terminal emulator")

(defvar-local exwmx-pretty-name nil
  "Record the application's pretty name.
This variable is used by exwmx-button: exwm-buffer-list.")

(defun exwmx--string-match-p (regexp string)
  "A wrap of `string-match-p', it can work when `string' is not a
string."
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string)))

(defun exwmx--switch-window ()
  "A switch-window command's wrapper used by EXWM-X.

Note: switch-window default input style do not work well
with EXWM-X, user should use 'minibuffer input style instead,
for example, add the following line:

  (setq switch-window-input-style 'minibuffer)

to your ~/.emacs file."
  (unless (eq switch-window-input-style 'minibuffer)
    (warn (concat "EXWM-X: please setq the value of "
                  "`switch-window-input-style' to 'minibuffer.")))
  (switch-window--then
   "Run command in window: "
   #'(lambda () (other-window 1))
   nil nil 1))

(defun exwmx--plist-p (list)
  "Non-null if and only if LIST is a plist with keyword keys."
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))


(defun exwmx--clean-keylist (keylist)
  "Remove all non-keyword elements of `keylist' and then remove duplicate."
  (cl-remove-duplicates
   (cl-remove-if-not #'keywordp keylist)
   :from-end t))

(defun exwmx-switch-application ()
  "Select an application and switch to it."
  (interactive)
  (let ((buffer-name
         (ivy-read "EXWM-X switch application: "
                   (mapcar
                    #'(lambda (x)
                        (buffer-name (cdr x)))
                    exwm--id-buffer-alist))))
    (exwm-workspace-switch-to-buffer buffer-name)))

(defun exwmx-shell-command (cmd)
  "Run shell command `cmd'."
  (start-process-shell-command cmd nil cmd))

(defun exwmx-terminal-emulator (command)
  "Run a `command' in a terminal emulator."
  (let ((cmd (format "%s -e 'bash -c %S'"
                     exwmx-terminal-emulator
                     (concat command "; exec bash"))))
    (message "EXWM-X run shell command: %s" cmd)
    (exwmx-shell-command cmd)))

(defun exwmx-input-set-key (key command)
  "This function is similar with `exwm-input-set-key', the
different is that `exwmx-input-set-key' protect `key' from
being override by other minor modes with the help of `bind-key*'."
  (exwm-input-set-key key command)
  (bind-key* key command))

;; * Footer
(provide 'exwmx-core)

;;; exwmx-core.el ends here
