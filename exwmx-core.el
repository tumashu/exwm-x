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

(defvar exwmx-terminal-emulator "xterm"
  "Exwm-X default terminal emulator")

(defvar exwm--keyboard-grabbed)
(declare-function exwmx-button--update-button-line "exwmx-button" nil)
(declare-function exwmx-appconfig--select-appconfig "exwmx-appconfig" ())
(declare-function exwmx-appconfig--get-all-appconfigs "exwmx-appconfig" ())
(declare-function exwmx-appconfig--add-appconfig "exwmx-appconfig" (appconfig))
(declare-function exwmx-appconfig--search "exwmx-appconfig"
                  (string search-prop return-prop &optional equal))

(defun exwmx--string-match-p (regexp string)
  "A wrap of `string-match-p', it can work when `string' is not a
string."
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string)))

(defun exwmx--switch-window ()
  "A switch-window command's wrapper used by Exwm-X.

Note: switch-window default input style do not work well
with Exwm-x, user should use 'minibuffer input style instead,
for example, add the following line:

  (setq switch-window-input-style 'minibuffer)

to your ~/.emacs file."
  (unless (eq switch-window-input-style 'minibuffer)
    (warn (concat "Exwm-X: please setq the value of "
                  "`switch-window-input-style' to 'minibuffer.")))
  (switch-window--then
   "Run command in window: "
   #'(lambda () (other-window 1))
   nil nil 1))

(defun exwmx-manage-finish-function ()
  "Exwm-X hook function, used by `exwm-manage-finish-hook'."
  (let* ((appconfig (exwmx-appconfig--search exwm-class-name :class t t))
         (floating (plist-get appconfig :floating))
         (workspace (plist-get appconfig :workspace))
         (prefix-keys-added (plist-get appconfig :add-prefix-keys))
         (prefix-keys-removed (plist-get appconfig :remove-prefix-keys))
         (ignore-simulation-keys (plist-get appconfig :ignore-simulation-keys))
         (expression (plist-get appconfig :eval)))
    ;; Deal with prefix-keys of application
    (when (and prefix-keys-removed
               (listp prefix-keys-removed))
      (dolist (key prefix-keys-removed)
        (setq-local exwm-input-prefix-keys
                    (remove key exwm-input-prefix-keys))))
    (when (eq prefix-keys-removed t)
      (setq-local exwm-input-prefix-keys nil))
    (when (and prefix-keys-added
               (listp prefix-keys-added))
      (setq-local exwm-input-prefix-keys
                  (append prefix-keys-added exwm-input-prefix-keys)))
    ;; Deal with simulation-keys of application
    (when ignore-simulation-keys
      (exwm-input-set-local-simulation-keys nil))
    ;; Deal with window floating
    (when floating
      (exwm-floating--set-floating exwm--id))
    ;; Eval the expression from :eval
    (when expression
      (eval expression))
    ;; Switch application's window to workspace
    (when (numberp workspace)
      (exwm-workspace-move-window workspace)
      (exwm-workspace-switch-create workspace))))

(defun exwmx--get-pretty-name ()
  "Get a pretty name of an application, based on application's :pretty-name,
:class, :instance or :title which is stored in `exwmx-appconfig-file'."
  (let ((prefer-name (exwmx-appconfig--search exwm-class-name :class :pretty-name t)))
    (cond ((and (> (length exwm-title) 0)
                (< (length exwm-title) 10)) exwm-title)
          (prefer-name prefer-name)
          (exwm-instance-name exwm-instance-name)
          (exwm-class-name exwm-class-name))))

(defun exwmx-switch-application ()
  "Select an application and switch to it."
  (interactive)
  (let ((buffer-name
         (ivy-read "Exwm-X switch application: "
                   (mapcar
                    #'(lambda (x)
                        (buffer-name (cdr x)))
                    exwm--id-buffer-alist))))
    (exwm-workspace-switch-to-buffer buffer-name)))

(defun exwmx-shell-command (cmd)
  "Run shell command `cmd'."
  (start-process-shell-command cmd nil cmd))

(defun exwmx-shell-command-interactively (cmd)
  "Run shell command `cmd' interactively."
  (interactive
   (list (read-shell-command "Run shell command: ")))
  (start-process-shell-command cmd nil cmd))


;; * Footer
(provide 'exwmx-core)

;;; exwmx-core.el ends here
