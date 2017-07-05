;;; exwmx-grocery.el --- Exwm-X utils which is hard classed.

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

;; * exwmx-grocery manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)
(require 'exwmx-appconfig)

(defun exwmx-grocery--manage-finish-function ()
  "Hook function, used by `exwm-manage-finish-hook'
in exwmx-example.el."
  (let* ((appconfig (exwmx-appconfig--search
                     `((:class ,exwm-class-name)
                       (:instance ,exwm-instance-name))))
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

(defun exwmx-grocery--rename-exwm-buffer ()
  "Hook function, used by `exwm-update-class-hook' and
`exwm-update-title-hook' in exwmx-example.el."
  (exwm-workspace-rename-buffer
   (concat "Exwm:" (exwmx-grocery--get-pretty-name))))

(defun exwmx-grocery--get-pretty-name ()
  "Get a pretty name of an application, based on application's
:pretty-name, :class, :instance or :title which is stored in
`exwmx-appconfig-file'."
  (let ((prefer-name
         (plist-get (exwmx-appconfig--search
                     `((:class ,exwm-class-name)
                       (:instance ,exwm-instance-name))
                     '(:pretty-name))
                    :pretty-name)))
    (cond ((and (> (length exwm-title) 0)
                (< (length exwm-title) 10)) exwm-title)
          (prefer-name prefer-name)
          (exwm-instance-name exwm-instance-name)
          (exwm-class-name exwm-class-name))))

(defvar exwmx-grocery--unset-key nil
  "Record the key which will be unset from emacs.")

(defun exwmx-grocery-unset-key (key)
  "Unset `key' from emacs's all buffers."
  ;; `global-set-key' only run once, for `exwm-input-set-key'
  ;;  will use `global-set-key' to set key.
  (global-set-key (kbd key) nil)
  (setq exwmx-grocery--unset-key key)
  (add-hook 'buffer-list-update-hook
            #'exwmx-grocery--unset-key)
  (message "Exwm-X: unset key %S from all buffers." key))

(defun exwmx-grocery--unset-key ()
  "Internal function, used by `exwmx-grocery-unset-key'."
  (let ((key exwmx-grocery--unset-key))
    (when key
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (local-set-key (kbd key) nil))))))

(provide 'exwmx-grocery)

;;; exwmx-grocery.el ends here
