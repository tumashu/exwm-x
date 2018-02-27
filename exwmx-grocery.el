;;; exwmx-grocery.el --- EXWM-X utils which is hard classed.

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
(require 'exwmx-floating)

(defun exwmx-grocery--manage-finish-function ()
  "Hook function, used by `exwm-manage-finish-hook'
in exwmx-example.el."
  (let* ((appconfig (exwmx-appconfig--search
                     `((:class ,exwm-class-name)
                       (:instance ,exwm-instance-name))))
         (floating (plist-get appconfig :floating))
         (pretty-name (plist-get appconfig :pretty-name))
         (size-and-position (plist-get appconfig :size-and-position))
         (workspace (plist-get appconfig :workspace))
         (prefix-keys-added (plist-get appconfig :add-prefix-keys))
         (prefix-keys-removed (plist-get appconfig :remove-prefix-keys))
         (ignore-simulation-keys (plist-get appconfig :ignore-simulation-keys))
         (expression (plist-get appconfig :eval)))
    ;; Record the application's pretty-name, which
    ;; is used by exwmx-button's exwm-buffer-list button.
    (setq-local exwmx-pretty-name
                (or pretty-name
                    exwm-instance-name
                    exwm-class-name))
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
      (exwm-floating--set-floating exwm--id)
      (setq exwmx-floating--first-floating nil)
      (apply #'exwmx-floating-adjust-window
             (if (= (length size-and-position) 4)
                 size-and-position
               exwmx-floating-default-size-and-position)))
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
  (when (buffer-live-p (current-buffer))
    (let* ((name (plist-get (exwmx-appconfig--search
                             `((:class ,exwm-class-name)
                               (:instance ,exwm-instance-name)))
                            :pretty-name))
           (name (cond ((and (> (length exwm-title) 0)
                             (< (length exwm-title) 10))
                        exwm-title)
                       (name name)
                       (exwm-instance-name exwm-instance-name)
                       (exwm-class-name exwm-class-name))))
      (exwm-workspace-rename-buffer (concat "[EXWM-X]: " name)))))

(provide 'exwmx-grocery)

;;; exwmx-grocery.el ends here
