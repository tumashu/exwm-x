;;; exwmx-quickrun.el --- Exwm-X's application launcher

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

;; * exwmx-quickrun manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)

(defun exwmx-jump-or-exec (command &optional _current-window search-alias extra-keys)
  "A wrap command of `exwmx-quickrun' for compatible, suggest to
use `exwmx-quickrun' instead."
  (exwmx-quickrun command search-alias extra-keys))

(defun exwmx-quickrun (command &optional search-alias keys)
  "Run `command' to launch an application, if application's window is found,
switch to this window instead of launching new one, when `search-alias' is t,
`command' will be regard as an appconfig alias and search it from
`exwmx-appconfig-file', by default, application's class and instance is used
to switch, user can override by `keys', it support :class, :instance, and
:title at the moment."
  (exwmx--switch-window)
  (let* ((returned-keys
          (if (and keys (listp keys))
              keys
            '(:class :instance)))
         (appconfigs (exwmx-appconfig--get-all-appconfigs))
         (cmd (if search-alias
                  (or (plist-get (exwmx-appconfig--search
                                  `((:alias ,command))
                                  '(:command))
                                 :command)
                      (when appconfigs
                        (let ((appconfig (exwmx-appconfig--select-appconfig)))
                          (plist-put appconfig :alias command)
                          (exwmx-appconfig--add-appconfig appconfig)
                          (plist-get appconfig :command))))
                command))
         (buffer (or (if search-alias
                         (exwmx-quickrun--find-buffer
                          (exwmx-appconfig--search
                           `((:alias ,command))
                           returned-keys))
                       (exwmx-quickrun--find-buffer
                        (exwmx-appconfig--search
                         `((:command ,command))
                         returned-keys)))
                     ;; The below two rules are just guess rules :-)
                     ;; Suggest use `exwmx-appconfig' to manage app's information.
                     (exwmx-quickrun--find-buffer
                      `(:class ,(capitalize (concat "^" (car (split-string command " "))))))
                     (exwmx-quickrun--find-buffer
                      `(:class ,(concat "^" (car (split-string command " "))))))))
    (if (and search-alias (not cmd))
        (message "Exwm-X: please run `exwmx-appconfig' to add appconfig.")
      (message "Exwm-X Quick Run: %s" cmd))
    ;; If current application window is a floating-window, minumize it.
    (when (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
      (exwm-floating-hide))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (when cmd
        (exwmx-shell-command cmd)))))

(defun exwmx-quickrun--find-buffer (ruler)
  "Find a exwm buffer which match `ruler', ruler is
a plist with three keys: :class, :instance and :title."
  (let ((current (current-buffer))
        (buffers (buffer-list))
        (result '()))
    (while buffers
      (let ((buffer (pop buffers))
            (class (plist-get ruler :class))
            (instance (plist-get ruler :instance))
            (title (plist-get ruler :title)))
        (with-current-buffer buffer
          (when (and class
                     (exwmx--string-match-p class exwm-class-name)
                     (exwmx--string-match-p (or instance ".*") exwm-instance-name)
                     (exwmx--string-match-p (or title ".*") exwm-title))
            (push buffer result)))))
    (setq result (reverse result))
    ;; If two more buffers are found, switch between these buffer.
    (if (and (cadr result)
             (eq (car result) current))
        (cadr result)
      (car result))))

(provide 'exwmx-quickrun)

;;; exwmx-quickrun.el ends here
