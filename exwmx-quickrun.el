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

(defalias 'exwmx-jump-or-exec 'exwmx-quickrun)

(defun exwmx-quickrun (command &optional current-window search-alias)
  "Run `command' to launch an application, if application's window is found,
switch to this window instead of launching new one, when `search-alias' is t,
`command' will be regard as an appconfig alias and search it from
`exwmx-appconfig-file'. if `current-window' is nil, `exwmx--switch-window'
will be called, it let user select application's position."
  (unless current-window
    (exwmx--switch-window))
  (let* ((appconfigs (exwmx-appconfig--get-all-appconfigs))
         (cmd (if search-alias
                  (or (exwmx-appconfig--search command :alias :command t)
                      (when appconfigs
                        (let ((appconfig (exwmx-appconfig--select-appconfig)))
                          (plist-put appconfig :alias command)
                          (exwmx-appconfig--add-appconfig appconfig)
                          (plist-get appconfig :command))))
                command))
         (buffer (or (if search-alias
                         (exwmx-quickrun--find-buffer
                          (exwmx-appconfig--search command :alias :class t))
                       (exwmx-quickrun--find-buffer
                        (exwmx-appconfig--search command :command :class t)))
                     ;; The below two rules are just guess rules :-)
                     ;; Suggest use `exwmx-appconfig' to manage app's information.
                     (exwmx-quickrun--find-buffer
                      (capitalize (concat "^" (car (split-string command " ")))))
                     (exwmx-quickrun--find-buffer
                      (concat "^" (car (split-string command " ")))))))
    (if (and search-alias (not cmd))
        (message "Exwm-X: please run `exwmx-appconfig' to add appconfig.")
      (message "Exwm-X jump-or-exec: %s" cmd))
    ;; If current application window is a floating-window, minumize it.
    (when (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
      (exwm-floating-hide))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (when cmd
        (exwmx-shell-command cmd)))))

(defun exwmx-quickrun--find-buffer (regexp)
  "Find such a exwm buffer which local variables: `exwm-class-name',
`exwm-instance-name' or `exwm-title' is matched `regexp'."
  (when (and regexp (stringp regexp))
    (let* ((buffers (buffer-list))
           (buffers-list (list nil nil nil)))

      (dolist (buffer buffers)
        (let ((wininfo `((0 . ,(buffer-local-value 'exwm-title buffer))
                         (1 . ,(buffer-local-value 'exwm-instance-name buffer))
                         (2 . ,(buffer-local-value 'exwm-class-name buffer)))))
          (dolist (x wininfo)
            (when (exwmx--string-match-p regexp (cdr x))
              (setf (nth (car x) buffers-list)
                    (append (list buffer) (nth (car x) buffers-list)))))))

      (caar (delq nil
                  (sort buffers-list
                        #'(lambda (a b)
                            (< (length a) (length b)))))))))


(provide 'exwmx-quickrun)

;;; exwmx-quickrun.el ends here
