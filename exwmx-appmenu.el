;;; exwmx-appmenu.el --- EXWM-X application menu

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

;; * exwmx-appmenu manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'cl-lib)
(require 'exwmx-core)
(require 'counsel)

(defvar exwmx-appmenu-buffer " *exwmx-appmenu-buffer*")

(defvar exwmx-appmenu-format-function #'exwmx-appmenu-format-function-default)

(defvar exwmx-appmenu--apps-cache nil
  "Cache of desktop files data.")

(defvar exwmx-appmenu--apps-cached-files nil
  "List of cached desktop files.")

(defvar exwmx-appmenu--apps-cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar exwmx-appmenu--apps-cache-format-function nil
  "The function used to format the cached application menu.")

(defvar exwmx-appmenu-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'exwmx-appmenu-quit)
    (define-key keymap (kbd "Q") 'exwmx-appmenu-quit)
    (define-key keymap (kbd "RET") 'exwmx-appmenu-launch)
    (define-key keymap [mouse-1] 'exwmx-appmenu-launch)
    (define-key keymap [mouse-3] 'exwmx-appmenu-quit)
    keymap)
  "Keymap for `exwmx-appmenu-mode'")

(define-derived-mode exwmx-appmenu-mode read-only-mode "exwmx-appmenu-mode"
  "Exwmx-appmenu major mode.")

(defun exwmx-appmenu-format-function-default (name comment _exec)
  "Format application names with the NAME (and COMMENT) first.
EXEC is the command to launch the application."
  (format "%-30s ( %s )" name (or comment "")))

(defun exwmx-appmenu--get-apps-list ()
  "Return list of all desktop applications."
  (let* ((new-desktop-alist (counsel-linux-apps-list-desktop-files))
         (new-files (mapcar 'cdr new-desktop-alist))
         (counsel-linux-app-format-function exwmx-appmenu-format-function))
    (unless (and
             (eq counsel-linux-app-format-function
                 exwmx-appmenu--apps-cache-format-function)
             (equal new-files exwmx-appmenu--apps-cached-files)
             (null (cl-find-if
                    (lambda (file)
                      (time-less-p
                       exwmx-appmenu--apps-cache-timestamp
                       (nth 5 (file-attributes file))))
                    new-files)))
      (setq exwmx-appmenu--apps-cache (counsel-linux-apps-parse new-desktop-alist)
            exwmx-appmenu--apps-cache-format-function exwmx-appmenu-format-function
            exwmx-appmenu--apps-cache-timestamp (current-time)
            exwmx-appmenu--apps-cached-files new-files)))
  exwmx-appmenu--apps-cache)

(defun exwmx-appmenu ()
  "Show exwmx's application menu."
  (interactive)
  (when (buffer-live-p exwmx-appmenu-buffer)
    (kill-buffer exwmx-appmenu-buffer))
  (let ((buffer (get-buffer-create exwmx-appmenu-buffer))
        (n 1)
        content)
    (with-current-buffer buffer
      (switch-to-buffer buffer)
      (dolist (x (cl-sort
                  (exwmx-appmenu--get-apps-list)
                  #'(lambda (a b)
                      (string< (car a) (car b)))))
        (let* ((desktop-shortcut (cdr x))
               (item
                (propertize
                 (concat (number-to-string n) ". " (car x))
                 'desktop-shortcut desktop-shortcut
                 'face 'font-lock-builtin-face)))
          (if content
              (setq content (concat content "\n" item))
            (setq content item))
          (setq n (1+ n))))
      (insert content)
      (goto-char (point-min))
      (exwmx-appmenu-mode))))

(defun exwmx-appmenu-launch ()
  "Call application at the current menu item."
  (interactive)
  (let* ((desktop-shortcut (get-text-property (point) 'desktop-shortcut)))
    (if desktop-shortcut
        (progn
          (call-process "gtk-launch" nil nil nil desktop-shortcut)
          (message "EXWM-X Appmenu: launch '%s' ..." desktop-shortcut))
      (message "EXWM-X Appmenu: no application is launched!"))
    (exwmx-appmenu-quit t)))

(defun exwmx-appmenu-quit (&optional silent)
  "Quit emwmx-appmenu."
  (interactive)
  (let ((buffer (get-buffer exwmx-appmenu-buffer)))
    (when (and buffer (buffer-live-p buffer))
      (kill-buffer buffer))
    (unless silent
      (message "EXWM-X Appmenu: Quit!"))))

(provide 'exwmx-appmenu)
;;; exwmx-appmenu.el ends here
