;;; exwm-x-modeline.el --- Simulate toolbar or dockor with emacs mode-line

;; * Header
;; Copyright 2015 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.0.1
;; Keywords: exwm, exwm-x

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

;; * README                                                               :doc:
;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'cl-lib)
(require 'exwm)
(require 'exwm-x-core)

(defvar exwm-x-mode-line-shortcuts-file
  "~/.emacs.d/exwm-x-exwm-shortcuts.el")

(defvar exwm-x--mode-line-shortcuts nil)
(defvar exwm-x--mode-line-taskbar nil)
(defvar exwm-x--mode-line-active-p nil)

(defun exwm-x--create-mode-line-button (string &optional mouse-1-action
                                               mouse-3-action mouse-2-action
                                               active-down-mouse)
  "Create clickable shortcut's code which is used by mode-line-format."
  `(:eval (propertize
           ,string
           'face 'mode-line-buffer-id
           'help-echo ""
           'mouse-face 'mode-line-highlight
           'local-map
           (let ((map (make-sparse-keymap)))
             (unless (eq (quote ,mouse-1-action) nil)
               (define-key map [mode-line mouse-1]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-1-action))))
             (unless (eq (quote ,mouse-2-action) nil)
               (define-key map [mode-line mouse-2]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-2-action))))
             (unless (eq (quote ,mouse-3-action) nil)
               (define-key map [mode-line mouse-3]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-3-action))))
             (when (and (eq major-mode 'exwm-mode)
                        exwm--floating-frame
                        (not (eq (quote ,active-down-mouse) nil)))
               (define-key map [mode-line down-mouse-1]
                 #'exwm-x-mouse-move-floating-window)
               (define-key map [mode-line down-mouse-2]
                 #'exwm-x-mouse-move-floating-window)
               (define-key map [mode-line down-mouse-3]
                 #'exwm-x-mouse-resize-floating-window))
             map))))

(defun exwm-x--create-mode-line ()
  (setq mode-line-format
        `(exwm--floating-frame
          (,(exwm-x--create-mode-line-button
             "[E]" '(exwm-x--reset-mode-line) '(start-menu-popup))
           ,(exwm-x--create-mode-line-button
             "[X]" '(exwm-x-kill-exwm-buffer) '(exwm-x-kill-exwm-buffer))
           ,(exwm-x--create-mode-line-button
             " - " nil nil nil t)
           ,(exwm-x--create-mode-line-button
             "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
           ,(exwm-x--create-mode-line-button
             "[_]" '(exwm-floating-hide) '(exwm-floating-hide))
           ,(exwm-x--create-mode-line-button
             "[Zoom]" '(exwm-x-resize-floating-window event 0.75)
             '(exwm-x-resize-floating-window event 0.5))
           " -:"
           mode-line-mule-info
           "- "
           ,(exwm-x--create-mode-line-button
             (make-string 200 ?-) nil nil nil t))
          (,(exwm-x--create-mode-line-button
             "[E]" '(exwm-x--reset-mode-line) '(start-menu-popup))
           ,(exwm-x--create-mode-line-button
             "[+]" '(delete-other-windows) '(delete-other-windows))
           ,(exwm-x--create-mode-line-button
             "[D]" '(delete-window) '(delete-window))
           ,(exwm-x--create-mode-line-button
             "[X]" '(exwm-x-kill-exwm-buffer) '(exwm-x-kill-exwm-buffer))
           " :"
           ,@(if (< (length exwm-x--mode-line-taskbar) 4)
                 `(,@exwm-x--mode-line-shortcuts
                   ,(when exwm-x--mode-line-taskbar "-")
                   ,@exwm-x--mode-line-taskbar)
               exwm-x--mode-line-taskbar)
           ": "
           ,(exwm-x--create-mode-line-button
             "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
           ,(exwm-x--create-mode-line-button
             "[-]" '(split-window-below) '(split-window-below))
           ,(exwm-x--create-mode-line-button
             "[|]" '(split-window-right) '(split-window-right))
           " -:"
           mode-line-mule-info
           "- "
           ,(exwm-x--create-mode-line-button
             (make-string 200 ?-) nil nil nil t))))
  (setq exwm-x--mode-line-active-p t)
  (force-mode-line-update))

(setq-default mode-line-format
              `(,(exwm-x--create-mode-line-button
                  "[E]" '(exwm-x--create-mode-line) '(start-menu-popup))
                ,(default-value 'mode-line-format)))

(defun exwm-x--reset-mode-line ()
  (setq mode-line-format (default-value 'mode-line-format))
  (setq exwm-x--mode-line-active-p nil)
  (force-mode-line-update))

(defun exwm-x--update-mode-line ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (eq major-mode 'exwm-mode)
          (exwm-x--create-mode-line)
        (exwm-x--reset-mode-line)))))

(defun exwm-x--update-taskbar (&optional ignore-current-task)
  (setq exwm-x--mode-line-taskbar
        (exwm-x--create-taskbar
         (if ignore-current-task
             (remove (current-buffer)
                     (buffer-list))
           (buffer-list))))
  (exwm-x--update-mode-line))

(defun exwm-x--create-taskbar (buffer-list)
  (let (buffers taskbar-buttons)
    (setq buffers (sort buffer-list
                        #'(lambda (x y)
                            (string< (buffer-name y)
                                     (buffer-name x)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (and (equal major-mode 'exwm-mode)
                   (or exwm-class-name
                       exwm-instance-name
                       exwm-title))
          (push (exwm-x--create-mode-line-button
                 (concat "[" (exwm-x--return-new-name) "]")
                 `(progn (exwm-workspace-switch-to-buffer ,buffer)
                         (exwm-x--update-taskbar))
                 `(exwm-x-kill-exwm-buffer ,buffer))
                taskbar-buttons))))
    taskbar-buttons))


(defun exwm-x--clean-all-shortcuts ()
  (interactive)
  (setq exwm-x--mode-line-shortcuts nil)
  (exwm-x--save-all-shortcuts)
  (exwm-x--update-mode-line))

(defun exwm-x--save-all-shortcuts ()
  (interactive)
  (message "Save exwm-x shortcuts to \"%s\"" exwm-x-mode-line-shortcuts-file)
  (unless (file-directory-p
           (file-name-directory exwm-x-mode-line-shortcuts-file))
    (make-directory (file-name-directory exwm-x-mode-line-shortcuts-file) t))
  (with-temp-buffer
    (erase-buffer)
    (cl-prettyprint exwm-x--mode-line-shortcuts)
    (write-file exwm-x-mode-line-shortcuts-file)))

(defun exwm-x--load-saved-shortcuts ()
  (interactive)
  (message "Load exwm-x shortcuts from \"%s\"" exwm-x-mode-line-shortcuts-file)
  (with-temp-buffer
    (erase-buffer)
    (insert-file-contents exwm-x-mode-line-shortcuts-file)
    (setq exwm-x--mode-line-shortcuts
          (read (current-buffer)))))

(defun exwm-x--delete-shortcut (button-name)
  (setq exwm-x--mode-line-shortcuts
        (cl-remove-if
         #'(lambda (x)
             (equal button-name (nth 1 (cadr x))))
         exwm-x--mode-line-shortcuts))
  (exwm-x--save-all-shortcuts)
  (exwm-x--update-mode-line))

(add-hook 'kill-emacs-hook #'exwm-x--save-all-shortcuts)
(add-hook 'emacs-startup-hook #'exwm-x--load-saved-shortcuts)
(add-hook 'exwm-manage-finish-hook #'exwm-x--update-taskbar)
(add-hook 'exwm-update-class-hook #'exwm-x--update-taskbar)
(add-hook 'exwm-update-title-hook #'exwm-x--update-taskbar)
(add-hook 'kill-buffer-hook #'(lambda () (exwm-x--update-taskbar t)))
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-modeline)

;;; exwm-x-modeline.el ends here
;; #+END_SRC
