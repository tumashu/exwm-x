;;; exwm-x-buttons.el --- Add some exwm buttons to mode-line or header-line

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
(require 'cl-lib)
(require 'exwm)
(require 'exwm-x-core)

(defun exwm-x--create-button (mode-line-or-header-line string &optional mouse-1-action
                                                       mouse-3-action mouse-2-action
                                                       active-down-mouse)
  "Generate `,mode-or-head-line-format' style code of a clickable button, which name
is `string'.

`mouse-1-action', `mouse-2-action' and `mouse-2-action' are often quoted lists.
when click it with mouse-1, `mouse-1-action' will be execute.
click mouse-2, `mouse-2-action' execute. click mouse-3, `mouse-3-action'
execute. "
  `(:eval (propertize
           ,string
           'face 'mode-line-buffer-id
           'help-echo ""
           'mouse-face 'mode-line-highlight
           'local-map
           (let ((map (make-sparse-keymap)))
             (unless (eq (quote ,mouse-1-action) nil)
               (define-key map [,mode-line-or-header-line mouse-1]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-1-action))))
             (unless (eq (quote ,mouse-2-action) nil)
               (define-key map [,mode-line-or-header-line mouse-2]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-2-action))))
             (unless (eq (quote ,mouse-3-action) nil)
               (define-key map [,mode-line-or-header-line mouse-3]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-3-action))))
             (when (and (eq major-mode 'exwm-mode)
                        exwm--floating-frame
                        (not (eq (quote ,active-down-mouse) nil)))
               (define-key map [,mode-line-or-header-line down-mouse-1]
                 #'exwm-x-mouse-move-floating-window)
               (define-key map [,mode-line-or-header-line down-mouse-3]
                 #'exwm-x-mouse-move-floating-window))
             map))))

;; Emacs's default mode-line is not suitable for window manager,
;; This function will create a new mode-line, which has start-menu,
;; application shortcuts ,taskbar and others useful for managing window.

;; 1. Tilling window's mode-line is like:

;;    '[E][+][D][X][F][-][|]'

;; [E]: Switch mode-line.
;; [+]: Maximize current window.
;; [D]: Delete current window.
;; [F]: toggle floating window.
;; [-]: Split window horizontal.
;; [|]: Split window vertical.
;; [<>]: Move border to left or right.

(defun exwm-x--create-mode-line ()
  (setq mode-line-format
        (list (exwm-x--create-button
               'mode-line "[X]" '(exwm-x-kill-exwm-buffer) '(exwm-x-kill-exwm-buffer))
              (exwm-x--create-button
               'mode-line "[D]" '(delete-window) '(delete-window))
              (exwm-x--create-button
               'mode-line "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
              " "
              (exwm-x--create-button
               'mode-line "[<]" '(exwm-x-move-border-left 10) '(exwm-x-move-border-left 10))
              (exwm-x--create-button
               'mode-line "[+]" '(delete-other-windows) '(delete-other-windows))
              (exwm-x--create-button
               'mode-line "[>]" '(exwm-x-move-border-right 10) '(exwm-x-move-border-right 10))
              " "
              (exwm-x--create-button
               'mode-line "[-]" '(split-window-below) '(split-window-below))
              (exwm-x--create-button
               'mode-line "[|]" '(split-window-right) '(split-window-right))
              " - "
              exwm-title)))

(defun exwm-x--create-header-line ()
  (setq header-line-format
        (list (exwm-x--create-button
               'header-line "[X]" '(exwm-x-kill-exwm-buffer) '(exwm-x-kill-exwm-buffer))
              (exwm-x--create-button
               'header-line "[_]" '(exwm-floating-hide) '(exwm-floating-hide))
              (exwm-x--create-button
               'header-line "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
              " "
              (exwm-x--create-button
               'header-line "[Z+]"
               '(progn (exwm-layout-enlarge-window 30)
                       (exwm-layout-enlarge-window-horizontally 100))
               '(progn (exwm-layout-enlarge-window 30)
                       (exwm-layout-enlarge-window-horizontally 100)))
              (exwm-x--create-button
               'header-line "[Z-]"
               '(progn (exwm-layout-enlarge-window -30)
                       (exwm-layout-enlarge-window-horizontally -100))
               '(progn (exwm-layout-enlarge-window -30)
                       (exwm-layout-enlarge-window-horizontally -100)))
              (exwm-x--create-button
               'header-line
               (concat " - " exwm-title (make-string 200 ? )) nil nil nil t))))

(defun exwm-x--reset-mode-line ()
  "Reset mode-line."
  (setq mode-line-format
        (default-value 'mode-line-format)))

(defun exwm-x--reset-header-line ()
  "Reset header-line."
  (setq header-line-format
        (default-value 'header-line-format)))

(defun exwm-x--update-mode-or-header-line ()
  "Update all buffer's mode-lines and header-lines."
  (interactive)
  ;; Set all buffer's mode-line.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((and (eq major-mode 'exwm-mode)
                  (not exwm--floating-frame))
             (exwm-x--create-mode-line)
             (exwm-x--reset-header-line))
            ((and (eq major-mode 'exwm-mode)
                  exwm--floating-frame)
             (exwm-x--create-header-line)
             (setq mode-line-format nil))
            (t (exwm-x--reset-mode-line)
               (exwm-x--reset-header-line))))
    (force-mode-line-update)))

(add-hook 'exwm-update-class-hook #'exwm-x--update-mode-or-header-line)
(add-hook 'exwm-update-title-hook #'exwm-x--update-mode-or-header-line)
(add-hook 'buffer-list-update-hook #'exwm-x--update-mode-or-header-line)

;; * Footer

(provide 'exwm-x-buttons)

;;; exwm-x-modeline.el ends here
