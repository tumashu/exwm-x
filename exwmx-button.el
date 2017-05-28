;;; exwmx-button.el --- Add some exwm buttons to mode-line

;; * Header
;; Copyright 2015-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.8.1
;; Keywords: exwm, exwmx

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
(require 'exwmx-core)
(require 'switch-window)

(defun exwmx-button--create-button (mode-line string &optional mouse-1-action
                                              mouse-3-action mouse-2-action
                                              active-down-mouse)
  "Generate `,mode-line-format' style code of a clickable button, which name
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
               (define-key map [,mode-line mouse-1]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-1-action))))
             (unless (eq (quote ,mouse-2-action) nil)
               (define-key map [,mode-line mouse-2]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-2-action))))
             (unless (eq (quote ,mouse-3-action) nil)
               (define-key map [,mode-line mouse-3]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-3-action))))
             (when (and (eq major-mode 'exwm-mode)
                        exwm--floating-frame
                        (not (eq (quote ,active-down-mouse) nil)))
               (define-key map [,mode-line down-mouse-1]
                 #'exwmx-mouse-move-floating-window)
               (define-key map [,mode-line down-mouse-3]
                 #'exwmx-mouse-move-floating-window))
             map))))

;; Emacs's default mode-line is not suitable for window manager,
;; This function will create a new mode-line, which has:

;; [X]: Delete current application.
;; [D]: Delete current window.
;; [R]: Run exwm-reset.
;; [F]: toggle floating window.
;; [<]: Move border to left.
;; [+]: Maximize current window.
;; [>]: Move border to right.
;; [-]: Split window horizontal.
;; [|]: Split window vertical.
;; [_]: minumize floating application
;; [Z+]: Zoom+ floating application's window
;; [Z-]: Zoom- floating application's window

(defun exwmx-button--create-tilling-mode-line ()
  "Create tilling window operate buttons."
  (setq mode-line-format
        (list (exwmx-button--create-button
               'mode-line "[X]" '(exwmx-kill-exwm-buffer) '(exwmx-kill-exwm-buffer))
              (exwmx-button--create-button
               'mode-line "[D]" '(delete-window) '(delete-window))
              (exwmx-button--create-button
               'mode-line "[R]" '(exwm-reset) '(exwm-reset))
              (exwmx-button--create-button
               'mode-line "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
              " "
              (exwmx-button--create-button
               'mode-line "[<]" '(switch-window-mvborder-left 10) '(switch-window-mvborder-left 10))
              (exwmx-button--create-button
               'mode-line "[+]" '(delete-other-windows) '(delete-other-windows))
              (exwmx-button--create-button
               'mode-line "[>]" '(switch-window-mvborder-right 10) '(switch-window-mvborder-right 10))
              " "
              (exwmx-button--create-button
               'mode-line "[-]" '(split-window-below) '(split-window-below))
              (exwmx-button--create-button
               'mode-line "[|]" '(split-window-right) '(split-window-right))
              " "
              (exwmx-button--create-line-char-button (exwm--buffer->id (window-buffer)))
              " - "
              exwm-title)))

(defun exwmx-button--create-floating-mode-line ()
  "Create floating window operate buttons."
  (setq mode-line-format
        (list (exwmx-button--create-button
               'mode-line "[X]" '(exwmx-kill-exwm-buffer) '(exwmx-kill-exwm-buffer))
              (exwmx-button--create-button
               'mode-line "[_]" '(exwm-floating-hide) '(exwm-floating-hide))
              (exwmx-button--create-button
               'mode-line "[R]" '(exwm-reset) '(exwm-reset))
              (exwmx-button--create-button
               'mode-line "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
              " "
              (exwmx-button--create-button
               'mode-line "[Z+]"
               '(progn (exwm-layout-enlarge-window 30)
                       (exwm-layout-enlarge-window-horizontally 60))
               '(progn (exwm-layout-enlarge-window 150)
                       (exwm-layout-enlarge-window-horizontally 300)))
              (exwmx-button--create-button
               'mode-line "[Z-]"
               '(progn (exwm-layout-enlarge-window -30)
                       (exwm-layout-enlarge-window-horizontally -60))
               '(progn (exwm-layout-enlarge-window -150)
                       (exwm-layout-enlarge-window-horizontally -300)))
              " "
              (exwmx-button--create-line-char-button (exwm--buffer->id (window-buffer)))
              (exwmx-button--create-button
               'mode-line
               (concat " - " exwm-title (make-string 200 ? )) nil nil nil t))))

(defun exwmx-button--create-line-char-button (id)
  (or (when id (exwmx-button--create-line-char-button-1 id)) ""))

(defun exwmx-button--create-line-char-button-1 (id)
  "Create Char-mode/Line-mode toggle button."
  (let (help-echo cmd mode)
    (cl-case exwm--on-KeyPress
      ((exwm-input--on-KeyPress-line-mode)
       (setq mode (substitute-command-keys
                   "[Line `\\[exwmx-toggle-keyboard]']")
             help-echo "mouse-1: Switch to char-mode"
             cmd `(lambda ()
                    (interactive)
                    (exwm-input-release-keyboard ,id))))
      ((exwm-input--on-KeyPress-char-mode)
       (setq mode (substitute-command-keys
                   "[Char `\\[exwmx-toggle-keyboard]']")
             help-echo "mouse-1: Switch to line-mode"
             cmd `(lambda ()
                    (interactive)
                    (exwm-input-grab-keyboard ,id)))))
    (with-current-buffer (exwm--id->buffer id)
      `(""
        (:propertize ,mode
                     face mode-line-buffer-id
                     help-echo ,help-echo
                     mouse-face mode-line-highlight
                     local-map
                     (keymap (mode-line keymap
                                        (down-mouse-1 . ,cmd))))))))

(defun exwmx-button--reset-mode-line ()
  "Reset `mode-line-format' to default value."
  (setq mode-line-format
        (default-value 'mode-line-format)))

(defun exwmx-button--update-mode-line ()
  "Update all buffer's mode-lines."
  (interactive)
  ;; Set all buffer's mode-line.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((and (eq major-mode 'exwm-mode)
                  (not exwm--floating-frame))
             (exwmx-button--create-tilling-mode-line))
            ((and (eq major-mode 'exwm-mode)
                  exwm--floating-frame)
             (exwmx-button--create-floating-mode-line))
            (t (exwmx-button--reset-mode-line))))
    (force-mode-line-update)))

(add-hook 'exwm-update-class-hook #'exwmx-button--update-mode-line)
(add-hook 'exwm-update-title-hook #'exwmx-button--update-mode-line)
(add-hook 'buffer-list-update-hook #'exwmx-button--update-mode-line)

;; * Footer

(provide 'exwmx-button)

;;; exwmx-modeline.el ends here
