;;; exwm-x-modeline.el --- Add some exwm buttons to mode-line

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

(defvar exwm-x--mode-line-active-p nil)

(defun exwm-x--create-mode-line-button (string &optional mouse-1-action
                                               mouse-3-action mouse-2-action
                                               active-down-mouse)
  "Generate `mode-line-format' style code of a clickable button, which name
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
             map))))

(defun exwm-x--create-mode-line ()
  "Emacs's default mode-line is not suitable for window manager,
This function will create a new mode-line, which has start-menu,
application shortcuts ,taskbar and others useful for managing window.

1. Tilling window's mode-line is like:

   '[E][+][D][X][F][-][|] -:-: -------'

2. Floating window's mode-line is like:

   '[E][X] - [F][-][|] -:-: -------'

NOTE:

[E]: Switch mode-line.
[+]: Maximize current window.
[D]: Delete current window.
[F]: toggle floating window.
[-]: Split window horizontal.
[|]: Split window vertical.
[<>]: Move border to left or right."
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
              "[_]" '(exwm-floating-hide) '(exwm-floating-hide)))
           (,(exwm-x--create-mode-line-button
              "[E]" '(exwm-x--reset-mode-line) '(start-menu-popup))
            ,(exwm-x--create-mode-line-button
              "[+]" '(delete-other-windows) '(delete-other-windows))
            ,(exwm-x--create-mode-line-button
              "[<]" '(exwm-x-move-border-left 10) '(exwm-x-move-border-left 10))
            ,(exwm-x--create-mode-line-button
              "[>]" '(exwm-x-move-border-right 10) '(exwm-x-move-border-right 10))
            ,(exwm-x--create-mode-line-button
              "[D]" '(delete-window) '(delete-window))
            ,(exwm-x--create-mode-line-button
              "[X]" '(exwm-x-kill-exwm-buffer) '(exwm-x-kill-exwm-buffer))
            ,(exwm-x--create-mode-line-button
              "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
            ,(exwm-x--create-mode-line-button
              "[-]" '(split-window-below) '(split-window-below))
            ,(exwm-x--create-mode-line-button
              "[|]" '(split-window-right) '(split-window-right)))))
   (setq exwm-x--mode-line-active-p t)
   (force-mode-line-update))

(defun exwm-x--reset-mode-line ()
  "Reset mode-line to original emacs mode-line with [E] button."
  (setq mode-line-format
        `(,(exwm-x--create-mode-line-button
            "[E]" '(exwm-x--create-mode-line) '(start-menu-popup))
          ,(default-value 'mode-line-format)))
  (setq exwm-x--mode-line-active-p nil)
  (force-mode-line-update))

(defun exwm-x--update-mode-line ()
  "Update all buffer's mode-lines, all exwm buffer will use
shortcut-taskbar-style mode-line."
  (interactive)
  ;; Set all buffer's mode-line.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (eq major-mode 'exwm-mode)
          (exwm-x--create-mode-line)
        (exwm-x--reset-mode-line)))))

(add-hook 'exwm-update-class-hook #'exwm-x--update-mode-line)
(add-hook 'exwm-update-title-hook #'exwm-x--update-mode-line)
(add-hook 'buffer-list-update-hook #'exwm-x--update-mode-line)
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-modeline)

;;; exwm-x-modeline.el ends here
;; #+END_SRC
