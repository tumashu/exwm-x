;;; exwm-x-utils.el --- Some useful exwm-x commands

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.0.1
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

;; * exwm-x-utils manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'exwm)
(require 'exwm-x-core)

(defun exwm-x-jump-or-exec (regexp cmd &optional current-window)
  "Jump to a window which class, instance or title matched `regexp',
if matched window can't be found, run shell command `cmd'."
  (when (and (not current-window)
             (featurep 'switch-window))
    (switch-window--then
     "Move to window: "
     #'(lambda () (other-window 1))
     nil nil 1))

  (let ((buffer (exwm-x--find-buffer regexp)))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command cmd nil cmd))))

(defun exwm-x--find-buffer (regexp)
  "Find such a exwm buffer: its local variables: `exwm-class-name', `exwm-instance-name'
or `exwm-title' is matched `regexp'."
  (let* ((buffers (buffer-list))
         (buffers-list (list nil nil nil)))

    (dolist (buffer buffers)
      (let ((wininfo `((0 . ,(buffer-local-value 'exwm-title buffer))
                       (1 . ,(buffer-local-value 'exwm-instance-name buffer))
                       (2 . ,(buffer-local-value 'exwm-class-name buffer)))))
        (dolist (x wininfo)
          (when (exwm-x--string-match-p regexp (cdr x))
            (setf (nth (car x) buffers-list)
                  (append (list buffer) (nth (car x) buffers-list)))))))

    (caar (delq nil
                (sort buffers-list
                      #'(lambda (a b)
                          (< (length a) (length b))))))))

(defun exwm-x-kill-exwm-buffer (&optional buffer-or-name)
  "Kill buffer, if current buffer is a exwm buffer."
  (let ((buffer (or buffer-or-name
                    (current-buffer))))
    (with-current-buffer buffer
      (if (eq major-mode 'exwm-mode)
          (progn (kill-buffer buffer)
                 (exwm-x--next-exwm-buffer))
        (message "This buffer is not a exwm buffer!")))))

(defun exwm-x-run-shell-command (cmd)
  "Run shell command `cmd'."
  (start-process-shell-command cmd nil cmd))

(defun exwm-x-run-shell-command-interactively (cmd)
  "Run shell command `cmd' interactively."
  (interactive
   (list (read-shell-command "Run shell command: ")))
  (start-process-shell-command cmd nil cmd))

(defun exwm-x-resize-floating-window (event &optional scale)
  "This is a mouse click event function, used by exwm mode-line
button, when click such button, resize current floating window to `scale'
property of screen size."
  (let* ((frame (window-frame (car (car (cdr event)))))
         (screen-width (display-pixel-width))
         (screen-height (display-pixel-height)))
    (set-frame-size
     frame
     (round (* scale screen-width))
     (round (* scale screen-height)) t)))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-utils)

;;; exwm-x-utils.el ends here
;; #+END_SRC
