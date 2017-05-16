;;; exwmx-utils.el --- Some useful exwmx commands

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.3
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

;; * exwmx-utils manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)
(require 'exwmx-core)

(defun exwmx-jump-or-exec (command &optional class-instance-or-title current-window)
  "if matched window can be found, switch to this window,
otherwise run shell command `command'."
  (unless current-window
    (exwmx--switch-window))
  (let ((buffer (or (exwmx--find-buffer class-instance-or-title)
                    (exwmx--find-buffer
                     (exwmx--search-apps-db command :regexp :jump-or-exec t))
                    ;; The below two rules are just guess rules :-)
                    ;; Suggest use variable `exwmx-jump-or-exec'
                    ;; to set you own rules.
                    (exwmx--find-buffer
                     (capitalize (concat "^" (car (split-string command " ")))))
                    (exwmx--find-buffer
                     (concat "^" (car (split-string command " ")))))))
    (message "Exwm-X jump-or-exec: %s" command)
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command command nil command))))

(defun exwmx--find-buffer (regexp)
  "Find such a exwm buffer: its local variables: `exwm-class-name',
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

(defun exwmx-kill-exwm-buffer (&optional buffer-or-name)
  "Kill buffer, if current buffer is a exwm buffer."
  (let ((buffer (or buffer-or-name
                    (current-buffer))))
    (with-current-buffer buffer
      (if (eq major-mode 'exwm-mode)
          (progn (kill-buffer buffer)
                 (exwmx--next-exwm-buffer))
        (message "This buffer is not a exwm buffer!")))))

(defun exwmx-shell-command (cmd)
  "Run shell command `cmd'."
  (start-process-shell-command cmd nil cmd))

(defun exwmx-shell-command-interactively (cmd)
  "Run shell command `cmd' interactively."
  (interactive
   (list (read-shell-command "Run shell command: ")))
  (start-process-shell-command cmd nil cmd))

(defun exwmx-mouse-move-floating-window (start-event)
  "This is a mouse drag event function, used by exwm
mode-line button, when drag mouse from such button,
move current floating window dynamic."
  (interactive "e")
  (exwmx--mouse-operate-floating-window start-event))

(defun exwmx-mouse-resize-floating-window (start-event)
  "This is a mouse drag event function, used by exwm
mode-line button, when drag mouse from such button,
resize current floating window dynamic."
  (interactive "e")
  (exwmx--mouse-operate-floating-window start-event t))

(defun exwmx--mouse-operate-floating-window (start-event &optional resize)
  "Internal function of `exwmx-mouse-move-floating-window'
and `exwmx-mouse-move-floating-window'"
  (interactive "e")
  (when exwm--floating-frame
    (let* ((orig-mouse (mouse-position))
           (orig-x (car (cdr orig-mouse)))
           (orig-y (cdr (cdr orig-mouse)))
           (frame (window-frame (car (car (cdr start-event)))))
           (frame-width (frame-width frame))
           (frame-height (frame-height frame))
           (char-width (frame-char-width frame))
           (char-height (frame-char-height frame))
           (echo-keystrokes 0)
           (done nil)
           (last-x orig-x)
           (last-y orig-y)
           event mouse x y)
      (track-mouse
        (while (not done)
          (setq event (read-event)
                mouse (mouse-position))
          ;; do nothing if
          ;;   - there is a switch-frame event.
          ;;   - the mouse isn't in the frame that we started in
          ;;   - the mouse isn't in any Emacs frame
          ;; drag if
          ;;   - there is a mouse-movement event
          ;;   - there is a scroll-bar-movement event
          ;;     (same as mouse movement for our purposes)
          ;; quit if
          ;;   - there is a keyboard event or some other unknown event
          ;;     unknown event.
          (cond ((integerp event)
                 (setq done t))
                ((eq (car event) 'switch-frame)
                 nil)
                ((not (memq (car event)
                            '(mouse-movement scroll-bar-movement)))
                 (setq done t))
                ((not (eq (car mouse) frame))
                 nil)
                ((null (car (cdr mouse)))
                 nil)
                (t (setq x (car (cdr mouse))
                         y (cdr (cdr mouse)))
                   (if resize
                       (set-frame-size
                        frame
                        (- frame-width (- orig-x x))
                        (- frame-height (- orig-y y)))
                     (exwm-floating-move
                      (* char-width (- x orig-x))
                      (* char-width (- y orig-y)))))))))))

(provide 'exwmx-utils)

;;; exwmx-utils.el ends here
