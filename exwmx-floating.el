;;; exwmx-floating.el --- Exwm-X tools for floating window

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

;; * exwmx-floating manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)

(defvar exwmx-floating--smart-hide-timer nil
  "The timer used by `exwmx-floating-smart-hide'.")

(defvar exwmx-floating--last-buffer nil
  "Record the last buffer which is used by `exwmx-floating-smart-hide'.")

(defun exwmx-floating-hide-all ()
  "Hide all floating window."
  (interactive)
  (exwmx-floating--hide-all)
  (select-frame-set-input-focus exwm-workspace--current))

(defun exwmx-floating--hide-all ()
  "Internal function of `exwmx-floating-hide-all'."
  (dolist (alist exwm--id-buffer-alist)
    (let ((buffer (cdr alist)))
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (and (eq major-mode 'exwm-mode)
                     exwm--floating-frame)
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window exwm--container
                               :value-mask xcb:ConfigWindow:StackMode
                               :stack-mode xcb:StackMode:Below))
            (exwm-layout--set-state exwm--id xcb:icccm:WM_STATE:IconicState)
            (xcb:flush exwm--connection)))))))

(defun exwmx-floating-smart-hide ()
  "Hide floating window if the current buffer is a tilling buffer
or normal buffer.

FIXME: This function use `run-with-timer', which may be not a
good approach, but I can not find other way at the moment."
  (interactive)
  (let ((repeat 0.2))
    (when exwmx-floating--smart-hide-timer
      (cancel-timer exwmx-floating--smart-hide-timer))
    (setq exwmx-floating--smart-hide-timer
          (run-with-timer
           nil repeat
           #'exwmx-floating--smart-hide))))

(defun exwmx-floating--smart-hide ()
  "Internal function of `exwmx-floating-smart-hide'."
  (let ((buffer (current-buffer)))
    ;; When buffer is not change, do nothing.
    (when (and buffer (buffer-live-p buffer))
      (unless (eq exwmx-floating--last-buffer buffer)
        (unless exwm--floating-frame
          (exwmx-floating--hide-all))))
    (setq exwmx-floating--last-buffer buffer)))

(defun exwmx-floating-mouse-move (start-event)
  "This is a mouse drag event function used by exwmx-button,
when drag mouse from such button, move current floating window dynamic."
  (interactive "e")
  (exwmx-floating--mouse-operate start-event))

(defun exwmx-floating-mouse-resize (start-event)
  "This is a mouse drag event function used by exwmx-button,
when drag mouse from such button, resize current floating window dynamic."
  (interactive "e")
  (exwmx-floating--mouse-operate start-event t))

(defun exwmx-floating--mouse-operate (start-event &optional resize)
  "Internal function of `exwmx-floating-mouse-move'
and `exwmx-floating-mouse-move'"
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

(defun exwmx-floating-toggle-floating ()
  "Toggle the current window between floating and non-floating states."
  (interactive)
  (with-current-buffer (window-buffer)
    (if exwm--floating-frame
        (progn
          (setq header-line-format nil)
          (exwm-layout--refresh)
          (exwm-floating--unset-floating exwm--id))
      (exwm-floating--set-floating exwm--id))))

;; Hack the bug: https://github.com/ch11ng/exwm/issues/248
(advice-add 'exwm-floating-toggle-floating :override #'exwmx-floating-toggle-floating)

(provide 'exwmx-floating)

;;; exwmx-floating.el ends here
