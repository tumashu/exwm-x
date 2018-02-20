;;; exwmx-floating.el --- EXWM-X tools for floating window

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

(defvar exwmx-floating-default-size-and-position '(0.8 0.8 center 0.05)
  "Floating window's default size and position.")

(defvar-local exwmx-floating--first-floating t) ;First floating a window

(defun exwmx-floating-hide-all ()
  "Hide all floating window."
  (interactive)
  (dolist (alist exwm--id-buffer-alist)
    (let ((buffer (cdr alist)))
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (exwm-floating-hide))))))

(defun exwmx-floating--smart-hide (window)
  "Advice function of `exwm-input--update-focus', hide floating
window if the current buffer is a tilling buffer or normal buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (unless exwm--floating-frame
        (exwmx-floating-hide-all)))))

(defun exwmx-floating-smart-hide (&optional disable)
  "Hide floating window if the current buffer is a tilling buffer
or normal buffer, when `disable' non-nil, disable smart hide.

FIXME: This is a hack, it should be improved in the future."
  (if disable
      (advice-remove 'exwm-input--update-focus
                     #'exwmx-floating--smart-hide)
    (advice-add 'exwm-input--update-focus
                :before #'exwmx-floating--smart-hide)))

(defun exwmx-floating--move-to-position (x y)
  "Move current floating window to position: `x', `y'."
  (when (and x y (>= x 0) (>= y 0)
             (eq major-mode 'exwm-mode)
             exwm--floating-frame)
    (let* ((edges (window-inside-absolute-pixel-edges))
           (floating-container (frame-parameter exwm--floating-frame
                                                'exwm-container)))
      (exwm--set-geometry floating-container x y nil nil)
      (exwm--set-geometry exwm--id (pop edges) (pop edges) nil nil))
    (xcb:flush exwm--connection)))

(defun exwmx-floating-adjust-window (width height &optional x y)
  "Set current floating window's size, when `width' < 1, set the window's
width to width * screen width, when `height' < 1, set the window's height
to height * screen height."
  (when (and (> width 0) (> height 0)
             (eq major-mode 'exwm-mode)
             exwm--floating-frame)
    (let* ((screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           (orig-width (frame-pixel-width))
           (orig-height (frame-pixel-height))
           (width (if (< width 1)
                      (round (* screen-width width))
                    width))
           (height (if (< height 1)
                       (round (* screen-height height))
                     height))
           (x (cond ((and (numberp x) (>= x 1))
                     x)
                    ((and (numberp x) (< x 1))
                     (round (* screen-width x)))
                    ((eq x 'center)
                     (round (/ (- screen-width width) 2)))))
           (y (cond ((and (numberp y) (>= y 1))
                     y)
                    ((and (numberp y) (< y 1))
                     (round (* screen-height y)))
                    ((eq y 'center)
                     (round (/ (- screen-height height) 2))))))
      (exwm-layout-enlarge-window (- width orig-width) t)
      (exwm-layout-enlarge-window (- height orig-height))
      (exwmx-floating--move-to-position x y))))

(defun exwmx-floating-mouse-move (start-event)
  "This is a mouse drag event function used by exwmx-button,
when drag mouse from such button, move current floating window dynamic."
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
                   (exwm-floating-move
                    (* char-width (- x orig-x))
                    (* char-width (- y orig-y))))))))))

(defun exwmx-floating-toggle-floating ()
  "Toggle the current window between floating and non-floating states."
  (interactive)
  (let ((exwm-floating-setup-hook
         (if exwm-floating-setup-hook
             `(,exwm-floating-setup-hook
               #'exwmx-floating-first-floating)
           #'exwmx-floating-first-floating)))
    (call-interactively #'exwm-floating-toggle-floating)))

(defun exwmx-floating-first-floating ()
  "The function handle `exwmx-floating--first-floating'
It is used as `exwm-floating-setup-hook'."
  (if exwmx-floating--first-floating
      (let ((size-and-position
             (plist-get (exwmx-appconfig--search
                         `((:class ,exwm-class-name)
                           (:instance ,exwm-instance-name)))
                        :size-and-position)))
        (apply #'exwmx-floating-adjust-window
               (if (= (length size-and-position) 4)
                   size-and-position
                 exwmx-floating-default-size-and-position)))
    (setq exwmx-floating--first-floating nil)))

(provide 'exwmx-floating)

;;; exwmx-floating.el ends here
