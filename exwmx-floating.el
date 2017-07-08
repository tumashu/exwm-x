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
  (when (and (>= x 0) (>= y 0)
             (eq major-mode 'exwm-mode)
             exwm--floating-frame)
    (let ((edges (window-inside-absolute-pixel-edges)))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window exwm--container
                         :value-mask (eval-when-compile
                                       (logior xcb:ConfigWindow:X
                                               xcb:ConfigWindow:Y))
                         :x x
                         :y y))
      ;; Inform the X window that its absolute position is changed
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0 :destination exwm--id
                         :event-mask xcb:EventMask:StructureNotify
                         :event (xcb:marshal
                                 (make-instance 'xcb:ConfigureNotify
                                                :event exwm--id
                                                :window exwm--id
                                                :above-sibling xcb:Window:None
                                                :width (- (elt edges 2)
                                                          (elt edges 0))
                                                :height (- (elt edges 3)
                                                           (elt edges 1))
                                                :border-width 0
                                                :override-redirect 0
                                                :x x
                                                :y y)
                                 exwm--connection))))
    (xcb:flush exwm--connection)))

(defun exwmx-floating-adjust-window (width height &optional x-pos y-pos)
  "Set current floating window's size, when `width' < 1, set the window's
width to width * screen width, when `height' < 1, set the window's height
to height * screen height."
  (when (and (> width 0) (> height 0)
             (eq major-mode 'exwm-mode)
             exwm--floating-frame)
    (let ((screen-width (display-pixel-width))
          (screen-height (display-pixel-height)))
      (when (< width 1)
        (setq width (round (* screen-width width))))
      (when (< height 1)
        (setq height (round (* screen-height height))))
      ;; Set width
      (setf (slot-value exwm--geometry 'width) width)
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id)
                         :value-mask xcb:ConfigWindow:Width
                         :width width))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-container)
                         :value-mask xcb:ConfigWindow:Width
                         :width width))
      ;; Set height
      (setf (slot-value exwm--geometry 'height) height)
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id)
                         :value-mask xcb:ConfigWindow:Height
                         :height height))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-container)
                         :value-mask xcb:ConfigWindow:Height
                         :height height))
      (xcb:flush exwm--connection)
      ;; Relocate the floating window.
      (let (x y)
        (cond ((and (numberp x-pos) (>= x-pos 1))
               (setq x x-pos))
              ((and (numberp x-pos) (< x-pos 1))
               (setq x (round (* screen-width x-pos))))
              ((eq x-pos 'center)
               (setq x (round (/ (- screen-width width) 2)))))
        (cond ((and (numberp y-pos) (>= y-pos 1))
               (setq y y-pos))
              ((and (numberp y-pos) (< y-pos 1))
               (setq y (round (* screen-height y-pos))))
              ((eq y-pos 'center)
               (setq y (round (/ (- screen-height height) 2)))))
        (when (and x y)
          (exwmx-floating--move-to-position x y))))))

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
      (exwm-floating--set-floating exwm--id)
      (when exwmx-floating--first-floating
        (let ((size-and-position
               (plist-get (exwmx-appconfig--search
                           `((:class ,exwm-class-name)
                             (:instance ,exwm-instance-name))
                           '(:size-and-position))
                          :size-and-position)))
          (apply #'exwmx-floating-adjust-window
                 (if (= (length size-and-position) 4)
                     size-and-position
                   exwmx-floating-default-size-and-position))))
      (setq exwmx-floating--first-floating nil))))

(provide 'exwmx-floating)

;;; exwmx-floating.el ends here
