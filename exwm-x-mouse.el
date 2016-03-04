;;; exwm-x-mouse.el --- Mouse functions used by exwm-x

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

;; * exwm-x-mouse manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'exwm)
(require 'exwm-x-core)

(defun exwm-x-resize-floating-window (event &optional scale)
  (let* ((frame (window-frame (car (car (cdr event)))))
         (screen-width (display-pixel-width))
         (screen-height (display-pixel-height)))
    (set-frame-size
     frame
     (round (* scale screen-width))
     (round (* scale screen-height)) t)))

(defun exwm-x-mouse-move-floating-window (start-event)
  (interactive "e")
  (exwm-x--mouse-operate-floating-window start-event))

(defun exwm-x-mouse-resize-floating-window (start-event)
  (interactive "e")
  (exwm-x--mouse-operate-floating-window start-event t))

(defun exwm-x--mouse-operate-floating-window (start-event &optional resize)
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
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-mouse)

;;; exwm-x-mouse.el ends here
;; #+END_SRC
