;;; exwmx-mvborder.el --- Let exwm tiling window resize easily

;; * Header
;; Copyright (c) the unknown original author

;; I found the original code on a blog where the blog author
;; is also saying that he is not the author.
;; So, no one knows who is the author.

;; If you are the author, please send me a word.

;; Author: Unknown
;; URL: https://www.emacswiki.org/emacs/WindowResize
;;      https://www.emacswiki.org/emacs/GrowShrinkWindows
;;      https://github.com/ramnes/move-border
;;
;; Keywords: window-manager, exwm

;;; Commentary:

;; * exwmx-mvborder manual                                            :doc:

;;; Code:

;; * Code                                                                 :code:
(defun exwmx-mvborder--xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun exwmx-mvborder--left-or-right (arg dir)
  "General function covering exwmx-mvborder-left and exwmx-mvborder-right.
If DIR is t, then move left, otherwise move right."
  (when (null arg)
    (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (exwmx-mvborder--xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun exwmx-mvborder--up-or-down (arg dir)
  "General function covering exwmx-mvborder-up and exwmx-mvborder-down.
If DIR is t, then move up, otherwise move down."
  (when (null arg)
    (setq arg 5))
  (let ((top-edge (nth 1 (window-edges))))
    (if (exwmx-mvborder--xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun exwmx-mvborder-left (arg)
  (interactive "P")
  (exwmx-mvborder--left-or-right arg t))

(defun exwmx-mvborder-right (arg)
  (interactive "P")
  (exwmx-mvborder--left-or-right arg nil))

(defun exwmx-mvborder-up (arg)
  (interactive "P")
  (exwmx-mvborder--up-or-down arg t))

(defun exwmx-mvborder-down (arg)
  (interactive "P")
  (exwmx-mvborder--up-or-down arg nil))

;; * Footer
(provide 'exwmx-mvborder)

;;; exwmx-mvborder.el ends here
