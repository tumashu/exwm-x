;;; exwmx-move-border.el --- Let exwm tiling window resize easily

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

;; * exwmx-move-border manual                                            :doc:

;;; Code:

;; * Code                                                                 :code:
(defun exwmx--xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun exwmx--move-border-left-or-right (arg dir)
  "General function covering exwmx-move-border-left and exwmx-move-border-right.
If DIR is t, then move left, otherwise move right."
  (when (null arg)
    (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (exwmx--xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun exwmx--move-border-up-or-down (arg dir)
  "General function covering exwmx-move-border-up and exwmx-move-border-down.
If DIR is t, then move up, otherwise move down."
  (when (null arg)
    (setq arg 5))
  (let ((top-edge (nth 1 (window-edges))))
    (if (exwmx--xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun exwmx-move-border-left (arg)
  (interactive "P")
  (exwmx--move-border-left-or-right arg t))

(defun exwmx-move-border-right (arg)
  (interactive "P")
  (exwmx--move-border-left-or-right arg nil))

(defun exwmx-move-border-up (arg)
  (interactive "P")
  (exwmx--move-border-up-or-down arg t))

(defun exwmx-move-border-down (arg)
  (interactive "P")
  (exwmx--move-border-up-or-down arg nil))

;; * Footer
(provide 'exwmx-move-border)

;;; exwmx-move-border.el ends here
