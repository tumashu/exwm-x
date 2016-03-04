;;; exwm-x-move-border.el --- Let exwm tiling window resize easily

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

;; * exwm-x-move-border manual                                            :doc:

;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(defun exwm-x--xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun exwm-x--move-border-left-or-right (arg dir)
  "General function covering exwm-x-move-border-left and exwm-x-move-border-right.
If DIR is t, then move left, otherwise move right."
  (when (null arg)
    (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (exwm-x--xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun exwm-x--move-border-up-or-down (arg dir)
  "General function covering exwm-x-move-border-up and exwm-x-move-border-down.
If DIR is t, then move up, otherwise move down."
  (when (null arg)
    (setq arg 5))
  (let ((top-edge (nth 1 (window-edges))))
    (if (exwm-x--xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun exwm-x-move-border-left (arg)
  (interactive "P")
  (exwm-x--move-border-left-or-right arg t))

(defun exwm-x-move-border-right (arg)
  (interactive "P")
  (exwm-x--move-border-left-or-right arg nil))

(defun exwm-x-move-border-up (arg)
  (interactive "P")
  (exwm-x--move-border-up-or-down arg t))

(defun exwm-x-move-border-down (arg)
  (interactive "P")
  (exwm-x--move-border-up-or-down arg nil))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-move-border)

;;; exwm-x-move-border.el ends here
;; #+END_SRC
