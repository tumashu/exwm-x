;;; exwmx-commit.el --- Exwm-X commit mode

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

;; * exwmx-commit manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)
(require 'exwmx-core)

(defvar exwmx-commit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'exwmx-commit-finish)
    (define-key keymap "\C-c\C-q" 'exwmx-commit-ignore)
    keymap)
  "Keymap for `exwmx-commit-mode'")

(define-minor-mode exwmx-commit-mode
  "Minor for exwmx-commit."
  nil " exwmx-commit" exwmx-commit-mode-map)

(defvar exwmx-commit-buffer "*Exwm-X-Edit*")

(defun exwmx-commit ()
  (interactive)
  (let ((buffer (get-buffer-create exwmx-commit-buffer)))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (exwmx-commit-mode 1)
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\<exwmx-commit-mode-map>Finish with `\\[exwmx-commit-finish]', "
              "Ignore with `\\[exwmx-commit-ignore]' "))))
    (pop-to-buffer buffer)))

(defun exwmx-commit-finish ()
  (interactive)
  (let ((string (buffer-string)))
    (delete-window)
    (exwmx--send-string string)))

(defun exwmx-commit-ignore ()
  (interactive)
  (delete-window)
  (kill-buffer exwmx-commit-buffer))


(provide 'exwmx-commit)

;;; exwmx-commit.el ends here
