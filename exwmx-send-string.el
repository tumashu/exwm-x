;;; exwmx-send-string.el --- Send string to application

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

;; * exwmx-send-string manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)
(require 'exwmx-core)

(defvar exwmx-send-string-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'exwmx-send-string-finish)
    (define-key keymap "\C-c\C-q" 'exwmx-send-string-ignore)
    keymap)
  "Keymap for `exwmx-send-string-mode'")

(define-minor-mode exwmx-send-string-mode
  "Minor for exwmx-send-string."
  nil " exwmx-send-string" exwmx-send-string-mode-map)

(defvar exwmx-send-string-buffer " *exwmx-send-string*")

(defun exwmx-send-string-with-minibuffer ()
  (interactive)
  (exwmx--send-string
   (read-from-minibuffer "Exwm-X: please input: ")))

(defun exwmx-send-string ()
  (interactive)
  (let ((buffer (get-buffer-create exwmx-send-string-buffer)))
    (with-current-buffer buffer
      (text-mode)
      (exwmx-send-string-mode)
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\<exwmx-send-string-mode-map>"
              "Finish with `\\[exwmx-send-string-finish]', "
              "Ignore with `\\[exwmx-send-string-ignore]'. "))))
    (pop-to-buffer buffer)))

(defun exwmx-send-string-finish ()
  (interactive)
  (let ((string (buffer-string)))
    (delete-window)
    (kill-buffer exwmx-send-string-buffer)
    (exwmx--send-string string)))

(defun exwmx-send-string-ignore ()
  (interactive)
  (delete-window)
  (kill-buffer exwmx-send-string-buffer))


(provide 'exwmx-send-string)

;;; exwmx-send-string.el ends here
