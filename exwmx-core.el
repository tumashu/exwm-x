;;; exwmx-core.el --- Core functions used by exwmx

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwmx
;; Version: 0.0.1
;; Keywords: window-manager, exwm, exwmx

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

;; * exwmx-core manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)

(defvar exwmx-prefer-name-alist
  '(("navigator" . "Firefox")
    ("virtual[ ]*box" . "VirtualBox")
    ("gimp" . "Gimp")
    ("default-terminal" . "Term"))
  "Dict used by `exwmx--get-prefer-name'")

(defun exwmx--get-prefer-name ()
  "Get a prefer name of a application, based on its class-name, instance-name
and title."
  (let* ((dict-alist exwmx-prefer-name-alist)
         (prefer-name
          (or (exwmx--replace-string exwm-title dict-alist)
              (exwmx--replace-string exwm-instance-name dict-alist)
              (exwmx--replace-string exwm-class-name dict-alist))))
    (cond ((and (> (length exwm-title) 0)
                (< (length exwm-title) 10)) exwm-title)
          (prefer-name prefer-name)
          (exwm-instance-name exwm-instance-name)
          (exwm-class-name exwm-class-name))))

(defun exwmx--replace-string (string dict-alist)
  "If the `string' match the car of element in `dict-alist',
return its cdr value."
  (let ((case-fold-search t)
        new-string)
    (dolist (x dict-alist)
      (when (exwmx--string-match-p (car x) string)
        (setq dict-alist nil)
        (setq new-string (cdr x))))
    new-string))

(defun exwmx--string-match-p (regexp string)
  "A wrap of `string-match-p', it can work when `string' is not a
string."
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string)))

(defun exwmx--next-exwm-buffer ()
  "Switch to next exwm buffer."
  (let ((buffer
         (car (cl-remove-if-not
               #'(lambda (buf)
                   (with-current-buffer buf
                     (eq major-mode 'exwm-mode)))
               (buffer-list)))))
    (when buffer
      (exwm-workspace-switch-to-buffer buffer))))

;; * Footer
(provide 'exwmx-core)

;;; exwmx-core.el ends here
