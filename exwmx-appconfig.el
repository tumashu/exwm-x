;;; exwmx-appconfig.el --- Send string to application

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.8.1
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

;; * exwmx-appconfig manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)

(defvar exwmx-appconfig-file (locate-user-emacs-file "exwm-x/exwmx-appconfig")
  "Application's Exwm-X configure file, Which will be used by
 many Exwm-X's commands.")

(defvar exwmx-appconfig-extra-properties nil
  "Extra properties which will be deal with by `exwmx-appconfig'.

These properties will not be inserted when create a new appconfig record,
user should add it by hand. but if user want to override an exist appconfig,
the history value will be inserted to exwmx-appconfig's buffer, then user
can to stay or edit it.")

(defvar exwmx-appconfig-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'exwmx-appconfig-finish)
    (define-key keymap "\C-c\C-q" 'exwmx-appconfig-ignore)
    keymap)
  "Keymap for `exwmx-appconfig-mode'")

(define-minor-mode exwmx-appconfig-mode
  "Minor for exwmx-appconfig."
  nil " exwmx-appconfig" exwmx-appconfig-mode-map)

(defvar exwmx-appconfig-buffer " *exwmx-appconfig*")

;; Fix compile warn
(defvar exwmx-sendstring-default-paste-key)

(defun exwmx-appconfig--search (string search-prop return-prop &optional equal)
  (when (and string (stringp string))
    (let ((file (expand-file-name exwmx-appconfig-file))
          appconfigs search-result)
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (ignore-errors
            (while (not (eobp))
              (push (read (current-buffer)) appconfigs)))))
      (while appconfigs
        (let* ((x (pop appconfigs))
               (search-string (plist-get x search-prop))
               (value (if (eq return-prop t)
                          x
                        (plist-get x return-prop))))
          (if equal
              (when (equal search-string string)
                (setq search-result value)
                (setq appconfigs nil))
            (when (or (exwmx--string-match-p string search-string)
                      (exwmx--string-match-p search-string string))
              (setq search-result value)
              (setq appconfigs nil)))))
      search-result)))

(defun exwmx-appconfig ()
  (interactive)
  (if (not (derived-mode-p 'exwm-mode))
      (message "Exwm-X: No application is found.")
    (unless (file-readable-p exwmx-appconfig-file)
      (append-to-file "" nil exwmx-appconfig-file))
    (let* ((buffer (get-buffer-create exwmx-appconfig-buffer))
           (history (exwmx-appconfig--search
                     (md5 (concat exwm-title exwm-instance-name)) :key t t))
           (appconfig (list :command
                            (or (plist-get history :command)
                                exwm-instance-name)
                            :alias
                            (or (plist-get history :alias)
                                exwm-instance-name)
                            :pretty-name
                            (or (plist-get history :pretty-name)
                                exwm-instance-name)
                            :paste-key
                            (or (plist-get history :paste-key)
                                exwmx-sendstring-default-paste-key)
                            :class
                            (or (plist-get history :class)
                                exwm-class-name)
                            :instance
                            (or (plist-get history :instance)
                                exwm-instance-name)
                            :title
                            (or (plist-get history :title)
                                exwm-title))))
      (dolist (prop exwmx-appconfig-extra-properties)
        (let ((value (plist-get history prop)))
          (when value
            (plist-put appconfig prop value))))
      (plist-put appconfig :key (md5 (concat exwm-title exwm-instance-name)))
      (with-current-buffer buffer
        (emacs-lisp-mode)
        (exwmx-appconfig-mode)
        (erase-buffer)
        (insert (format "%S\n" appconfig))
        (goto-char (point-min))
        (setq header-line-format
              (substitute-command-keys
               (concat
                "\\<exwmx-appconfig-mode-map>"
                "Appconfig: "
                "Finish with `\\[exwmx-appconfig-finish]', "
                "Ignore with `\\[exwmx-appconfig-ignore]'. "))))
      (pop-to-buffer buffer))))

(defun exwmx-appconfig-finish ()
  (interactive)
  (if exwmx-appconfig-mode
      (let ((file (expand-file-name exwmx-appconfig-file))
            key record appconfigs search-result)
        (goto-char (point-min))
        (setq record (read (current-buffer)))
        (setq key (plist-get record :key))
        (when (file-readable-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (ignore-errors
              (while (not (eobp))
                (push (read (current-buffer)) appconfigs)))
            (setq appconfigs
                  (cons record
                        (cl-remove-if
                         #'(lambda (x)
                             (equal key (plist-get x :key)))
                         appconfigs)))
            (erase-buffer)
            (mapc
             #'(lambda (x)
                 (insert (format "%S\n" x)))
             appconfigs)
            (write-file file)
            (delete-window)
            (kill-buffer exwmx-appconfig-buffer))))
    (message "Exwm-X: exwmx-appconfig-mode is not enabled.")))

(defun exwmx-appconfig-ignore ()
  (interactive)
  (if exwmx-appconfig-mode
      (progn
        (delete-window)
        (kill-buffer exwmx-appconfig-buffer))
    (message "Exwm-X: exwmx-appconfig-mode is not enabled.")))

(provide 'exwmx-appconfig)

;;; exwmx-appconfig.el ends here
