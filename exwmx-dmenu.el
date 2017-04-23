;;; exwmx-dmenu.el --- simulate the dmenu command line program

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;;         Feng Shu <tumashu@163.com>
;; Created: 2015-12-01
;; Version: 0.3
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: convenience, usability

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'swiper)
(require 'exwmx-core)
(require 'exwmx-utils)

(defgroup exwmx-dmenu nil
  "simulate the dmenu command line program."
  :group 'extensions
  :group 'convenience)

(defcustom exwmx-dmenu-cache-file
  (locate-user-emacs-file "exwm-x/exwmx-dmenu-cache")
  "File in which the dmenu state is saved between Emacs sessions.
Variables stored are: `exwmx-dmenu--commands',
`exwmx-dmenu--history'. Must be set before initializing Dmenu."
  :type 'string
  :group 'exwmx-dmenu)

(defcustom exwmx-dmenu-prompt "Exwmx-dmenu"
  "String to display in the exwm-dmenu or exwm-dmenu-simple prompt."
  :type 'string
  :group 'exwmx-dmenu)

(defcustom exwmx-dmenu-history-size 7
  "Determines on how many recently executed commands
dmenu should keep a record. "
  :type 'integer
  :group 'exwmx-dmenu)

(defcustom exwmx-dmenu-prefix-setting
  '(("," . exwmx-dmenu--run-with-terminal)
    (";" . exwmx-dmenu--run-with-terminal)
    ("-" . (lambda (_) (split-window-below)))
    ("|" . (lambda (_) (split-window-right))))
  "Exwmx-dmenu command-prefix's setting."
  :group 'exwmx-dmenu)

(defvar exwmx-dmenu--initialized-p nil)
(defvar exwmx-dmenu--history nil)
(defvar exwmx-dmenu--commands nil)
(defvar exwmx-dmenu--update-timer nil)

(defvar exwmx-dmenu-ivy-minibuffer-map
  (let ((map (copy-keymap ivy-minibuffer-map)))
    (define-key map (kbd "<return>") 'ivy-immediate-done)
    map))

;;;###autoload
(defun exwmx-dmenu ()
  (interactive)
  (exwmx-dmenu--internal))

;;;###autoload
(defun exwmx-dmenu-simple ()
  (interactive)
  (exwmx-dmenu--internal t))

(defun exwmx-dmenu--internal (&optional simple-mode)
  (interactive)
  (make-directory (file-name-directory exwmx-dmenu-cache-file) t)
  (unless exwmx-dmenu--initialized-p
	(exwmx-dmenu-initialize))
  (unless exwmx-dmenu--commands
	(exwmx-dmenu--get-commands))
  (let* ((command
          (substring-no-properties
           (if simple-mode
               (read-from-minibuffer (concat exwmx-dmenu-prompt ": "))
             (ivy-read
              (concat exwmx-dmenu-prompt
                      (substitute-command-keys
                       "\\<exwmx-dmenu-ivy-minibuffer-map> (select with `\\[ivy-alt-done]'): "))
              (cl-remove-if #'(lambda (x)
                                (or (string-match-p "^\\." x)
                                    (string-match-p "^ *$" x)))
                            (cl-remove-duplicates
                             (append exwmx-dmenu--history
                                     (exwmx-dmenu--get-emacs-commands)
                                     exwmx-dmenu--commands)
                             :from-end t :test #'equal))
              :keymap exwmx-dmenu-ivy-minibuffer-map)))))
    (setq exwmx-dmenu--history
          (cons command
                (remove command exwmx-dmenu--history)))
    (when (> (length exwmx-dmenu--history)
             exwmx-dmenu-history-size)
      (setcdr (nthcdr (- exwmx-dmenu-history-size 1)
                      exwmx-dmenu--history)
              nil))
    (exwmx-dmenu--run command)))

(defun exwmx-dmenu--run-with-terminal (command)
  (let ((cmd (format "%s -e 'bash -c %S'"
                     exwmx-terminal-emulator
                     (concat command "; exec bash"))))
    (message "Exwm-X run shell command: %s" cmd)
    (exwmx-shell-command cmd)))

(defun exwmx-dmenu--parse-command (string)
  (let ((prefix-list '())
        command)
    (while (> (length string) 0)
      (let ((first (substring string 0 1))
            (rest (substring string 1)))
        (if (or (assoc first exwmx-dmenu-prefix-setting)
                (equal first " "))
            (progn
              (unless (equal first " ")
                (progn (push first prefix-list)))
              (setq command rest)
              (setq string rest))
          (setq command string)
          (setq string nil))))
    (when (> (length command) 0)
      (list (delete-dups (reverse prefix-list))
            command))))

(defun exwmx-dmenu--run (command)
  (let* ((list (exwmx-dmenu--parse-command command))
         (prefix-list (car list))
         (command (cadr list)))
    (if (not command)
        (message "Exwm-X: No command will be executed.")
      (let ((emacs-command (intern command)))
        (if (and (< (length prefix-list) 1)
                 (string-match-p "^exwmx:" command)
                 (functionp emacs-command))
            (progn (message "Exwm-X run emacs command: `%s'" emacs-command)
                   (exwmx--switch-window)
                   (funcall emacs-command))
          (if (> (length prefix-list) 0)
              (dolist (prefix prefix-list)
                (let ((func (cdr (assoc prefix exwmx-dmenu-prefix-setting))))
                  (when (functionp func)
                    (exwmx--switch-window)
                    (funcall func command))))
            (message "Exwm-X jump-or-exec: %s" command)
            (exwmx-jump-or-exec command)))))))

(defun exwmx-dmenu--get-emacs-commands ()
  (let (output)
    (mapatoms
     #'(lambda (symbol)
         (when (and (string-match-p "^exwmx:" (symbol-name symbol))
                    (functionp symbol))
           (push (symbol-name symbol) output))))
    output))

(defun exwmx-dmenu-initialize ()
  (exwmx-dmenu-load-cache-file)
  (exwmx-dmenu-auto-update)
  (add-hook 'kill-emacs-hook 'exwmx-dmenu-save-cache-file)
  (setq exwmx-dmenu--initialized-p t))

(defun exwmx-dmenu-load-cache-file ()
  "Loads `exwmx-dmenu--history' and `exwmx-dmenu--commands'
from `exwmx-dmenu-cache-file'"
  (let ((save-file (expand-file-name exwmx-dmenu-cache-file)))
    (if (file-readable-p save-file)
        (with-temp-buffer
          (insert-file-contents save-file)
          (ignore-errors
            (setq exwmx-dmenu--commands (read (current-buffer)))
            (setq exwmx-dmenu--history (read (current-buffer)))))
      (setq exwmx-dmenu--history nil
            exwmx-dmenu--commands nil))))

(defun exwmx-dmenu-save-cache-file ()
  "Saves `exwmx-dmenu--history' and `exwmx-dmenu--commands'
to `exwmx-dmenu-cache-file'"
  (interactive)
  (with-temp-file (expand-file-name exwmx-dmenu-cache-file)
    (prin1 exwmx-dmenu--commands (current-buffer))
    (prin1 exwmx-dmenu--history (current-buffer))))

(defun exwmx-dmenu--get-commands()
  "cache executable files"
  (let* ((valid-exec-path (cl-remove-if-not
                           #'file-exists-p
                           (cl-remove-if-not #'stringp exec-path)))
         (files (cl-mapcan
                 (lambda (dir)
                   (directory-files dir t nil nil))
                 valid-exec-path))
         (commands
          (mapcar #'file-name-nondirectory
                  (cl-remove-if
                   #'file-directory-p
                   (cl-remove-if-not #'file-executable-p files)))))
    (setq exwmx-dmenu--commands
          (sort commands #'string<))))

(defun exwmx-dmenu-auto-update (&optional idle-time)
  "Update dmenu when Emacs has been idle for IDLE-TIME."
  (let ((idle-time (or idle-time 60)))
    (when exwmx-dmenu--update-timer
      (cancel-timer exwmx-dmenu--update-timer))
    (setq exwmx-dmenu--update-timer
          (run-with-idle-timer
           idle-time t
           #'exwmx-dmenu--get-commands))))

(provide 'exwmx-dmenu)

;;; exwmx-dmenu.el ends here
