;;; exwmx-dmenu.el --- simulate the dmenu command line program

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;;         Feng Shu <tumashu@163.com>
;; Created: 2015-12-01
;; Version: 1.0
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
(require 'exwmx-core)

(defgroup exwmx-dmenu nil
  "Simulate the dmenu command line program."
  :group 'extensions
  :group 'convenience)

(defcustom exwmx-dmenu-cache-file
  (locate-user-emacs-file "exwm-x/exwmx-dmenu-cache")
  "File in which the dmenu state is saved between Emacs sessions.
Variables stored are: `exwmx-dmenu--commands',
`exwmx-dmenu--history'. Must be set before initializing Dmenu."
  :type 'string
  :group 'exwmx-dmenu)

(defcustom exwmx-dmenu-prompt "EXWM-X Dmenu"
  "String to display in the exwm-dmenu or exwm-dmenu-simple prompt."
  :type 'string
  :group 'exwmx-dmenu)

(defcustom exwmx-dmenu-history-size 15
  "Determines on how many recently executed commands
dmenu should keep a record. "
  :type 'integer
  :group 'exwmx-dmenu)

(defcustom exwmx-dmenu-prefix-setting
  '(("," . exwmx-dmenu--run-with-terminal)
    (";" . exwmx-dmenu--run-emacs-command)
    ("-" . exwmx-dmenu--split-window-left-to-right)
    ("|" . exwmx-dmenu--split-window-top-to-bottom))
  "EXWM-X Dmenu command-prefix's setting."
  :group 'exwmx-dmenu)

(defvar exwmx-dmenu--initialized-p nil)
(defvar exwmx-dmenu--history nil)
(defvar exwmx-dmenu--commands nil)
(defvar exwmx-dmenu--update-timer nil)

(defvar exwmx-dmenu-ivy-minibuffer-map
  (let ((map (copy-keymap ivy-minibuffer-map)))
    map))

;;;###autoload
(defun exwmx-dmenu ()
  "EXWM-X dynamic menu, which will use ivy to show commands candidates."
  (interactive)
  (exwmx-dmenu--internal))

;;;###autoload
(defun exwmx-dmenu-simple ()
  "A simple version EXWM-X dynamic menu."
  (interactive)
  (exwmx-dmenu--internal t))

(defun exwmx-dmenu--internal (&optional simple-mode)
  "The internal function of `exwmx-dmenu'."
  (interactive)
  (make-directory (file-name-directory exwmx-dmenu-cache-file) t)
  (unless exwmx-dmenu--initialized-p
	(exwmx-dmenu-initialize))
  (unless exwmx-dmenu--commands
	(exwmx-dmenu--get-commands))
  (let (command)
    (while (< (length command) 1)
      (let ((commands
             (cl-remove-if #'(lambda (x)
                               (or (null x)
                                   (string-match-p "^\\." x)
                                   (string-match-p "^ *$" x)))
                           (cl-remove-duplicates
                            (append
                             (let* ((history (cl-remove-duplicates
                                              exwmx-dmenu--history
                                              :from-end t :test #'equal))
                                    (length (length history)))
                               (when history
                                 (cl-subseq history 0 (min length exwmx-dmenu-history-size))))
                             (exwmx-dmenu--get-emacs-commands)
                             exwmx-dmenu--commands)
                            :from-end t :test #'equal))))
        (setq command
              (substring-no-properties
               (if simple-mode
                   (read-from-minibuffer (concat exwmx-dmenu-prompt ": "))
                 (ivy-read
                  (concat exwmx-dmenu-prompt
                          (substitute-command-keys
                           "\\<exwmx-dmenu-ivy-minibuffer-map> (Edit with `\\[ivy-insert-current]'): "))
                  #'(lambda (input)
                      (cons (if (< (length input) 1)
                                "**NULL**"
                              input)
                            (cl-remove-if-not
                             #'(lambda (cmd)
                                 (string-match-p (funcall ivy--regex-function input) cmd))
                             commands)))
                  :dynamic-collection t
                  :keymap exwmx-dmenu-ivy-minibuffer-map))))))
    (when (equal command "**NULL**")
      (setq command ""))
    (setq exwmx-dmenu--history
          (cons command exwmx-dmenu--history))
    ;; We must record more history to cache file.
    (when (> (length exwmx-dmenu--history) 101)
      (setq exwmx-dmenu--history
            (cl-subseq exwmx-dmenu--history 0 exwmx-dmenu-history-size)))
    (exwmx-dmenu--run command)))

(defun exwmx-dmenu--run-with-terminal (command)
  "Run a `command' in a terminal emulator."
  (exwmx-terminal-emulator command))

(defun exwmx-dmenu--run-emacs-command (command)
  "If the function exwmx:command is found, call-interactively it."
  (let ((emacs-command
         (intern (concat "exwmx:" command))))
    (if (not (functionp emacs-command))
        (message "EXWM-X can't find emacs command: `%s'" emacs-command)
      (message "EXWM-X run emacs command: `%s'" emacs-command)
      (call-interactively emacs-command))))

(defun exwmx-dmenu--split-window-left-to-right (command)
  (exwmx-dmenu--split-window command 'left-to-right))

(defun exwmx-dmenu--split-window-top-to-bottom (command)
  (exwmx-dmenu--split-window command 'top-to-bottom))

(defun exwmx-dmenu--split-window (command type)
  (let* ((list (remove "" (split-string command "")))
         (num1 (string-to-number (nth 0 list)))
         (num2 (string-to-number (nth 1 list))))
    (when (and (> num1 0) (> num2 0))
      (delete-other-windows)
      (if (eq type 'top-to-bottom)
          (split-window-horizontally)
        (split-window-vertically))
      (dotimes (_ (- num1 1))
        (if (eq type 'top-to-bottom)
            (split-window-below)
          (split-window-right)))
      (other-window num1)
      (dotimes (_ (- num2 1))
        (if (eq type 'top-to-bottom)
            (split-window-below)
          (split-window-right)))
      (other-window num2)
      (balance-windows))))

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
  "Run EXWM-X command, depend `command''s prefix and body."
  (let* ((list (exwmx-dmenu--parse-command command))
         (prefix-list (car list))
         (command (cadr list)))
    (if (not command)
        (message "EXWM-X: No command will be executed.")
      (let ((emacs-command (intern command)))
        (if (and (< (length prefix-list) 1)
                 (string-match-p "^exwmx:" command)
                 (functionp emacs-command))
            (progn (message "EXWM-X run emacs command: `%s'" emacs-command)
                   (exwmx--switch-window)
                   (funcall emacs-command))
          (if (> (length prefix-list) 0)
              (dolist (prefix prefix-list)
                (let ((func (cdr (assoc prefix exwmx-dmenu-prefix-setting))))
                  (when (functionp func)
                    (unless (member func '(exwmx-dmenu--split-window-left-to-right
                                           exwmx-dmenu--split-window-top-to-bottom))
                      (exwmx--switch-window))
                    (funcall func command))))
            (exwmx-quickrun command)))))))

(defun exwmx-dmenu--get-emacs-commands ()
  "Get all emacs commands with name is match exwmx:command."
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
  "cache executable files for EXWM-X Dmenu."
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
