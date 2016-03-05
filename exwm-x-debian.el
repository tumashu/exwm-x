;;; exwm-x-debian.el --- Let exwm-x work better with debian

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

;; * exwm-x-debian manual                                                  :doc:

;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(defun exwm-x-generate-debian-menu (debian-menu-file)
  "Generate a emacs command based of the info of `debian-menu-file'."
  (when (file-exists-p debian-menu-file)
    (let ((file-name (file-name-nondirectory debian-menu-file))
          (file-content (with-temp-buffer
                          (insert-file-contents debian-menu-file)
                          (buffer-string)))
          command section title hints icon needs)
      (when (string-match "command=\"\\([^\"]+\\)\"" file-content)
        (setq command (match-string 1 file-content)))
      (when (string-match "section=\"\\([^\"]+\\)\"" file-content)
        (setq section (match-string 1 file-content)))
      (when (string-match "title=\"\\([^\"]+\\)\"" file-content)
        (setq title (match-string 1 file-content)))
      (when (string-match "hints=\"\\([^\"]+\\)\"" file-content)
        (setq hints (match-string 1 file-content)))
      (when (string-match "icon=\"\\([^\"]+\\)\"" file-content)
        (setq icon (match-string 1 file-content)))
      (when (string-match "needs=\"\\([^\"]+\\)\"" file-content)
        (setq needs (match-string 1 file-content)))
      (when command
        (if (equal needs "text")
            (eval `(defun ,(intern (concat "exwm-x/menu/" file-name)) ()
                     (interactive)
                     (start-process-shell-command
                      ,command nil
                      ,(format "x-terminal-emulator --title %s -e %s" command command))))
          (eval `(defun ,(intern (concat "exwm-x/menu/" file-name)) ()
                   (interactive)
                   (start-process-shell-command ,command nil ,command))))))))

(defun exwm-x-generate-debian-menus ()
  "Generate emacs commands for all the debian menu files available."
  (dolist (debian-menu-dir '("/usr/share/menu/"
                             "/usr/lib/menu/"
                             "/etc/menu/"
                             "~/.menu/"))
    (when (file-exists-p debian-menu-dir)
      (dolist (debian-menu-file (directory-files debian-menu-dir t "[^.].*"))
        (exwm-x-generate-debian-menu debian-menu-file)))))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-debian)

;;; exwm-x-debian.el ends here
;; #+END_SRC
