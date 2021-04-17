;;; exwmx-appmenu.el --- EXWM-X application menu

;; * Header
;; Copyright 2016-2021 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
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

;; * exwmx-appmenu manual                                                   :doc:

;;; Code:

;; * Code                                                                 :code:
(require 'cl-lib)
(require 'exwmx-core)
(require 'xdg)

(defvar exwmx-appmenu-buffer " *exwmx-appmenu-buffer*")

(defvar exwmx-appmenu-format-function #'exwmx-appmenu-format-function-default)

(defvar exwmx-appmenu--apps-cache nil
  "Cache of desktop files data.")

(defvar exwmx-appmenu--apps-cached-files nil
  "List of cached desktop files.")

(defvar exwmx-appmenu--apps-cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar exwmx-appmenu--apps-cache-format-function nil
  "The function used to format the cached application menu.")

(defvar exwmx-appmenu-linux-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
          (cons (xdg-data-home)
                (xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files).")

(defvar exwmx-appmenu-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'exwmx-appmenu-quit)
    (define-key keymap (kbd "Q") 'exwmx-appmenu-quit)
    (define-key keymap (kbd "RET") 'exwmx-appmenu-launch)
    (define-key keymap [mouse-1] 'exwmx-appmenu-launch)
    (define-key keymap [mouse-3] 'exwmx-appmenu-quit)
    keymap)
  "Keymap for `exwmx-appmenu-mode'")

(define-derived-mode exwmx-appmenu-mode read-only-mode "exwmx-appmenu-mode"
  "Exwmx-appmenu major mode.")

(defun exwmx-appmenu-format-function-default (name _comment exec)
  "Format application names with the NAME (and COMMENT) first.
EXEC is the command to launch the application."
  (format "%-30s (%S)" name exec))

(defun exwmx-appmenu-linux-apps-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir exwmx-appmenu-linux-apps-directories)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          ;; Function `directory-files-recursively' added in Emacs 25.1.
          (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
            (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
              (when (and (not (gethash id hash)) (file-readable-p file))
                (push (cons id file) result)
                (puthash id file hash)))))))
    result))

(defun exwmx-appmenu-linux-app--parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
          (end (re-search-forward "^\\[" nil t))
          (visible t)
          name comment exec)
      (catch 'break
        (unless start
          (push file exwmx-appmenu-linux-apps-faulty)
          (message "Warning: File %s has no [Desktop Entry] group" file)
          (throw 'break nil))

        (goto-char start)
        (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
          (setq visible nil))
        (setq name (match-string 1))

        (goto-char start)
        (unless (re-search-forward "^Type *= *Application *$" end t)
          (throw 'break nil))
        (setq name (match-string 1))

        (goto-char start)
        (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
          (push file exwmx-appmenu-linux-apps-faulty)
          (message "Warning: File %s has no Name" file)
          (throw 'break nil))
        (setq name (match-string 1))

        (goto-char start)
        (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
          (setq comment (match-string 1)))

        (goto-char start)
        (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
          ;; Don't warn because this can technically be a valid desktop file.
          (throw 'break nil))
        (setq exec (match-string 1))

        (goto-char start)
        (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
          (let ((try-exec (match-string 1)))
            (unless (locate-file try-exec exec-path nil #'file-executable-p)
              (throw 'break nil))))
        (propertize
         (funcall exwmx-appmenu-linux-app-format-function name comment exec)
         'visible visible)))))

(defun exwmx-appmenu-linux-apps-parse (desktop-entries-alist)
  "Parse the given alist of Linux desktop entries.
Each entry in DESKTOP-ENTRIES-ALIST is a pair of ((id . file-name)).
Any desktop entries that fail to parse are recorded in
`exwmx-appmenu-linux-apps-faulty'."
  (let (result)
    (setq exwmx-appmenu-linux-apps-faulty nil)
    (dolist (entry desktop-entries-alist result)
      (let* ((id (car entry))
             (file (cdr entry))
             (r (exwmx-appmenu-linux-app--parse-file file)))
        (when r
          (push (cons r id) result))))))

(defun exwmx-appmenu--get-apps-list ()
  "Return list of all desktop applications."
  (let* ((new-desktop-alist (exwmx-appmenu-linux-apps-list-desktop-files))
         (new-files (mapcar 'cdr new-desktop-alist))
         (exwmx-appmenu-linux-app-format-function exwmx-appmenu-format-function))
    (unless (and
             (eq exwmx-appmenu-linux-app-format-function
                 exwmx-appmenu--apps-cache-format-function)
             (equal new-files exwmx-appmenu--apps-cached-files)
             (null (cl-find-if
                    (lambda (file)
                      (time-less-p
                       exwmx-appmenu--apps-cache-timestamp
                       (nth 5 (file-attributes file))))
                    new-files)))
      (setq exwmx-appmenu--apps-cache (exwmx-appmenu-linux-apps-parse new-desktop-alist)
            exwmx-appmenu--apps-cache-format-function exwmx-appmenu-format-function
            exwmx-appmenu--apps-cache-timestamp (current-time)
            exwmx-appmenu--apps-cached-files new-files)))
  exwmx-appmenu--apps-cache)

(defun exwmx-appmenu ()
  "Show exwmx's application menu."
  (interactive)
  (when (buffer-live-p exwmx-appmenu-buffer)
    (kill-buffer exwmx-appmenu-buffer))
  (let ((buffer (get-buffer-create exwmx-appmenu-buffer))
        (n 1)
        content)
    (with-current-buffer buffer
      (switch-to-buffer buffer)
      (dolist (x (cl-sort
                  (exwmx-appmenu--get-apps-list)
                  #'(lambda (a b)
                      (string< (car a) (car b)))))
        (let* ((desktop-shortcut (cdr x))
               (item
                (propertize
                 (format "%02S. %s" n (car x))
                 'desktop-shortcut desktop-shortcut
                 'face 'font-lock-builtin-face)))
          (if content
              (setq content (concat content "\n" item))
            (setq content item))
          (setq n (1+ n))))
      (insert content)
      (goto-char (point-min))
      (exwmx-appmenu-mode))))

(defun exwmx-appmenu-launch ()
  "Call application at the current menu item."
  (interactive)
  (let* ((desktop-shortcut (get-text-property (point) 'desktop-shortcut)))
    (if desktop-shortcut
        (progn
          (call-process "gtk-launch" nil nil nil desktop-shortcut)
          (message "EXWM-X Appmenu: launch '%s' ..." desktop-shortcut))
      (message "EXWM-X Appmenu: no application is launched!"))
    (exwmx-appmenu-quit t)))

(defun exwmx-appmenu-quit (&optional silent)
  "Quit emwmx-appmenu."
  (interactive)
  (let ((buffer (get-buffer exwmx-appmenu-buffer)))
    (when (and buffer (buffer-live-p buffer))
      (kill-buffer buffer))
    (unless silent
      (message "EXWM-X Appmenu: Quit!"))))

(defun exwmx-appmenu-simple ()
  (interactive)
  (let* ((apps (exwmx-appmenu--get-apps-list))
         (app (completing-read "EXWM-X Appmenu: " apps))
         (desktop-shortcut
          (alist-get app apps nil nil #'equal)))
    (if desktop-shortcut
        (progn
          (call-process "gtk-launch" nil nil nil desktop-shortcut)
          (message "EXWM-X Appmenu: launch '%s' ..." desktop-shortcut))
      (message "EXWM-X Appmenu: no application is launched!"))))

(provide 'exwmx-appmenu)
;;; exwmx-appmenu.el ends here
