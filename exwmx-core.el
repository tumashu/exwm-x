;;; exwmx-core.el --- Core functions used by exwmx

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 1.0
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
(require 'cl-lib)
(require 'exwm)
(require 'switch-window)

(defvar exwmx-terminal-emulator "xterm"
  "Exwm-X default terminal emulator")

(defvar exwm--keyboard-grabbed)
(declare-function exwmx-button--update-button-line "exwmx-button" nil)
(declare-function exwmx-appconfig--select-appconfig "exwmx-appconfig" ())
(declare-function exwmx-appconfig--get-all-appconfigs "exwmx-appconfig" ())
(declare-function exwmx-appconfig--add-appconfig "exwmx-appconfig" (appconfig))
(declare-function exwmx-appconfig--search "exwmx-appconfig"
                  (string search-prop return-prop &optional equal))

(defun exwmx--string-match-p (regexp string)
  "A wrap of `string-match-p', it can work when `string' is not a
string."
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string)))

(defun exwmx--switch-window ()
  "A switch-window command's wrapper used by Exwm-X.

Note: switch-window default input style do not work well
with Exwm-x, user should use 'minibuffer input style instead,
for example, add the following line:

  (setq switch-window-input-style 'minibuffer)

to your ~/.emacs file."
  (unless (eq switch-window-input-style 'minibuffer)
    (warn (concat "Exwm-X: please setq the value of "
                  "`switch-window-input-style' to 'minibuffer.")))
  (switch-window--then
   "Run command in window: "
   #'(lambda () (other-window 1))
   nil nil 1))

(defun exwmx-manage-finish-function ()
  "Exwm-X hook function, used by `exwm-manage-finish-hook'."
  (let* ((appconfig (exwmx-appconfig--search exwm-class-name :class t t))
         (floating (plist-get appconfig :floating))
         (prefix-keys-added (plist-get appconfig :add-prefix-keys))
         (prefix-keys-removed (plist-get appconfig :remove-prefix-keys))
         (ignore-simulation-keys (plist-get appconfig :ignore-simulation-keys))
         (expression (plist-get appconfig :eval)))
    ;; Deal with prefix-keys of application
    (when (and prefix-keys-removed
               (listp prefix-keys-removed))
      (dolist (key prefix-keys-removed)
        (setq-local exwm-input-prefix-keys
                    (remove key exwm-input-prefix-keys))))
    (when (eq prefix-keys-removed t)
      (setq-local exwm-input-prefix-keys nil))
    (when (and prefix-keys-added
               (listp prefix-keys-added))
      (setq-local exwm-input-prefix-keys
                  (append prefix-keys-added exwm-input-prefix-keys)))
    ;; Deal with simulation-keys of application
    (when ignore-simulation-keys
      (exwm-input-set-local-simulation-keys nil))
    ;; Deal with window floating
    (when floating
      (exwm-floating--set-floating exwm--id))
    ;; Eval the expression from :eval
    (when expression
      (eval expression))))

(defun exwmx--get-pretty-name ()
  "Get a pretty name of an application, based on application's :pretty-name,
:class, :instance or :title which is stored in `exwmx-appconfig-file'."
  (let ((prefer-name (exwmx-appconfig--search exwm-class-name :class :pretty-name t)))
    (cond ((and (> (length exwm-title) 0)
                (< (length exwm-title) 10)) exwm-title)
          (prefer-name prefer-name)
          (exwm-instance-name exwm-instance-name)
          (exwm-class-name exwm-class-name))))

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

(defun exwmx-toggle-keyboard (&optional id)
  "Toggle between 'line-mode' and 'char-mode'."
  (interactive (list (exwm--buffer->id (window-buffer))))
  (if id
      (with-current-buffer (exwm--id->buffer id)
        (if exwm--keyboard-grabbed
            (progn
              (message "Switch to `char-mode', application will take up your keyboard.")
              (exwm-input-release-keyboard id))
          (message
           (substitute-command-keys
            (concat
             "\\<exwm-mode-map>Reset to `line-mode', "
             "`\\[exwm-input-send-next-key]' -> send key to application.")))
          (exwm-reset)))
    (message "Exwm-x: No application is actived."))
  (exwmx-button--update-button-line))

(defun exwmx-jump-or-exec (command &optional current-window search-alias)
  "if window which command matched `command' can be found, switch to this window,
otherwise run shell command `command', user need to select the place of application
window unless `current-window' set to t, when `search-alias' is t, `command' will
be regard as a alias of appconfig and search it from `exwmx-appconfig-file'."
  (unless current-window
    (exwmx--switch-window))
  (let* ((appconfigs (exwmx-appconfig--get-all-appconfigs))
         (cmd (if search-alias
                  (or (exwmx-appconfig--search command :alias :command t)
                      (when appconfigs
                        (let ((appconfig (exwmx-appconfig--select-appconfig)))
                          (plist-put appconfig :alias command)
                          (exwmx-appconfig--add-appconfig appconfig)
                          (plist-get appconfig :command))))
                command))
         (buffer (or (if search-alias
                         (exwmx--find-buffer
                          (exwmx-appconfig--search command :alias :class t))
                       (exwmx--find-buffer
                        (exwmx-appconfig--search command :command :class t)))
                     ;; The below two rules are just guess rules :-)
                     ;; Suggest use `exwmx-appconfig' to manage app's information.
                     (exwmx--find-buffer
                      (capitalize (concat "^" (car (split-string command " ")))))
                     (exwmx--find-buffer
                      (concat "^" (car (split-string command " ")))))))
    (if (and search-alias (not cmd))
        (message "Exwm-X: please run `exwmx-appconfig' to add appconfig.")
      (message "Exwm-X jump-or-exec: %s" cmd))
    ;; If current application window is a floating-window, minumize it.
    (when (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
      (exwm-floating-hide))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (when cmd
        (start-process-shell-command cmd nil cmd)))))

(defun exwmx-floating-hide-all ()
  "Hide all floating window."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'exwm-mode)
                 exwm--floating-frame)
        (exwm-floating-hide)))))

(defun exwmx--find-buffer (regexp)
  "Find such a exwm buffer: its local variables: `exwm-class-name',
`exwm-instance-name' or `exwm-title' is matched `regexp'."
  (when (and regexp (stringp regexp))
    (let* ((buffers (buffer-list))
           (buffers-list (list nil nil nil)))

      (dolist (buffer buffers)
        (let ((wininfo `((0 . ,(buffer-local-value 'exwm-title buffer))
                         (1 . ,(buffer-local-value 'exwm-instance-name buffer))
                         (2 . ,(buffer-local-value 'exwm-class-name buffer)))))
          (dolist (x wininfo)
            (when (exwmx--string-match-p regexp (cdr x))
              (setf (nth (car x) buffers-list)
                    (append (list buffer) (nth (car x) buffers-list)))))))

      (caar (delq nil
                  (sort buffers-list
                        #'(lambda (a b)
                            (< (length a) (length b)))))))))

(defun exwmx-kill-exwm-buffer (&optional buffer-or-name)
  "Kill buffer, if current buffer is a exwm buffer."
  (let ((buffer (or buffer-or-name
                    (current-buffer))))
    (with-current-buffer buffer
      (if (eq major-mode 'exwm-mode)
          (progn (kill-buffer buffer)
                 (exwmx--next-exwm-buffer))
        (message "This buffer is not a exwm buffer!")))))

(defun exwmx-shell-command (cmd)
  "Run shell command `cmd'."
  (start-process-shell-command cmd nil cmd))

(defun exwmx-shell-command-interactively (cmd)
  "Run shell command `cmd' interactively."
  (interactive
   (list (read-shell-command "Run shell command: ")))
  (start-process-shell-command cmd nil cmd))

(defun exwmx-mouse-move-floating-window (start-event)
  "This is a mouse drag event function used by exwmx-button,
when drag mouse from such button, move current floating window dynamic."
  (interactive "e")
  (exwmx--mouse-operate-floating-window start-event))

(defun exwmx-mouse-resize-floating-window (start-event)
  "This is a mouse drag event function used by exwmx-button,
when drag mouse from such button, resize current floating window dynamic."
  (interactive "e")
  (exwmx--mouse-operate-floating-window start-event t))

(defun exwmx--mouse-operate-floating-window (start-event &optional resize)
  "Internal function of `exwmx-mouse-move-floating-window'
and `exwmx-mouse-move-floating-window'"
  (interactive "e")
  (when exwm--floating-frame
    (let* ((orig-mouse (mouse-position))
           (orig-x (car (cdr orig-mouse)))
           (orig-y (cdr (cdr orig-mouse)))
           (frame (window-frame (car (car (cdr start-event)))))
           (frame-width (frame-width frame))
           (frame-height (frame-height frame))
           (char-width (frame-char-width frame))
           (char-height (frame-char-height frame))
           (echo-keystrokes 0)
           (done nil)
           (last-x orig-x)
           (last-y orig-y)
           event mouse x y)
      (track-mouse
        (while (not done)
          (setq event (read-event)
                mouse (mouse-position))
          ;; do nothing if
          ;;   - there is a switch-frame event.
          ;;   - the mouse isn't in the frame that we started in
          ;;   - the mouse isn't in any Emacs frame
          ;; drag if
          ;;   - there is a mouse-movement event
          ;;   - there is a scroll-bar-movement event
          ;;     (same as mouse movement for our purposes)
          ;; quit if
          ;;   - there is a keyboard event or some other unknown event
          ;;     unknown event.
          (cond ((integerp event)
                 (setq done t))
                ((eq (car event) 'switch-frame)
                 nil)
                ((not (memq (car event)
                            '(mouse-movement scroll-bar-movement)))
                 (setq done t))
                ((not (eq (car mouse) frame))
                 nil)
                ((null (car (cdr mouse)))
                 nil)
                (t (setq x (car (cdr mouse))
                         y (cdr (cdr mouse)))
                   (if resize
                       (set-frame-size
                        frame
                        (- frame-width (- orig-x x))
                        (- frame-height (- orig-y y)))
                     (exwm-floating-move
                      (* char-width (- x orig-x))
                      (* char-width (- y orig-y)))))))))))

(defun exwmx-floating-toggle-floating ()
  "Toggle the current window between floating and non-floating states."
  (interactive)
  (with-current-buffer (window-buffer)
    (if exwm--floating-frame
        (progn
          (setq header-line-format nil)
          (exwm-layout--refresh)
          (exwm-floating--unset-floating exwm--id))
      (exwm-floating--set-floating exwm--id))))

;; Hack the bug: https://github.com/ch11ng/exwm/issues/248
(advice-add 'exwm-floating-toggle-floating :override #'exwmx-floating-toggle-floating)

;; * Footer
(provide 'exwmx-core)

;;; exwmx-core.el ends here
