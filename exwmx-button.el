;;; exwmx-button.el --- Add some EXWM-X buttons to mode-line or header-line

;; * Header
;; Copyright 2015-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 1.0
;; Keywords: exwm, exwmx

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

;; * README                                                               :doc:
;;; Code:

;; * Code                                                                 :code:
(require 'exwmx-core)
(require 'switch-window)
(require 'exwmx-floating)

(defvar exwmx-button-floating-line
  '(kill-buffer
    hide
    toggle-char-line
    toggle-floating
    separator
    adjust-window-9
    adjust-window-8
    adjust-window-7
    adjust-window-6
    adjust-window-5
    separator
    title)
  "Button-line used by floating window.")

(defvar exwmx-button-tilling-line
  '(kill-buffer
    delete-window
    toggle-floating
    space
    mvborder-left
    delete-other-windows
    mvborder-right
    space
    split-window-below
    split-window-right
    space
    toggle-char-line
    space
    exwm-buffer-list
    separator
    title)
  "Button-line used by tilling window.")

(defvar exwmx-button-alist
  '((separator
     :tilling-label " - "
     :floating-label " - ")
    (space
     :tilling-label " "
     :floating-label " ")
    (kill-buffer
     :tilling-label "[X]"
     :floating-label "[X]"
     :mouse-1 (lambda (_) (exwmx-button-kill-buffer)))
    (delete-window
     :tilling-label "[D]"
     :floating-label "[D]"
     :mouse-1 (lambda (_) (delete-window)))
    (delete-other-windows
     :tilling-label "[+]"
     :floating-label "[+]"
     :mouse-1 (lambda (_) (delete-other-windows)))
    (mvborder-left
     :tilling-label "[<]"
     :floating-label "[<]"
     :mouse-1 (lambda (_) (switch-window-mvborder-left 10)))
    (mvborder-right
     :tilling-label "[>]"
     :floating-label "[>]"
     :mouse-1 (lambda (_) (switch-window-mvborder-right 10)))
    (split-window-below
     :tilling-label "[-]"
     :floating-label "[-]"
     :mouse-1 (lambda (_) (split-window-below)))
    (split-window-right
     :tilling-label "[|]"
     :floating-label "[|]"
     :mouse-1 (lambda (_) (split-window-right)))
    (hide
     :floating-label "[_]"
     :mouse-1 (lambda (_) (exwm-floating-hide)))
    (toggle-floating
     :tilling-label "[F]"
     :floating-label "[F]"
     :mouse-1 (lambda (_) (exwmx-floating-toggle-floating)))
    (toggle-char-line
     :tilling-label
     (lambda ()
       (cl-case exwm--on-KeyPress
         ((exwm-input--on-KeyPress-line-mode)
          (substitute-command-keys
           "[Line `\\[exwmx-button-toggle-keyboard]']"))
         ((exwm-input--on-KeyPress-char-mode)
          (substitute-command-keys
           "[Char `\\[exwmx-button-toggle-keyboard]']"))))
     :floating-label
     (lambda ()
       (cl-case exwm--on-KeyPress
         ((exwm-input--on-KeyPress-line-mode) "[L]")
         ((exwm-input--on-KeyPress-char-mode) "[C]")))
     :mouse-1 (lambda (_) (exwmx-button-toggle-keyboard)))
    (adjust-window-9
     :floating-label "[9]"
     :mouse-1 (lambda (_) (exwmx-floating-adjust-window 0.9 0.9 'center 0.02))
     :mouse-3 (lambda (_) (exwmx-floating-adjust-window 0.9 0.9)))
    (adjust-window-8
     :floating-label "[8]"
     :mouse-1 (lambda (_) (exwmx-floating-adjust-window 0.8 0.8 'center 0.05))
     :mouse-3 (lambda (_) (exwmx-floating-adjust-window 0.8 0.8)))
    (adjust-window-7
     :floating-label "[7]"
     :mouse-1 (lambda (_) (exwmx-floating-adjust-window 0.7 0.7 'center 0.05))
     :mouse-3 (lambda (_) (exwmx-floating-adjust-window 0.7 0.7)))
    (adjust-window-6
     :floating-label "[6]"
     :mouse-1 (lambda (_) (exwmx-floating-adjust-window 0.6 0.6 'center 0.05))
     :mouse-3 (lambda (_) (exwmx-floating-adjust-window 0.6 0.6)))
    (adjust-window-5
     :floating-label "[5]"
     :mouse-1 (lambda (_) (exwmx-floating-adjust-window 0.5 0.5 'center 0.05))
     :mouse-3 (lambda (_) (exwmx-floating-adjust-window 0.5 0.5)))
    (title
     :floating-label
     (lambda ()
       (format "%-100s" exwm-title))
     :tilling-label (lambda () exwm-title)
     :down-mouse-1 (lambda (e) (exwmx-floating-mouse-move e)))
    (exwm-buffer-list
     :tilling-label
     (lambda ()
       (format "{%s}"
               (mapconcat #'(lambda (x)
                              (propertize
                               (replace-regexp-in-string
                                "\\[EXWM-X\\]: " "" (buffer-name (cdr x)))
                               'exwm-buffer (cdr x)))
                          exwm--id-buffer-alist
                          " ")))
     :mouse-1
     (lambda (e)
       (let ((exwm-buffer
              (exwmx-button-text-property-at-event e 'exwm-buffer)))
         (when (buffer-live-p exwm-buffer)
           (exwm-workspace-switch-to-buffer exwm-buffer))))))
  "Exwmx-buttons' setting.")

(defvar exwmx-button-header-line-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line down-mouse-1] #'exwmx-button-mouse-handler)
    (define-key keymap [header-line mouse-1] #'exwmx-button-mouse-handler)
    (define-key keymap [header-line down-mouse-2] #'exwmx-button-mouse-handler)
    (define-key keymap [header-line mouse-2] #'exwmx-button-mouse-handler)
    (define-key keymap [header-line down-mouse-3] #'exwmx-button-mouse-handler)
    (define-key keymap [header-line mouse-3] #'exwmx-button-mouse-handler)
    keymap)
  "Keymap used by exwmx-buttons on header-line.")

(defvar exwmx-button-mode-line-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mode-line down-mouse-1] #'exwmx-button-mouse-handler)
    (define-key keymap [mode-line mouse-1] #'exwmx-button-mouse-handler)
    (define-key keymap [mode-line down-mouse-2] #'exwmx-button-mouse-handler)
    (define-key keymap [mode-line mouse-2] #'exwmx-button-mouse-handler)
    (define-key keymap [mode-line down-mouse-3] #'exwmx-button-mouse-handler)
    (define-key keymap [mode-line mouse-3] #'exwmx-button-mouse-handler)
    keymap)
  "Keymap used by exwmx-buttons on mode-line.")

;; Fix compile warn
(defvar exwm--keyboard-grabbed)

(defun exwmx-button-text-property-at-event (event text-property)
  "Get the TEXT-PROPERTY at EVENT start"
  (let ((target (posn-string (event-start event))))
    (get-text-property (cdr target) text-property (car target))))

(defun exwmx-button-mouse-handler (event)
  "Handle a mouse EVENT on an exwmx-button."
  (interactive "e")
  (let* ((name (exwmx-button-text-property-at-event
                event 'exwmx-button-name))
         (prop (intern (concat ":" (symbol-name (car event)))))
         (func (or (plist-get (cdr (assq name exwmx-button-alist)) prop)
                   ;; when :mouse-3 function is nil, fallback to use :mouse-1 function
                   (and (memq prop '(:double-down-mouse-1
                                     :triple-down-mouse-1
                                     :mouse-3
                                     :double-down-mouse-3
                                     :triple-down-mouse-3))
                        (plist-get (cdr (assq name exwmx-button-alist)) :mouse-1)))))
    (if (functionp func)
        (progn (funcall func event)
               (force-mode-line-update))
      (ignore))))

(defun exwmx-button-create (button-name &optional place)
  "Create an exwmx-button named BUTTON-NAME, which will be used in PLACE.
PLACE can be mode-line or header-line."
  (let* ((config (cdr (assq button-name exwmx-button-alist)))
         (label (if exwm--floating-frame
                    (plist-get config :floating-label)
                  (plist-get config :tilling-label)))
         (label (if (functionp label)
                    (funcall label)
                  label)))
    (when (and label (stringp label))
      (cond ((eq place 'header-line)
             (propertize
              label
              'exwmx-button-name button-name
              'help-echo ""
              'face 'header-line
              'mouse-face 'header-line-highlight
              'local-map exwmx-button-header-line-keymap))
            ((eq place 'mode-line)
             (propertize
              label
              'exwmx-button-name button-name
              'help-echo ""
              'face 'mode-line-emphasis
              'mouse-face 'mode-line-highlight
              'local-map exwmx-button-mode-line-keymap))
            (t nil)))))

(defun exwmx-button-create-line (buttons &optional place)
  "Create an exwmx button-line include BUTTONS, which will be use in PLACE.
PLACE can be mode-line or header-line."
  (mapcar
   #'(lambda (button-name)
       (exwmx-button-create button-name place))
   buttons))

(defun exwmx-button--update-button-line ()
  "Update all buffer's button-line."
  (interactive)
  ;; Set all buffer's mode-line or header-line.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((and (eq major-mode 'exwm-mode)
                  (not exwm--floating-frame))
             (kill-local-variable 'header-line-format)
             (setq mode-line-format
                   '(:eval (exwmx-button-create-line
                            exwmx-button-tilling-line 'mode-line))))
            ((and (eq major-mode 'exwm-mode)
                  exwm--floating-frame)
             (setq mode-line-format nil)
             (setq header-line-format
                   '(:eval (exwmx-button-create-line
                            exwmx-button-floating-line 'header-line))))
            (t nil)))
    (force-mode-line-update)))

(defun exwmx-button-kill-buffer (&optional buffer-or-name)
  "Kill buffer, if current buffer is a exwm buffer."
  (let ((buffer (or buffer-or-name
                    (current-buffer))))
    (with-current-buffer buffer
      (if (eq major-mode 'exwm-mode)
          (progn (kill-buffer buffer)
                 (exwmx-button--next-buffer))
        (message "This buffer is not a exwm buffer!")))))

(defun exwmx-button--next-buffer ()
  "Switch to next exwm buffer."
  (let ((buffer
         (car (cl-remove-if-not
               #'(lambda (buf)
                   (with-current-buffer buf
                     (eq major-mode 'exwm-mode)))
               (buffer-list)))))
    (when buffer
      (exwm-workspace-switch-to-buffer buffer))))

(defun exwmx-button-toggle-keyboard ()
  "Toggle between 'line-mode' and 'char-mode'."
  (interactive)
  (let ((id (exwm--buffer->id (window-buffer))))
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
      (message "EXWM-X: No application is actived."))))

(defun exwmx-button-enable ()
  "Enable exwmx-button."
  (add-hook 'exwm-update-class-hook #'exwmx-button--update-button-line)
  (add-hook 'exwm-update-title-hook #'exwmx-button--update-button-line)
  (add-hook 'buffer-list-update-hook #'exwmx-button--update-button-line))

;; * Footer

(provide 'exwmx-button)

;;; exwmx-modeline.el ends here
