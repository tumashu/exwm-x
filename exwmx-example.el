;;; exwmx-example.el --- a example configure of exwmx

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 1.0
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

;; * exwmx-example                                                  :doc:
;; exwmx configure example.

;;; Code:

;; * Code                                                            :code:

;; Require
(require 'exwm-x)

;; Disable menu-bar, tool-bar and scroll-bar to
;; increase the usable space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Shrink fringes to 1 pixel
(fringe-mode 1)

;; Disable dialog boxes since they are unusable in EXWM
(setq use-dialog-box nil)

;; Set workspace number
(setq exwm-workspace-number 4)

;; Set floating window border
(setq exwm-floating-border-width 3)
(setq exwm-floating-border-color "orange")

;; All buffers created in EXWM mode are named "*EXWM*".
;; You may want to change when a new window class name
;; or title is available. it in `exwm-update-class-hook'
;; and `exwm-update-title-hook', which are run
(add-hook 'exwm-update-class-hook #'exwmx-grocery--rename-exwm-buffer)
(add-hook 'exwm-update-title-hook #'exwmx-grocery--rename-exwm-buffer)

;; Manage `exwm-manage-finish-hook'
(add-hook 'exwm-manage-finish-hook #'exwmx-grocery--manage-finish-function)

;; Smart hide floating window
(exwmx-floating-smart-hide)

;; Enable exwmx-button
(exwmx-button-enable)

(defun exwmx:web-browser ()
  (interactive)
  (exwmx-quickrun "web-browser" t))

(defun exwmx:file-browser ()
  (interactive)
  (exwmx-quickrun "file-browser" t))

(defun exwmx:terminal ()
  (interactive)
  (exwmx-quickrun "terminal" t))

(defun exwmx:emacs ()
  (interactive)
  (exwmx-quickrun "emacs" t))

(defun exwmx-switch-to-1-workspace ()
  (interactive)
  (exwm-workspace-switch 0))

(defun exwmx-switch-to-2-workspace ()
  (interactive)
  (exwm-workspace-switch 1))

(defun exwmx-switch-to-3-workspace ()
  (interactive)
  (exwm-workspace-switch 2))

(defun exwmx-switch-to-4-workspace ()
  (interactive)
  (exwm-workspace-switch 3))

;; Unset global key: "C-t"
(global-set-key (kbd "C-t") nil)

;; Use "C-t" as an exwm prefix key.
(push ?\C-t exwm-input-prefix-keys)

;; Note: keybinds setting with `exwmx-input-set-key' are
;; *global keybinds*, if you can solve the problem with
;; other approach, for example: `exwm-mode-map', you should
;; not use it.
;;
;; * Setting it only you have no choice... *
;;
;; Note: `exwmx-input-set-key' is similar with `exwm-input-set-key',
;; the different is that ‘exwmx-input-set-key’ protect key
;; from being override by other minor modes with the help of
;; ‘bind-key*’.

(exwmx-input-set-key (kbd "C-t ;") #'exwmx-dmenu)
(exwmx-input-set-key (kbd "C-t :") #'counsel-linux-app)
(exwmx-input-set-key (kbd "C-t C-e") #'exwmx-sendstring)
(exwmx-input-set-key (kbd "C-t C-r") #'exwmx-appconfig)

(exwmx-input-set-key (kbd "C-t 1")  #'exwmx-switch-to-1-workspace)
(exwmx-input-set-key (kbd "C-t 2")  #'exwmx-switch-to-2-workspace)
(exwmx-input-set-key (kbd "C-t 3")  #'exwmx-switch-to-3-workspace)
(exwmx-input-set-key (kbd "C-t 4")  #'exwmx-switch-to-4-workspace)

;; We always need a way to switch between line-mode and char-mode
(exwmx-input-set-key (kbd "C-t C-t") #'exwmx-button-toggle-keyboard)

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; If you don't use exim, you may need not to set "?\C-\\"
;; (push ?\C-\\ exwm-input-prefix-keys)

(require 'switch-window)
;; switch-window 'default input style do not work well with exwm.
(setq switch-window-input-style 'minibuffer)
(define-key exwm-mode-map (kbd "C-x o") #'switch-window)
(define-key exwm-mode-map (kbd "C-x 1") #'switch-window-then-maximize)
(define-key exwm-mode-map (kbd "C-x 2") #'switch-window-then-split-below)
(define-key exwm-mode-map (kbd "C-x 3") #'switch-window-then-split-right)
(define-key exwm-mode-map (kbd "C-x 0") #'switch-window-then-delete)

;; Let kill-ring works with app
(define-key exwm-mode-map (kbd "C-c y") #'exwmx-sendstring-from-kill-ring)

;; `exwmx-floating-toggle-floating' does not work well with EXWM-X,
;; See: https://github.com/ch11ng/exwm/issues/248
(define-key exwm-mode-map (kbd "C-c C-t C-f") #'exwmx-floating-toggle-floating)

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)))

;; Don't delete it
(exwm-enable)

;; * Footer
(provide 'exwmx-example)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exwmx-example.el ends here
