;;; exwmx-example.el --- a example configure of exwmx

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwmx
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

;; * exwmx-example                                                  :doc:
;; exwmx configure example.

;;; Code:

;; * Code                                                            :code:

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
(add-hook 'exwm-update-class-hook #'exwmx-rename-exwm-buffer)
(add-hook 'exwm-update-title-hook #'exwmx-rename-exwm-buffer)

(defun exwmx-rename-exwm-buffer ()
  (exwm-workspace-rename-buffer
   (concat "Exwm:" (exwmx--get-prefer-name))))

(defun exwmx/web-browser ()
  (interactive)
  (exwmx-jump-or-exec "Icecat" "icecat"))

(defun exwmx/file-manager ()
  (interactive)
  (exwmx-jump-or-exec "Thunar" "thunar"))

(defun exwmx/terminal ()
  (interactive)
  (exwmx-jump-or-exec "default-terminal" "xfce4-terminal -T default-terminal"))

(defun exwmx/new-terminal ()
  (interactive)
  (exwmx-shell-command "xfce4-terminal"))

(defun exwmx/xset-bell-off ()
  (interactive)
  (exwmx-shell-command "xset b off"))

(defun exwmx/xmodmap ()
  (interactive)
  (exwmx-shell-command "xmodmap -e 'keycode 135 = Super_R'"))

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

;; Don't Delete the below two lines
(global-unset-key (kbd "C-t"))
(push ?\C-t exwm-input-prefix-keys)

(exwm-input-set-key (kbd "C-t R")  nil)
(exwm-input-set-key (kbd "C-t q")  nil)
(exwm-input-set-key (kbd "C-t m")  nil)
(exwm-input-set-key (kbd "C-t v")  'exwmx/file-manager)
(exwm-input-set-key (kbd "C-t c")  'exwmx/terminal)
(exwm-input-set-key (kbd "C-t ff") 'exwmx/web-browser)
(exwm-input-set-key (kbd "C-t C-x")  'exwmx/new-terminal)

(exwm-input-set-key (kbd "C-t 1")  'exwmx-switch-to-1-workspace)
(exwm-input-set-key (kbd "C-t 2")  'exwmx-switch-to-2-workspace)
(exwm-input-set-key (kbd "C-t 3")  'exwmx-switch-to-3-workspace)
(exwm-input-set-key (kbd "C-t 4")  'exwmx-switch-to-4-workspace)

(exwm-input-set-key (kbd "C-S-<up>") 'exwmx-move-border-up)
(exwm-input-set-key (kbd "C-S-<down>") 'exwmx-move-border-down)
(exwm-input-set-key (kbd "C-S-<left>") 'exwmx-move-border-left)
(exwm-input-set-key (kbd "C-S-<right>") 'exwmx-move-border-right)

;; We always need a way to go back to line-mode from char-mode
(exwm-input-set-key (kbd "C-t t") 'exwm-reset)
(exwm-input-set-key (kbd "C-t C-t") 'exwm-reset)
(exwm-input-set-key (kbd "s-r") 'exwm-reset)

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(push ?\C-\\ exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

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

(require 'windmove)
(exwm-input-set-key (kbd "C-<up>") 'windmove-up)
(exwm-input-set-key (kbd "C-<down>") 'windmove-down)
(exwm-input-set-key (kbd "C-<left>") 'windmove-left)
(exwm-input-set-key (kbd "C-<right>") 'windmove-right)

(require 'start-menu)
(start-menu-enable)
(exwm-input-set-key (kbd "C-t ,")  'start-menu-popup)

(require 'dmenu)
(setq dmenu-prompt-string "dmenu: ")
(exwm-input-set-key (kbd "C-t c") 'dmenu)

(require 'switch-window)
(setq switch-window-increase 8)
(setq switch-window-shortcut-style 'qwerty)
(exwm-input-set-key (kbd "C-x o") 'switch-window)
(exwm-input-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(exwm-input-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(exwm-input-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(exwm-input-set-key (kbd "C-x 0") 'switch-window-then-delete)

;; * Footer
(provide 'exwmx-example)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exwmx-example.el ends here
