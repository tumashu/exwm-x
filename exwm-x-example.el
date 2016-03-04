;;; exwm-x-example.el --- a example configure of exwm-x

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

;; * exwm-x-example manual                                                 :doc:
;; ** Description
;; An exwm-x configure example.

;; ** Configure
;; Create ~/.xinitrc file or ~/.xsession file which content likes
;; the below example:

;; #+BEGIN_EXAMPLE
;; # You may need to comment out the next line to disable access control
;; xhost +

;; # Kill beep
;; xset b off

;; # Tell emacs load exwm-x-example.el file's content.
;; export exwm_x_enable="yes"

;; # Emacs X input method (exim) setting
;; export XMODIFIERS=@im=exim
;; export GTK_IM_MODULE=xim
;; export QT_IM_MODULE=xim
;; export CLUTTER_IM_MODULE=xim

;; # Launch exwm
;;  exec dbus-launch --exit-with-session emacs

;; #+END_EXAMPLE

;; ** Run exwm
;; Run startx command or login with display-manager

;;; Code:

;; * Code                                                                 :code:

;; #+BEGIN_SRC emacs-lisp
(use-package exwm-x
  :if (string= (getenv "exwm_x_enable") "yes")
  :ensure nil
  :demand t
  :config

  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Shrink fringes to 1 pixel
  (fringe-mode 1)

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  ;; Set floating window border
  (setq exwm-floating-border-width 3)
  (setq exwm-floating-border-color "orange")

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; when a new window class name or title is available.
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  (add-hook 'exwm-update-class-hook #'exwm-x-rename-exwm-buffer)
  (add-hook 'exwm-update-title-hook #'exwm-x-rename-exwm-buffer)

  (defun exwm-x-rename-exwm-buffer ()
    (exwm-workspace-rename-buffer
     (concat "Exwm:" (exwm-x--get-prefer-name))))

  (defun exwm-x-suspend-computer ()
    (interactive)
    (exwm-x-run-shell-command "systemctl suspend"))

  (defun exwm-x-hibernate-computer ()
    (exwm-x-run-shell-command "systemctl hibernate"))

  (defun exwm-x-restart-computer ()
    (interactive)
    (exwm-x-run-shell-command "systemctl reboot"))

  (defun exwm-x-shutdown-commputer ()
    (interactive)
    (exwm-x-run-shell-command "systemctl poweroff"))

  (defun exwm-x-firefox ()
    (interactive)
    (exwm-x-jump-or-exec "Iceweasel" "iceweasel" "网"))

  (defun exwm-x-file-manager ()
    (interactive)
    (exwm-x-jump-or-exec "Nautilus" "nautilus --no-desktop" "文"))

  (defun exwm-x-crossover ()
    (interactive)
    (exwm-x-jump-or-exec "Crossover" "/opt/cxoffice/bin/crossover"))

  (defun exwm-x-launch-crossover-app (app bottle &optional shortcut-name)
    (exwm-x-jump-or-exec
     app
     (format "/opt/cxoffice/bin/wine --bottle %s --cx-app '%s'" bottle app)
     shortcut-name))

  (defun exwm-x-qq ()
    (interactive)
    (exwm-x-launch-crossover-app "TM.exe" "腾讯_TM_2013" "TM"))

  (defun exwm-x-word ()
    (interactive)
    (exwm-x-launch-crossover-app "WINWORD.EXE" "Microsoft_Office_2007" "Words"))

  (defun exwm-x-excel ()
    (interactive)
    (exwm-x-launch-crossover-app "EXCEL.EXE" "Microsoft_Office_2007" "Excel"))

  (defun exwm-x-ppt ()
    (interactive)
    (exwm-x-launch-crossover-app "POWERPNT.EXE" "Microsoft_Office_2007" "PPT"))

  (defun exwm-x-winxp ()
    (interactive)
    (exwm-x-jump-or-exec "VirtualBox" "VBoxManage startvm winxp" "Winxp"))

  (defun exwm-x-virtualbox ()
    (interactive)
    (exwm-x-jump-or-exec "VirtualBox" "virtualbox" "VBox"))

  (defun exwm-x-mplayer ()
    (interactive)
    (exwm-x-jump-or-exec "Smplayer" "smplayer" "Mplayer"))

  (defun exwm-x-htop ()
    (interactive)
    (exwm-x-jump-or-exec "htop" "xfce4-terminal -T htop -e htop" "Top"))

  (defun exwm-x-terminal ()
    (interactive)
    (exwm-x-jump-or-exec "default-terminal" "xfce4-terminal -T default-terminal" "终"))

  (defun exwm-x-new-terminal ()
    (interactive)
    (exwm-x-run-shell-command "xfce4-terminal"))

  (defun exwm-x-power-manager-settings ()
    (interactive)
    (exwm-x-run-shell-command "xfce4-power-manager-settings"))

  (defun exwm-x-power-manager ()
    (interactive)
    (exwm-x-run-shell-command "xfce4-power-manager"))

  (defun exwm-x-xset-bell-off ()
    (interactive)
    (exwm-x-run-shell-command "xset b off"))

  (defun exwm-x-xmodmap ()
    (interactive)
    (exwm-x-run-shell-command "xmodmap -e 'keycode 135 = Super_R'"))

  (defun exwm-x-network-manager-applet ()
    (interactive)
    (exwm-x-run-shell-command "nm-applet"))

  (defun exwm-x-volit ()
    (interactive)
    (exwm-x-run-shell-command "volti"))

  (defun exwm-x-xscreensaver ()
    (interactive)
    (exwm-x-run-shell-command "xscreensaver -no-splash"))

  (defun exwm-x-lock-screen ()
    (interactive)
    (exwm-x-run-shell-command "exec xscreensaver-command -lock"))

  (defun exwm-x-switch-to-1-workspace ()
    (interactive)
    (exwm-workspace-switch 0))

  (defun exwm-x-switch-to-2-workspace ()
    (interactive)
    (exwm-workspace-switch 1))

  (defun exwm-x-switch-to-3-workspace ()
    (interactive)
    (exwm-workspace-switch 2))

  (defun exwm-x-switch-to-4-workspace ()
    (interactive)
    (exwm-workspace-switch 3))

  ;; Don't Delete the below two lines
  (global-unset-key (kbd "C-t"))
  (push ?\C-t exwm-input-prefix-keys)

  (exwm-input-set-key (kbd "C-t R")  nil)
  (exwm-input-set-key (kbd "C-t q")  nil)
  (exwm-input-set-key (kbd "C-t m")  nil)
  (exwm-input-set-key (kbd "C-t v")  'exwm-x-file-manager)
  (exwm-input-set-key (kbd "C-t c")  'exwm-x-terminal)
  (exwm-input-set-key (kbd "C-t ff") 'exwm-x-firefox)
  (exwm-input-set-key (kbd "C-t fq") 'exwm-x-qq)
  (exwm-input-set-key (kbd "C-t fj") 'exwm-x-jabref)
  (exwm-input-set-key (kbd "C-t fc") 'exwm-x-cajviewer)
  (exwm-input-set-key (kbd "C-t fp") 'exwm-x-pdfreader)
  (exwm-input-set-key (kbd "C-t fw") 'exwm-x-winxp)
  (exwm-input-set-key (kbd "C-t w")  'exwm-workspace-switch)
  (exwm-input-set-key (kbd "C-t x")  'exwm-x-terminal)
  (exwm-input-set-key (kbd "C-t C-x")  'exwm-x-new-terminal)
  (exwm-input-set-key (kbd "C-t c")  'exwm-x-run-shell-command-interactively)

  (exwm-input-set-key (kbd "C-t 1")  'exwm-x-switch-to-1-workspace)
  (exwm-input-set-key (kbd "C-t 2")  'exwm-x-switch-to-2-workspace)
  (exwm-input-set-key (kbd "C-t 3")  'exwm-x-switch-to-3-workspace)
  (exwm-input-set-key (kbd "C-t 4")  'exwm-x-switch-to-4-workspace)

  (exwm-input-set-key (kbd "C-S-<up>") 'exwm-x-move-border-up)
  (exwm-input-set-key (kbd "C-S-<down>") 'exwm-x-move-border-down)
  (exwm-input-set-key (kbd "C-S-<left>") 'exwm-x-move-border-left)
  (exwm-input-set-key (kbd "C-S-<right>") 'exwm-x-move-border-right)


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

  ;; Debian menu
  (exwm-x-generate-debian-menus)

  ;; Don't delete it
  (exwm-enable)

  (use-package windmove
    :ensure nil
    :config
    (exwm-input-set-key (kbd "C-<up>") 'windmove-up)
    (exwm-input-set-key (kbd "C-<down>") 'windmove-down)
    (exwm-input-set-key (kbd "C-<left>") 'windmove-left)
    (exwm-input-set-key (kbd "C-<right>") 'windmove-right))

  (use-package start-menu
    :ensure nil
    :config
    (start-menu-enable)
    (exwm-input-set-key (kbd "C-t ,")  'start-menu-popup))

  (use-package dmenu
    :ensure nil
    :config
    (setq dmenu-prompt-string "dmenu: ")
    (exwm-input-set-key (kbd "C-t c") 'dmenu))

  (use-package switch-window
    :bind (("C-x o" . switch-window)
           ("C-x 1" . switch-window-then-maximize)
           ("C-x 2" . switch-window-then-split-below)
           ("C-x 3" . switch-window-then-split-right)
           ("C-x 0" . switch-window-then-delete))
    :config
    (setq switch-window-increase 8)
    (setq switch-window-shortcut-style 'qwerty))

  (use-package exwm-systemtray
    :ensure nil
    :config
    (setq exwm-systemtray-height 16)
    (exwm-systemtray-enable)
    (add-hook 'exwm-init-hook 'exwm-x-network-manager-applet t)
    (add-hook 'exwm-init-hook 'exwm-x-volit t)
    (add-hook 'exwm-init-hook 'exwm-x-power-manager t)
    (add-hook 'exwm-init-hook 'exwm-x-xscreensaver t)
    (add-hook 'exwm-init-hook 'exwm-x-xset-bell-off t)
    (add-hook 'exwm-init-hook 'exwm-x-xmodmap t))

  (use-package exim
    :ensure nil
    :config (add-hook 'exwm-init-hook 'exim-start)))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-example)

;;; exwm-x-example.el ends here
;; #+END_SRC
