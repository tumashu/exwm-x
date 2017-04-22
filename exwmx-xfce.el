;;; exwmx-xfce.el --- Let Exwm-X work with xfce

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwmx
;; Version: 0.3
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

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)
(require 'exwmx-core)
(require 'exwmx-dmenu)

(defvar exwmx-terminal-emulator nil)
(setq exwmx-terminal-emulator "xfce4-terminal")

(defun exwmx:startxfce4 ()
  (interactive)
  (message "Exwm-X: starting Xfce ...")
  (exwmx-shell-command "startxfce4"))

(add-hook 'exwm-init-hook #'exwmx:startxfce4)

;; * Footer
(provide 'exwmx-xfce)

;;; exwmx-xfce.el ends here
