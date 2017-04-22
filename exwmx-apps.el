;;; exwmx-apps.el --- Emacs commands used by Exwm-X

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

;;; Code:

;; * Code                                                                 :code:
(require 'exwm)
(require 'exwmx-core)
(require 'exwmx-utils)
(require 'exwmx-dmenu)

(defun exwmx:thunar ()
  (interactive)
  (exwmx-jump-or-exec "thunar"))

(defun exwmx:icecat ()
  (interactive)
  (exwmx-jump-or-exec "icecat"))

(defun exwmx:firefox ()
  (interactive)
  (exwmx-jump-or-exec "firefox"))

(defun exwmx:xfce4-terminal ()
  (interactive)
  (exwmx-jump-or-exec "xfce4-terminal -T default-terminal"
                      "default-terminal"))

(defun exwmx:xfce4-new-terminal ()
  (interactive)
  (exwmx-shell-command "xfce4-terminal"))

;; * Footer
(provide 'exwmx-apps)

;;; exwmx-apps.el ends here
