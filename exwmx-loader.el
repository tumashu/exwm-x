;;; exwmx-loader.el --- An exwm loader used in file ~/.xsession or ~/.xinitrc

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.3
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

;;; Code:

;; * Code                                                                 :code:

(cond ((file-exists-p "~/.exwm")
       (load-file "~/.exwm"))
      ((not (featurep 'exwm))
       (require 'exwm)
       (require 'exwm-config)
       (exwm-config-default)
       (message "exwm configuration not found. Falling back to default configuration...")))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exwmx-loader.el ends here
