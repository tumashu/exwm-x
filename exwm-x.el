;;; exwm-x.el --- Addition tools for exwm (emacs x window manager)

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5")(exwm "0.1")(switch-window "0.10")(dmenu "0.1")(start-menu "0.1"))
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

;; * exwm-x manual                                                       :README:doc:

;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'exwm-x-core)
(require 'exwm-x-modeline)
(require 'exwm-x-utils)
(require 'exwm-x-mouse)
(require 'exwm-x-move-border)
(require 'exwm-x-debian)
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x)

;;; exwm-x.el ends here
;; #+END_SRC
