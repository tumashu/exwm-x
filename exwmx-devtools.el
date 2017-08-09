;;; exwmx-devtools.el --- Tools for exwmx developers

;; * Header
;; Copyright 2016-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 1.0

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
;; This file include org-webpage configure for exwmx project.

;;; Code:

;; * Code                                                                 :code:
(require 'org2web)

(defvar exwmx-repository-directory
  "~/project/emacs-packages/exwm-x/")

(org2web-add-project
 '("EXWM-X"
   :repository-directory (:eval exwmx-repository-directory)
   :remote (git "https://github.com/tumashu/exwm-x.git" "gh-pages")
   :site-domain "http://tumashu.github.com/exwm-x"
   :site-main-title "EXWM-X"
   :site-sub-title "(Addition useful tools for EXWM)"
   :default-category "documents"
   :theme (worg killjs)
   :force-absolute-url t
   :category-ignore-list ("themes" "assets" "upload-scripts" "snapshots")
   :source-browse-url ("GitHub" "https://github.com/tumashu/exwm-x")
   :personal-avatar nil
   :personal-duoshuo-shortname nil
   :preparation-function org2web-el2org-preparation-function
   :org-export-function org2web-el2org-org-export-function
   :el2org-doc-sources ("exwm-x-.*\\.el$")
   :el2org-readme-sources ("exwm-x.el")
   :el2org-index-sources ("exwm-x.el")
   :web-server-port 8765))

;; * Footer
(provide 'exwmx-devtools)

;; Local Variables:
;; no-byte-compile: t
;; End:
