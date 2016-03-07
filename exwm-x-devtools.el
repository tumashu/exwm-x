;;; exwm-x-devtools.el --- Tools for exwm-x developers

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/exwm-x
;; Version: 0.0.1

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
;; This file include org-webpage configure for exwm-x project.

;;; Code:

;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'org-webpage)
(require 'owp-web-server)
(require 'owp-lentic)
(require 'easy-lentic)

(defvar exwm-x-repository-directory
  "~/project/emacs-packages/exwm-x/")

(owp/add-project-config
 '("exwm-x"
   :repository-directory (:eval exwm-x-repository-directory)
   :remote (git "https://github.com/tumashu/exwm-x.git" "gh-pages")
   :site-domain "http://tumashu.github.com/exwm-x"
   :site-main-title "Exwm-X"
   :site-sub-title "(Addition useful tools for Exwm)"
   :default-category "documents"
   :theme (worg killjs)
   :force-absolute-url t
   :category-ignore-list ("themes" "assets" "upload-scripts" "snapshots")
   :source-browse-url ("GitHub" "https://github.com/tumashu/exwm-x")
   :personal-avatar nil
   :personal-duoshuo-shortname nil
   :preparation-function owp/lentic-preparation-function
   :org-export-function owp/lentic-org-export-function
   :lentic-doc-sources ("exwm-x-.*\\.el$")
   :lentic-readme-sources ("exwm-x.el")
   :lentic-index-sources ("exwm-x.el")
   :web-server-port 8765))
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'exwm-x-devtools)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exwm-x-devtools.el ends here
;; #+END_SRC
