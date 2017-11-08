;;; eaws.el --- A Git porcelain inside Emacs  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017  The EAWS Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.

;; Author: Ramz Sivagurunathan <ramzthecoder@gmail.com>
;; Maintainer: Ramz Sivagurunathan <ramzthecoder@gmail.com>

;; Package-Requires: ((emacs "24.4") (async "20170823") (dash "20170810") (with-editor "20170817"))
;; Keywords: aws
;; Homepage: https://github.com/ageekymonk/eaws

;; Eaws is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Eaws is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Eaws.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Eaws is an console to manage aws resources.

;;; Code:

(defcustom eaws-buffer-name-format "*%p:%m*"
  "The format string used to name Eaws buffers.

The following %-sequences are supported:
`%p' The aws profile name
`%m' The name of the major-mode, but with the `-mode' suffix
     removed."
  :group 'eaws-buffers
  :type 'string)

;; Functions

(defun eaws-generate-buffer-name-default-function (mode &optional value)
  "Generate buffer name for a MODE buffer.
The returned name is based on `magit-buffer-name-format' and
takes `magit-uniquify-buffer-names' and VALUE, if non-nil, into
account."
  (let ((m (substring (symbol-name mode) 0 -5))
        (p eaws-config-profile)
        )
    (format-spec
     eaws-buffer-name-format
     `((?m . ,m)
       (?p . ,p)
       ))))
