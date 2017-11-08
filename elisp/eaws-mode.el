;;; eaws-mode.el --- Emacs AWS Console  -*- lexical-binding: t; coding: utf-8 -*-

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

;; keymap for eaws-mode
(defvar eaws-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map   [return]  'eaws-visit-thing)
    map)
  "Parent keymap for all keymaps of modes derived from `eaws-mode'.")

;; Mode
(define-derived-mode eaws-mode special-mode "Eaws"
  "Parent Major Mode from which all Eaws major mode inherit"
  :group 'eaws-modes
  )

;; Functions
(defun eaws-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (message "%s" major-mode)
    (user-error "There is no thing at point that could be visited"))

(defun eaws-mode-get-buffer (mode &optional create value)
      (or (--first (with-current-buffer it
                     (and (eq major-mode mode)))
                     (buffer-list))
          (if create
                 (eaws-generate-new-buffer mode value))))

(defun eaws-generate-new-buffer (mode &optional value)
  (let* ((buffer-name (eaws-generate-buffer-name-default-function mode value))
         (buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (funcall mode))
    buffer)
  )

(defun eaws-refresh-buffer ()
  "Refresh the current eaws buffer"
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5)))))
    (when (functionp refresh)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply refresh nil))
      )))

;;;###autoload
(defun eaws-mode-setup (mode &rest args)
  "Setup Eaws Mode buffer"
  (eaws-mode-setup-internal mode args))

(defun eaws-mode-setup-internal (mode args &optional value)
  (let ((buffer (eaws-mode-get-buffer mode t))
        )
    (with-current-buffer buffer
      (funcall mode))

    (with-current-buffer buffer
      (eaws-refresh-buffer))
    ))

(provide 'eaws-mode)
