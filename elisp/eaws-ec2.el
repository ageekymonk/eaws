;;; eaws-ec2.el --- Emacs AWS Console  -*- lexical-binding: t; coding: utf-8 -*-

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

(require 'json)
(require 'cl-lib)
(require 's)

(require 'eaws-config)

;; mode
(defvar eaws-ec2-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map   [return]  'eaws-visit-thing)
    map)
  "Parent keymap for all keymaps of modes derived from `eaws-mode'.")

(define-derived-mode eaws-ec2-mode eaws-mode "Eaws"
  "Eaws EC2 Mode"
  )

;; hooks
(defcustom eaws-ec2-mode-hook nil
  "Eaws EC2 Mode hook"
  :group 'eaws-modes
  :type 'hook)

(defcustom eaws-ec2-status-section-hook
  '(eaws-ec2-insert-status)
  "Eaws EC2 Status Hook"
  :group 'eaws-status
  :type 'hook
  )

;; Command
(defvar eaws-list-ec2-command
  (format "aws ec2 describe-instances --profile %s" eaws-profile)
  "Command to list instances.  Run `aws configure` to set up AWS cli.")

(defun eaws-run-ec2-command ()
  "Return the full json from running describe-instances."
    (with-temp-buffer
      (shell-command eaws-list-ec2-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max))))

(defun aws-parse-server-list (input)
  "Extract instances from describe-instances.
Argument INPUT json input in string form."
  (let* ((json-object-type 'plist)
         (aws-json         (json-read-from-string input))
         (reservations     (plist-get aws-json :Reservations))
         (instances        (mapcar (lambda (el) (plist-get el :Instances)) reservations))
         (instance-list    (mapcar (lambda (x) (elt x 0)) instances)))
    instance-list))

(defun eaws-ec2-insert-instance-status (instance)
  "Constructs a human-friendly string of a server instance.
show: <name>, <IP> and <launch date>.
Argument INSTANCE is the aws json in plist form"
  (let* ((ip (plist-get instance :PrivateIpAddress))
         (instance-id (plist-get instance :InstanceId))
         (instance-state (plist-get (plist-get instance :State) :Name))
         (tags (plist-get instance :Tags))
         (nameTag (cl-remove-if-not #'(lambda (tag) (string= (plist-get tag :Key) "Name")) tags))
         (name (if (= (length nameTag) 1) (plist-get (elt nameTag 0) :Value) instance-id))
         (launch-time (plist-get instance :LaunchTime))
         (launch-date (car (split-string launch-time "T")))
         (formatted-string
          (concat (format "%-40s" (s-truncate 30 name))
                  (format "%-10s" instance-state)
                  (format "%15s\n" ip)
                  )))
    (cons formatted-string instance)))

(defun eaws-ec2-insert-status ()
  "Insert ec2 instance status"
  (--map (insert (car (eaws-ec2-insert-instance-status it)))(aws-parse-server-list (eaws-run-ec2-command)) )
    )

(defun eaws-ec2-refresh-buffer ()
  (eaws-insert-section (ec2-status)
  (eaws-run-section-hook 'eaws-ec2-status-section-hook))
  )

;;; autoload
(defun eaws-ec2-status (&optional value)
  "Show the status of the ec2 instances"
  (interactive)
   (progn (eaws-mode-setup #'eaws-ec2-mode)))

(provide 'eaws-ec2)
