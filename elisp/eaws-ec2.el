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
    (define-key map   [return]  'eaws-ec2-dispatch-popup)
    map)
  "Parent keymap for all keymaps of modes derived from `eaws-mode'.")

(defvar eaws-ec2-instance-status-section-map
  (let ((map (make-sparse-keymap)))
    (progn
      (define-key map [return] 'eaws-show-instance)
      (define-key map "?" 'eaws-ec2-dispatch-popup))
    map)
  "Keymap for the `ec2-instance-status' section.")

(define-derived-mode eaws-ec2-mode eaws-mode "Eaws"
  "Eaws EC2 Mode"
  )

;; hooks
(defcustom eaws-ec2-mode-hook nil
  "Eaws EC2 Mode hook"
  :group 'eaws-modes
  :type 'hook)

(defcustom eaws-ec2-status-section-hook
  '(eaws-ec2-insert-instance-status)
  "Eaws EC2 Status Hook"
  :group 'eaws-ec2
  :type 'hook
  )

;; custom

;; Popup
(eaws-define-popup eaws-ec2-dispatch-popup
  "Popup console for ec2"
  :actions '("EC2 Instance Commands"
             (?s "Start" eaws-ec2-start-instance)
             (?S "Stop" eaws-ec2-stop-instance)
             (?T "Terminate" eaws-ec2-terminate-instance)
             "Status Commands"
             (?g  "    refresh current buffer"   eaws-refresh)
             ;; This binding has no effect and only appears to do
             ;; so because it is identical to the global binding.
             ("C-h m" "show all key bindings"    describe-mode))
  :setup-function 'eaws-dispatch-popup-setup
  :max-action-columns (lambda (heading)
                        (pcase heading
                          ("EC2 Status Commands" 4)
                          ("Essential commands" 1))))

(defvar eaws-dispatch-popup-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map eaws-popup-mode-map)
    (cond ((featurep 'jkl)
           (define-key map [tab]    'eaws-invoke-popup-action)
           (define-key map [return] 'eaws-invoke-popup-action))
          (t
           (define-key map (kbd "C-i") 'eaws-invoke-popup-action)
           (define-key map (kbd "C-m") 'eaws-invoke-popup-action)))
    map)
  "Keymap used by `eaws-dispatch-popup'.")

(defun eaws-dispatch-popup-setup (val def)
  (eaws-popup-default-setup val def)
  (use-local-map eaws-dispatch-popup-map)
  ;; This is necessary for users (i.e. me) who have broken the
  ;; connection between C-i (aka TAB) and tab, and C-m (aka RET)
  ;; and return.
  (eaws-popup-put
   :actions (nconc (eaws-popup-get :actions)
                   (list (make-eaws-popup-event :key 'tab
                                                 :fun 'eaws-section-toggle)
                         (make-eaws-popup-event :key 'return
                                                 :fun 'eaws-visit-thing)))))
;; Structs
(cl-defstruct eaws-ec2-instance
  id name state type private-ip
  subnet-id vpc-id image-id security-groups
  launch-time)

;; EC2 Commands
(defvar eaws-ec2-get-instances-command
  (format "aws ec2 describe-instances --profile %s" eaws-profile)
  "Command to list instances.")

(defvar eaws-ec2-get-images-command
  (format "aws ec2 describe-images --profile %s" eaws-profile)
  "Command to list images.")

(defun aws-ec2-get-instances-parser (input)
  "Extract instances from describe-instances.
Argument INPUT json input in string form."
  (let* ((json-object-type 'plist)
         (aws-json         (json-read-from-string input))
         (reservations     (plist-get aws-json :Reservations))
         (instances        (mapcar (lambda (el) (plist-get el :Instances)) reservations))
         (instance-list    (mapcar (lambda (x) (elt x 0)) instances)))
    (--map (let*
               (
                 (tags (plist-get it :Tags))
                 (nameTag (cl-remove-if-not #'(lambda (tag) (string= (plist-get tag :Key) "Name")) tags))
                 (name (if (= (length nameTag) 1) (plist-get (elt nameTag 0) :Value) ""))
                 (parsed-instance (make-eaws-ec2-instance
                                   :id (plist-get it :InstanceId)
                                   :name name
                                   :state (plist-get (plist-get it :State) :Name)
                                   :type (plist-get it :InstanceType)
                                   :image-id (plist-get it :ImageId)
                                   :private-ip (plist-get it :PrivateIpAddress)
                                   :vpc-id (plist-get it :VpcId)
                                   :subnet-id (plist-get it :SubnetId)
                                   :launch-time (plist-get it :LaunchTime)
                                   )))
             parsed-instance
             )
           instance-list)))

(defun eaws-ec2-insert-instance-status ()
  (let* ((output (eaws-run-command eaws-ec2-get-instances-command))
         (instance-list (aws-ec2-get-instances-parser output)))
    (progn
    (--map (eaws-insert-section (ec2-instance-status (eaws-ec2-instance-id it))
             (insert (concat (format "%-40s" (s-truncate 30 (eaws-ec2-instance-name it)))
                             (format "%-10s" (eaws-ec2-instance-state it))
                             (format "%25s\n" (eaws-ec2-instance-id it)))))
           instance-list))
    )
  )

(defun eaws-ec2-refresh-buffer ()
  (eaws-insert-section (ec2-status)
    (eaws-run-section-hook 'eaws-ec2-status-section-hook)
    )
  )

(defun eaws-ec2-status (&optional value)
  "Show the status of the ec2 inst"
  (interactive)
   (progn (eaws-mode-setup #'eaws-ec2-mode)))

(provide 'eaws-ec2)
