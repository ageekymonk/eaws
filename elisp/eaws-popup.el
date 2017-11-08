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

;; This library implements a generic interface for toggling switches
;; and setting options and then invoking an Emacs command which does
;; something with these arguments.  The prototypical use is for the
;; command to call an external process, passing on the arguments as
;; command line arguments.  But this is only one of many possible
;; uses (though the one this library is optimized for).

;; With the Emacs concept of "prefix arguments" in mind this could be
;; described as "infix arguments with feedback in a buffer".

;; Commands that set the prefix argument for the subsequent command do
;; not limit what that next command could be.  But entering a command
;; console popup does limit the selection to the commands defined for
;; that popup, and so we use the term "infix" instead of "prefix".

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'dash)
(require 'format-spec)
(eval-when-compile (require 'subr-x))

(and (require 'async-bytecomp nil t)
     (cl-intersection '(all eaws)
                      (bound-and-true-p async-bytecomp-allowed-packages))
     (fboundp 'async-bytecomp-package-mode)
     (async-bytecomp-package-mode 1))

(declare-function info 'info)
(declare-function Man-find-section 'man)
(declare-function Man-next-section 'man)

;; For the `:variable' event type.
(declare-function eaws-git-string 'eaws-git)
(declare-function eaws-refresh 'eaws-mode)
(declare-function eaws-get 'eaws-git)
(declare-function eaws-set 'eaws-git)

;; For branch actions.
(declare-function eaws-branch-set-face 'eaws-git)

;;; Settings
;;;; Custom Groups

(defgroup eaws-popup nil
  "Infix arguments with a popup as feedback."
  :link '(info-link "(eaws-popup)")
  :group 'bindings)

(defgroup eaws-popup-faces nil
  "Faces used by Eaws-Popup."
  :group 'eaws-popup)

;;;; Custom Options

(defcustom eaws-popup-display-buffer-action '((display-buffer-below-selected))
  "The action used to display a popup buffer.

Popup buffers are displayed using `display-buffer' with the value
of this option as ACTION argument.  You can also set this to nil
and instead add an entry to `display-buffer-alist'."
  :package-version '(eaws-popup . "2.4.0")
  :group 'eaws-popup
  :type 'sexp)

(defcustom eaws-popup-manpage-package
  (if (memq system-type '(windows-nt ms-dos)) 'woman 'man)
  "The package used to display manpages.
One of `man' or `woman'."
  :group 'eaws-popup
  :type '(choice (const man) (const woman)))

(defcustom eaws-popup-show-help-echo t
  "Show usage information in the echo area."
  :group 'eaws-popup
  :type 'boolean)

(defcustom eaws-popup-show-common-commands nil
  "Whether to initially show section with commands common to all popups.
This section can also be toggled temporarily using \
\\<eaws-popup-mode-map>\\[eaws-popup-toggle-show-common-commands]."
  :package-version '(eaws-popup . "2.9.0")
  :group 'eaws-popup
  :type 'boolean)

(defcustom eaws-popup-use-prefix-argument 'default
  "Control how prefix arguments affect infix argument popups.

This option controls the effect that the use of a prefix argument
before entering a popup has.

`default'  With a prefix argument directly invoke the popup's
           default action (an Emacs command), instead of bringing
           up the popup.

`popup'    With a prefix argument bring up the popup, otherwise
           directly invoke the popup's default action.

`nil'      Ignore prefix arguments."
  :group 'eaws-popup
  :type '(choice
          (const :tag "Call default action instead of showing popup" default)
          (const :tag "Show popup instead of calling default action" popup)
          (const :tag "Ignore prefix argument" nil)))

;;;; Custom Faces

(defface eaws-popup-heading
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'eaws-popup-faces)

(defface eaws-popup-key
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'eaws-popup-faces)

(defface eaws-popup-argument
  '((t :inherit font-lock-warning-face))
  "Face used to display enabled arguments in popups."
  :group 'eaws-popup-faces)

(defface eaws-popup-disabled-argument
  '((t :inherit shadow))
  "Face used to display disabled arguments in popups."
  :group 'eaws-popup-faces)

(defface eaws-popup-option-value
  '((t :inherit font-lock-string-face))
  "Face used to display option values in popups."
  :group 'eaws-popup-faces)

;;;; Keymap

(defvar eaws-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'eaws-invoke-popup-action)
    (define-key map (kbd "- <t>")               'eaws-invoke-popup-switch)
    (define-key map (kbd "= <t>")               'eaws-invoke-popup-option)
    (define-key map (kbd "C-g")     'eaws-popup-quit)
    (define-key map (kbd "?")       'eaws-popup-help)
    (define-key map (kbd "C-h k")   'eaws-popup-help)
    (define-key map (kbd "C-h i")   'eaws-popup-info)
    (define-key map (kbd "C-t")     'eaws-popup-toggle-show-common-commands)
    (define-key map (kbd "C-c C-c") 'eaws-popup-set-default-arguments)
    (define-key map (kbd "C-x C-s") 'eaws-popup-save-default-arguments)
    (cond ((featurep 'jkl)
           (define-key map (kbd "C-p") 'universal-argument)
           (define-key map [return]    'push-button)
           (define-key map (kbd "C-i") 'backward-button)
           (define-key map (kbd "C-k") 'forward-button))
          (t
           (define-key map (kbd "C-m") 'push-button)
           (define-key map (kbd "DEL") 'backward-button)
           (define-key map (kbd "C-p") 'backward-button)
           (define-key map (kbd "C-i") 'forward-button)
           (define-key map (kbd "C-n") 'forward-button)))
    map)
  "Keymap for `eaws-popup-mode'.

\\<eaws-popup-mode-map>\
This keymap contains bindings common to all popups.  A section
listing these commands can be shown or hidden using \
\\[eaws-popup-toggle-show-common-commands].

The prefix used to toggle any switch can be changed by binding
another key to `eaws-invoke-popup-switch'.  Likewise binding
another key to `eaws-invoke-popup-option' changes the prefixed
used to set any option.  The two prefixes have to be different.
If you change these bindings, you should also change the `prefix'
property of the button types `eaws-popup-switch-button' and
`eaws-popup-option-button'.

If you change any other binding, then you might have to also edit
`eaws-popup-common-commands' for things to align correctly in
the section listing these commands.

Never bind an alphabetic character in this keymap or you might
make it impossible to invoke certain actions.")

(defvar eaws-popup-common-commands
  '(("Set defaults"          eaws-popup-set-default-arguments)
    ("View popup manual"     eaws-popup-info)
    ("Toggle this section"   eaws-popup-toggle-show-common-commands)
    ("Save defaults"         eaws-popup-save-default-arguments)
    ("    Popup help prefix" eaws-popup-help)
    ("Abort"                 eaws-popup-quit)))

;;;; Buttons

(define-button-type 'eaws-popup-button
  'face nil
  'action (lambda (button)
            (funcall (button-get button 'function)
                     (button-get button 'event))))

(define-button-type 'eaws-popup-switch-button
  'supertype 'eaws-popup-button
  'function  'eaws-invoke-popup-switch
  'property  :switches
  'heading   "Switches\n"
  'formatter 'eaws-popup-format-argument-button
  'format    " %k %d (%a)"
  'prefix    ?-
  'maxcols   1)

(define-button-type 'eaws-popup-option-button
  'supertype 'eaws-popup-button
  'function  'eaws-invoke-popup-option
  'property  :options
  'heading   "Options\n"
  'formatter 'eaws-popup-format-argument-button
  'format    " %k %d (%a%v)"
  'prefix    ?=
  'maxcols   1)

(define-button-type 'eaws-popup-variable-button
  'supertype 'eaws-popup-button
  'function  'eaws-invoke-popup-action
  'property  :variables
  'heading   "Variables\n"
  'formatter 'eaws-popup-format-variable-button
  'format    " %k %d"
  'prefix    nil
  'maxcols   1)

(define-button-type 'eaws-popup-action-button
  'supertype 'eaws-popup-button
  'function  'eaws-invoke-popup-action
  'property  :actions
  'heading   "Actions\n"
  'formatter 'eaws-popup-format-action-button
  'format    " %k %d"
  'prefix    nil
  'maxcols   :max-action-columns)

(define-button-type 'eaws-popup-command-button
  'supertype 'eaws-popup-action-button
  'formatter 'eaws-popup-format-command-button
  'action    (lambda (button)
               (let ((command (button-get button 'function)))
                 (unless (eq command 'push-button)
                   (call-interactively command)))))

(define-button-type 'eaws-popup-internal-command-button
  'supertype 'eaws-popup-command-button
  'heading   "Common Commands\n"
  'maxcols   3)

;;; Events

(defvar-local eaws-this-popup nil
  "The popup which is currently active.
This is intended for internal use only.
Don't confuse this with `eaws-current-popup'.")

(defvar-local eaws-this-popup-events nil
  "The events known to the active popup.
This is intended for internal use only.
Don't confuse this with `eaws-current-popup-args'.")

(defvar-local eaws-previous-popup nil)

(defun eaws-popup-get (prop)
  "While a popup is active, get the value of PROP."
  (if (memq prop '(:switches :options :variables :actions))
      (plist-get eaws-this-popup-events prop)
    (plist-get (symbol-value eaws-this-popup) prop)))

(defun eaws-popup-put (prop val)
  "While a popup is active, set the value of PROP to VAL."
  (if (memq prop '(:switches :options :variables :actions))
      (setq eaws-this-popup-events
            (plist-put eaws-this-popup-events prop val))
    (error "Property %s isn't supported" prop)))

(defvar eaws-current-popup nil
  "The popup from which this editing command was invoked.

Use this inside the `interactive' form of a popup aware command
to determine whether it was invoked from a popup and if so from
which popup.  If the current command was invoked without the use
of a popup, then this is nil.")

(defvar eaws-current-popup-action nil
  "The popup action now being executed.")

(defvar eaws-current-popup-args nil
  "The value of the popup arguments for this editing command.

If the current command was invoked from a popup, then this is
a list of strings of all the set switches and options.  This
includes arguments which are set by default not only those
explicitly set during this invocation.

When the value is nil, then that can be because no argument is
set, or because the current command wasn't invoked from a popup;
consult `eaws-current-popup' to tell the difference.

Generally it is better to use `NAME-arguments', which is created
by `eaws-define-popup', instead of this variable or the function
by the same name, because `NAME-argument' uses the default value
for the arguments when the editing command is invoked directly
instead of from a popup.  When the command is bound in several
popups that might not be feasible though.")

(defun eaws-current-popup-args (&rest filter)
  "Return the value of the popup arguments for this editing command.

The value is the same as that of the variable by the same name
\(which see), except that FILTER is applied.  FILTER is a list
of regexps; only arguments that match one of them are returned.
The first element of FILTER may also be `:not' in which case
only arguments that don't match any of the regexps are returned,
or `:only' which doesn't change the behaviour."
  (let ((-compare-fn (lambda (a b) (eaws-popup-arg-match b a))))
    (-filter (if (eq (car filter) :not)
                 (lambda (arg) (not (-contains-p (cdr filter) arg)))
               (when (eq (car filter) :only)
                 (pop filter))
               (lambda (arg) (-contains-p filter arg)))
             eaws-current-popup-args)))

(defun eaws-popup-arg-match (pattern string)
  (if (or (string-match-p "=$" pattern)
          (string-match-p "^-[A-Z]$" pattern))
      (string-match (format "^%s\\(.*\\)$" pattern) string)
    (string-equal string pattern)))

(cl-defstruct eaws-popup-event key dsc arg fun use val)

(defun eaws-popup-event-keydsc (ev)
  (let ((key (eaws-popup-event-key ev)))
    (key-description (if (vectorp key) key (vector key)))))

(defun eaws-popup-lookup (event type)
  (--first (equal (eaws-popup-event-key it) event)
           (-filter 'eaws-popup-event-p (eaws-popup-get type))))

(defun eaws-popup-get-args ()
  (--mapcat (when (and (eaws-popup-event-p it)
                       (eaws-popup-event-use it))
              (list (format "%s%s"
                            (eaws-popup-event-arg it)
                            (or (eaws-popup-event-val it) ""))))
            (append (eaws-popup-get :switches)
                    (eaws-popup-get :options))))

(defmacro eaws-popup-convert-events (def form)
  (declare (indent 1) (debug (form form)))
  `(--map (if (or (null it) (stringp it) (functionp it)) it ,form) ,def))

(defun eaws-popup-convert-switches (val def)
  (eaws-popup-convert-events def
    (let ((a (nth 2 it)))
      (make-eaws-popup-event
       :key (car it) :dsc (cadr it) :arg a
       :use (and (member a val) t)
       ;; For arguments implemented in lisp, this function's
       ;; doc-string is used by `eaws-popup-help'.  That is
       ;; the only thing it is used for.
       :fun (and (string-prefix-p "\+\+" a) (nth 3 it))))))

(defun eaws-popup-convert-options (val def)
  (eaws-popup-convert-events def
    (let* ((a (nth 2 it))
           (r (format "^%s\\(.*\\)" a))
           (v (--first (string-match r it) val)))
      (make-eaws-popup-event
       :key (car it)  :dsc (cadr it) :arg a
       :use (and v t) :val (and v (match-string 1 v))
       :fun (or (nth 3 it) 'read-from-minibuffer)))))

(defun eaws-popup-convert-variables (_val def)
  (eaws-popup-convert-events def
    (make-eaws-popup-event
     :key (car it) :dsc (cadr it) :fun (nth 2 it) :arg (nth 3 it))))

(defun eaws-popup-convert-actions (_val def)
  (eaws-popup-convert-events def
    (make-eaws-popup-event
     :key (car it) :dsc (cadr it) :fun (nth 2 it))))

;;; Define

(defmacro eaws-define-popup (name doc &rest args)
  "Define a popup command named NAME.

NAME should begin with the package prefix and by convention end
with `-popup'.  That name is used for the actual command as well
as for a variable used internally.  DOC is used as the doc-string
of that command.

Also define an option and a function named `SHORTNAME-arguments',
where SHORTNAME is NAME with the trailing `-popup' removed.  The
name of this option and this function can be overwritten using
the optional argument OPTION, but that is rarely advisable. As a
special case if OPTION is specified but nil, do not define this
option and this function at all.

The option `SHORTNAME-arguments' holds the default value for the
popup arguments.  It can be customized from within the popup or
using the Custom interface.

The function `SHORTNAME-arguments' is a wrapper around the
variable `eaws-current-popup-args', both of which are intended
to be used inside the `interactive' form of commands commonly
invoked from the popup `NAME'.  When such a command is invoked
from that popup, then the function `SHORTNAME-arguments' returns
the value of the variable `eaws-current-popup-args'; however
when the command is invoked directly, then it returns the default
value of the variable `SHORTNAME-arguments'.

Optional argument GROUP specifies the Custom group into which the
option is placed.  If omitted, then the option is placed into some
group the same way it is done when directly using `defcustom' and
omitting the group, except when NAME begins with \"eaws-\", in
which case the group `eaws-git-arguments' is used.

Optional argument MODE is deprecated, instead use the keyword
arguments `:setup-function' and/or `:refresh-function'.  If MODE
is non-nil, then it specifies the mode used by the popup buffer,
instead of the default, which is `eaws-popup-mode'.

The remaining arguments should have the form

    [KEYWORD VALUE]...

The following keywords are meaningful (and by convention are
usually specified in that order):

`:actions'
  The actions which can be invoked from the popup.  VALUE is a
  list whose members have the form (KEY DESC COMMAND), see
  `eaws-define-popup-action' for details.

  Actions are regular Emacs commands, which usually have an
  `interactive' form setup to consume the values of the popup
  `:switches' and `:options' when invoked from the corresponding
  popup, else when invoked as the default action or directly
  without using the popup, the default value of the variable
  `SHORTNAME-arguments'.  This is usually done by calling the
  function `SHORTNAME-arguments'.

  Members of VALUE may also be strings, assuming the first member
  is also a string.  Instead of just one action section with the
  heading \"Actions\", multiple sections are then inserted into
  the popup buffer, using these strings as headings.

  Members of VALUE may also be nil.  This should only be used
  together with `:max-action-columns' and allows having gaps in
  the action grid, which can help arranging actions sensibly.

`:default-action'
  The default action of the popup which is used directly instead
  of displaying the popup buffer, when the popup is invoked with
  a prefix argument.  Also see `eaws-popup-use-prefix-argument'
  and `:use-prefix', which can be used to inverse the meaning of
  the prefix argument.

`:use-prefix'
  Controls when to display the popup buffer and when to invoke
  the default action (if any) directly.  This overrides the
  global default set using `eaws-popup-use-prefix-argument'.
  The value, if specified, should be one of `default' or `popup',
  or a function that is called with no arguments and returns one
  of these symbols.

`:max-action-columns'
  The maximum number of actions to display on a single line, a
  number or a function that returns a number and takes the name
  of the section currently being inserted as argument.  If there
  isn't enough room to display as many columns as specified here,
  then fewer are used.

`:switches'
  The popup arguments which can be toggled on and off.  VALUE
  is a list whose members have the form (KEY DESC SWITCH), see
  `eaws-define-popup-switch' for details.

`:options'
  The popup arguments which take a value, as in \"--opt=OPTVAL\".
  VALUE is a list whose members have the form (KEY DESC OPTION
  READER), see `eaws-define-popup-option' for details.

`:default-arguments'
  The default arguments, a list of switches (which are then
  enabled by default) and options with there default values, as
  in \"--OPT=OPTVAL\".

`:variables'

  Git variables which can be set from the popup.  VALUE is a list
  whose members have the form (KEY DESC COMMAND FORMATTER), see
  `eaws-define-popup-variable' for details.

`:sequence-predicate'
  When this function returns non-nil, then the popup uses
  `:sequence-actions' instead of `:actions', and does not show
  the `:switches' and `:options'.

`:sequence-actions'
  The actions which can be invoked from the popup, when
  `:sequence-predicate' returns non-nil.

`:setup-function'
  When this function is specified, then it is used instead of
  `eaws-popup-default-setup'.

`:refresh-function'
  When this function is specified, then it is used instead of
  calling `eaws-popup-insert-section' three times with symbols
  `eaws-popup-switch-button', `eaws-popup-option-button', and
  finally `eaws-popup-action-button' as argument.

`:man-page'
  The name of the manpage to be displayed when the user requests
  help for a switch or argument.

\(fn NAME DOC [GROUP [MODE [OPTION]]] :KEYWORD VALUE...)"
  (declare (indent defun) (doc-string 2))
  (let* ((str  (symbol-name name))
         (grp  (if (keywordp (car args))
                   (and (string-prefix-p "eaws-" str) ''eaws-git-arguments)
                 (pop args)))
         (mode (and (not (keywordp (car args))) (pop args)))
         (opt  (if (keywordp (car args))
                   (intern (concat (if (string-suffix-p "-popup" str)
                                       (substring str 0 -6)
                                     str)
                                   "-arguments"))
                 (eval (pop args)))))
    `(progn
       (defun ,name (&optional arg) ,doc
         (interactive "P")
         (eaws-invoke-popup ',name ,mode arg))
       (defvar ,name
         (list :variable ',opt ,@args))
       (eaws-define-popup-keys-deferred ',name)
       ,@(when opt
           `((defcustom ,opt (plist-get ,name :default-arguments)
               ""
               ,@(and grp (list :group grp))
               :type '(repeat (string :tag "Argument")))
             (defun ,opt ()
               (if (eq eaws-current-popup ',name)
                   eaws-current-popup-args
                 ,opt))
             (put ',opt 'definition-name ',name))))))

(defun eaws-define-popup-switch (popup key desc switch
                                        &optional enable at prepend)
  "In POPUP, define KEY as SWITCH.

POPUP is a popup command defined using `eaws-define-popup'.
SWITCH is a string representing an argument that takes no value.
KEY is a character representing the second event in the sequence
of keystrokes used to toggle the argument.  (The first event, the
prefix, is shared among all switches, defaults to -, and can be
changed in `eaws-popup-mode-keymap').

DESC is a string describing the purpose of the argument, it is
displayed in the popup.

If optional ENABLE is non-nil, then the switch is on by default.

SWITCH is inserted after all other switches already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another switch already defined for POPUP, the argument
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (eaws-define-popup-key popup :switches key
    (list desc switch enable) at prepend))

(defun eaws-define-popup-option (popup key desc option
                                        &optional reader value at prepend)
  "In POPUP, define KEY as OPTION.

POPUP is a popup command defined using `eaws-define-popup'.
OPTION is a string representing an argument that takes a value.
KEY is a character representing the second event in the sequence
of keystrokes used to set the argument's value.  (The first
event, the prefix, is shared among all options, defaults to =,
and can be changed in `eaws-popup-mode-keymap').

DESC is a string describing the purpose of the argument, it is
displayed in the popup.

If optional VALUE is non-nil then the option is on by default,
and VALUE is its default value.

READER is used to read a value from the user when the option is
invoked and does not currently have a value.  It should take one
argument and use it as the prompt.  If this is nil, then
`read-from-minibuffer' is used.

OPTION is inserted after all other options already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another option already defined for POPUP, the argument
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (eaws-define-popup-key popup :options key
    (list desc option reader value) at prepend))

(defun eaws-define-popup-variable (popup key desc command formatter
                                          &optional at prepend)
  "In POPUP, define KEY as COMMAND.

POPUP is a popup command defined using `eaws-define-popup'.
COMMAND is a command which calls `eaws-popup-set-variable'.
FORMATTER is a function which calls `eaws-popup-format-variable'.
These two functions have to be called with the same arguments.

KEY is a character representing the event used interactively call
the COMMAND.

DESC is the variable or a representation thereof.  It's not
actually used for anything.

COMMAND is inserted after all other commands already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another command already defined for POPUP, the command
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (eaws-define-popup-key popup :variables key
    (list desc command formatter) at prepend))

(defun eaws-define-popup-action (popup key desc command
                                        &optional at prepend)
  "In POPUP, define KEY as COMMAND.

POPUP is a popup command defined using `eaws-define-popup'.
COMMAND can be any command but should usually consume the popup
arguments in its `interactive' form.
KEY is a character representing the event used invoke the action,
i.e. to interactively call the COMMAND.

DESC is a string describing the purpose of the action, it is
displayed in the popup.

COMMAND is inserted after all other commands already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another command already defined for POPUP, the command
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (eaws-define-popup-key popup :actions key
    (list desc command) at prepend))

(defun eaws-define-popup-sequence-action
    (popup key desc command &optional at prepend)
  "Like `eaws-define-popup-action' but for `:sequence-action'."
  (declare (indent defun))
  (eaws-define-popup-key popup :sequence-actions key
    (list desc command) at prepend))

(defconst eaws-popup-type-plural-alist
  '((:switch . :switches)
    (:option . :options)
    (:variable . :variables)
    (:action . :actions)
    (:sequence-action . :sequence-actions)))

(defun eaws-popup-pluralize-type (type)
  (or (cdr (assq type eaws-popup-type-plural-alist))
      type))

(defun eaws-define-popup-key
    (popup type key def &optional at prepend)
  "In POPUP, define KEY as an action, switch, or option.
It's better to use one of the specialized functions
  `eaws-define-popup-action',
  `eaws-define-popup-sequence-action',
  `eaws-define-popup-switch',
  `eaws-define-popup-option', or
  `eaws-define-popup-variable'."
  (declare (indent defun))
  (setq type (eaws-popup-pluralize-type type))
  (if (memq type '(:switches :options :variables :actions :sequence-actions))
      (if (boundp popup)
          (let* ((plist (symbol-value popup))
                 (value (plist-get plist type))
                 (elt   (assoc key value)))
            (if elt
                (setcdr elt def)
              (setq elt (cons key def)))
            (if at
                (when (setq at (cl-member at value :key 'car-safe :test 'equal))
                  (setq value (cl-delete key value :key 'car-safe :test 'equal))
                  (if prepend
                      (progn (push (car at) (cdr at))
                             (setcar at elt))
                    (push elt (cdr at))))
              (setq value (cl-delete key value :key 'car-safe :test 'equal)))
            (unless (assoc key value)
              (setq value (if prepend
                              (cons elt value)
                            (append value (list elt)))))
            (set popup (plist-put plist type value)))
        (push (list type key def at prepend)
              (get popup 'eaws-popup-deferred)))
    (error "Unknown popup event type: %s" type)))

(defun eaws-define-popup-keys-deferred (popup)
  (dolist (args (get popup 'eaws-popup-deferred))
    (condition-case err
        (apply #'eaws-define-popup-key popup args)
      ((debug error)
       (display-warning 'eaws (error-message-string err) :error))))
  (put popup 'eaws-popup-deferred nil))

(defun eaws-change-popup-key (popup type from to)
  "In POPUP, bind TO to what FROM was bound to.
TYPE is one of `:action', `:sequence-action', `:switch', or
`:option'.  Bind TO and unbind FROM, both are characters."
  (--if-let (assoc from (plist-get (symbol-value popup)
                                   (eaws-popup-pluralize-type type)))
      (setcar it to)
    (message "eaws-change-popup-key: FROM key %c is unbound" from)))

(defun eaws-remove-popup-key (popup type key)
  "In POPUP, remove KEY's binding of TYPE.
POPUP is a popup command defined using `eaws-define-popup'.
TYPE is one of `:action', `:sequence-action', `:switch', or
`:option'.  KEY is the character which is to be unbound."
  (setq type (eaws-popup-pluralize-type type))
  (let* ((plist (symbol-value popup))
         (alist (plist-get plist type))
         (value (assoc key alist)))
    (set popup (plist-put plist type (delete value alist)))))

;;; Invoke

(defvar-local eaws-popup-previous-winconf nil)

(defun eaws-invoke-popup (popup mode arg)
  (let* ((def     (symbol-value popup))
         (val     (symbol-value (plist-get def :variable)))
         (default (plist-get def :default-action))
         (local   (plist-get def :use-prefix))
         (local   (if (functionp local)
                      (funcall local)
                    local))
         (use-prefix (or local eaws-popup-use-prefix-argument)))
    (cond
     ((or (and (eq use-prefix 'default) arg)
          (and (eq use-prefix 'popup) (not arg)))
      (if default
          (let ((eaws-current-popup (list popup 'default))
                (eaws-current-popup-args
                 (let ((eaws-this-popup popup)
                       (eaws-this-popup-events nil))
                   (eaws-popup-default-setup val def)
                   (eaws-popup-get-args))))
            (when (and arg (listp arg))
              (setq current-prefix-arg (and (not (= (car arg) 4))
                                            (list (/ (car arg) 4)))))
            (call-interactively default))
        (message "%s has no default action; showing popup instead." popup)
        (eaws-popup-mode-setup popup mode)))
     ((memq use-prefix '(default popup nil))
      (eaws-popup-mode-setup popup mode)
      (when eaws-popup-show-help-echo
        (message
         (format
          "[%s] show common commands, [%s] describe events, [%s] show manual"
          (propertize "C-t"   'face 'eaws-popup-key)
          (propertize "?"     'face 'eaws-popup-key)
          (propertize "C-h i" 'face 'eaws-popup-key)))))
     (local
      (error "Invalid :use-prefix popup property value: %s" use-prefix))
     (t
      (error "Invalid eaws-popup-use-prefix-argument value: %s" use-prefix)))))

(defun eaws-invoke-popup-switch (event)
  (interactive (list last-command-event))
  (--if-let (eaws-popup-lookup event :switches)
      (progn
        (setf (eaws-popup-event-use it)
              (not (eaws-popup-event-use it)))
        (eaws-refresh-popup-buffer))
    (user-error "%c isn't bound to any switch" event)))

(defun eaws-invoke-popup-option (event)
  (interactive (list last-command-event))
  (--if-let (eaws-popup-lookup event :options)
      (progn
        (if (eaws-popup-event-use it)
            (setf (eaws-popup-event-use it) nil)
          (let* ((arg (eaws-popup-event-arg it))
                 (val (funcall
                       (eaws-popup-event-fun it)
                       (concat arg (unless (string-match-p "=$" arg) ": "))
                       (eaws-popup-event-val it))))
            (setf (eaws-popup-event-use it) t)
            (setf (eaws-popup-event-val it) val)))
        (eaws-refresh-popup-buffer))
    (user-error "%c isn't bound to any option" event)))

(defun eaws-invoke-popup-action (event)
  (interactive (list last-command-event))
  (let ((action   (eaws-popup-lookup event :actions))
        (variable (eaws-popup-lookup event :variables)))
    (when (and variable (not (eaws-popup-event-arg variable)))
      (setq action variable)
      (setq variable nil))
    (cond ((or action variable)
           (let* ((eaws-current-popup eaws-this-popup)
                  (eaws-current-popup-args (eaws-popup-get-args))
                  (command (eaws-popup-event-fun (or action variable)))
                  (eaws-current-popup-action command))
             (when action
               (eaws-popup-quit))
             (call-interactively command)
             (when (eq this-command 'eaws-invoke-popup-action)
               (setq this-command command))
             (unless action
               (eaws-refresh-popup-buffer))))
          ((eq event ?q)
           (eaws-popup-quit)
           (when eaws-previous-popup
             (eaws-popup-mode-setup eaws-previous-popup nil)))
          (t
           (user-error "%c isn't bound to any action" event)))))

(defun eaws-popup-set-variable
    (variable choices &optional default other)
  (eaws-set (--if-let (eaws-git-string "config" "--local" variable)
                 (cadr (member it choices))
               (car choices))
             variable)
  (eaws-refresh)
  (message "%s %s" variable
           (eaws-popup-format-variable-1 variable choices default other)))

(defun eaws-popup-quit ()
  "Quit the current popup command without invoking an action."
  (interactive)
  (let ((winconf eaws-popup-previous-winconf))
    (if (derived-mode-p 'eaws-popup-mode)
        (kill-buffer)
      (eaws-popup-help-mode -1)
      (kill-local-variable 'eaws-popup-previous-winconf))
    (when winconf
      (set-window-configuration winconf))))

(defun eaws-popup-read-number (prompt &optional default)
  "Like `read-number' but DEFAULT may be a numeric string."
  (read-number prompt (if (stringp default)
                          (string-to-number default)
                        default)))

;;; Save

(defun eaws-popup-set-default-arguments (arg)
  "Set default value for the arguments for the current popup.
Then close the popup without invoking an action; unless a prefix
argument is used in which case the popup remains open.

For a popup named `NAME-popup' that usually means setting the
value of the custom option `NAME-arguments'."
  (interactive "P")
  (-if-let (var (eaws-popup-get :variable))
      (progn (customize-set-variable var (eaws-popup-get-args))
             (unless arg (eaws-popup-quit)))
    (user-error "Nothing to set")))

(defun eaws-popup-save-default-arguments (arg)
  "Save default value for the arguments for the current popup.
Then close the popup without invoking an action; unless a prefix
argument is used in which case the popup remains open.

For a popup named `NAME-popup' that usually means saving the
value of the custom option `NAME-arguments'."
  (interactive "P")
  (-if-let (var (eaws-popup-get :variable))
      (progn (customize-save-variable var (eaws-popup-get-args))
             (unless arg (eaws-popup-quit)))
    (user-error "Nothing to save")))

;;; Help

(defun eaws-popup-toggle-show-common-commands ()
  "Show or hide an additional section with common commands.
The commands listed in this section are common to all popups
and are defined in `eaws-popup-mode-map' (which see)."
  (interactive)
  (setq eaws-popup-show-common-commands
        (not eaws-popup-show-common-commands))
  (eaws-refresh-popup-buffer)
  (fit-window-to-buffer))

(defun eaws-popup-help ()
  "Show help for the argument or action at point."
  (interactive)
  (let* ((man (eaws-popup-get :man-page))
         (key (read-key-sequence
               (concat "Describe key" (and man " (? for manpage)") ": ")))
         (int (aref key (1- (length key))))
         (def (or (lookup-key (current-local-map)  key t)
                  (lookup-key (current-global-map) key))))
    (pcase def
      (`eaws-invoke-popup-switch
       (--if-let (eaws-popup-lookup int :switches)
           (if (and (string-prefix-p "++" (eaws-popup-event-arg it))
                    (eaws-popup-event-fun it))
               (eaws-popup-describe-function (eaws-popup-event-fun it))
             (eaws-popup-manpage man it))
         (user-error "%c isn't bound to any switch" int)))
      (`eaws-invoke-popup-option
       (--if-let (eaws-popup-lookup int :options)
           (if (and (string-prefix-p "++" (eaws-popup-event-arg it))
                    (eaws-popup-event-fun it))
               (eaws-popup-describe-function (eaws-popup-event-fun it))
             (eaws-popup-manpage man it))
         (user-error "%c isn't bound to any option" int)))
      (`eaws-popup-help
       (eaws-popup-manpage man nil))
      ((or `self-insert-command
           `eaws-invoke-popup-action)
       (setq def (or (eaws-popup-lookup int :actions)
                     (eaws-popup-lookup int :variables)))
       (if def
           (eaws-popup-describe-function (eaws-popup-event-fun def))
         (ding)
         (message nil)))
      (`nil (ding)
            (message nil))
      (_    (eaws-popup-describe-function def)))))

(defun eaws-popup-manpage (topic arg)
  (unless topic
    (user-error "No man page associated with %s"
                (eaws-popup-get :man-page)))
  (when arg
    (setq arg (eaws-popup-event-arg arg))
    (when (string-prefix-p "--" arg)
      ;; handle '--' option and the '--[no-]' shorthand
      (setq arg (cond ((string= "-- " arg)
                       "\\(?:\\[--\\] \\)?<[^[:space:]]+>\\.\\.\\.")
                      ((string-prefix-p "--no-" arg)
                       (concat "--"
                               "\\[?no-\\]?"
                               (substring arg 5)))
                      (t
                       (concat "--"
                               "\\(?:\\[no-\\]\\)?"
                               (substring arg 2)))))))
  (let ((winconf (current-window-configuration)) buffer)
    (pcase eaws-popup-manpage-package
      (`woman (delete-other-windows)
              (split-window-below)
              (with-no-warnings ; display-buffer-function is obsolete
                (let ((display-buffer-alist nil)
                      (display-buffer-function nil)
                      (display-buffer-overriding-action nil))
                  (woman topic)))
              (setq buffer (current-buffer)))
      (`man   (cl-letf (((symbol-function #'fboundp) (lambda (_) nil)))
                (setq buffer (man topic)))
              (delete-other-windows)
              (split-window-below)
              (set-window-buffer (selected-window) buffer)))
    (with-current-buffer buffer
      (setq eaws-popup-previous-winconf winconf)
      (eaws-popup-help-mode)
      (fit-window-to-buffer (next-window))
      (if (and arg
               (Man-find-section "OPTIONS")
               (let ((case-fold-search nil)
                     ;; This matches preceding/proceeding options.
                     ;; Options such as '-a', '-S[<keyid>]', and
                     ;; '--grep=<pattern>' are matched by this regex
                     ;; without the shy group. The '. ' in the shy
                     ;; group is for options such as '-m
                     ;; parent-number', and the '-[^[:space:]]+ ' is
                     ;; for options such as '--mainline parent-number'
                     (others "-\\(?:. \\|-[^[:space:]]+ \\)?[^[:space:]]+"))
                 (re-search-forward
                  ;; should start with whitespace, and may have any
                  ;; number of options before/after
                  (format "^[\t\s]+\\(?:%s, \\)*?\\(?1:%s\\)%s\\(?:, %s\\)*$"
                          others
                          ;; options don't necessarily end in an '='
                          ;; (e.g., '--gpg-sign[=<keyid>]')
                          (string-remove-suffix "=" arg)
                          ;; Simple options don't end in an '='.
                          ;; Splitting this into 2 cases should make
                          ;; getting false positives less likely.
                          (if (string-suffix-p "=" arg)
                              ;; [^[:space:]]*[^.[:space:]] matches
                              ;; the option value, which is usually
                              ;; after the option name and either '='
                              ;; or '[='. The value can't end in a
                              ;; period, as that means it's being used
                              ;; at the end of a sentence. The space
                              ;; is for options such as '--mainline
                              ;; parent-number'.
                              "\\(?: \\|\\[?=\\)[^[:space:]]*[^.[:space:]]"
                            ;; Either this doesn't match anything
                            ;; (e.g., '-a'), or the option is followed
                            ;; by a value delimited by a '[', '<', or
                            ;; ':'. A space might appear before this
                            ;; value, as in '-f <file>'. The space
                            ;; alternative is for options such as '-m
                            ;; parent-number'.
                            "\\(?:\\(?: \\| ?[\\[<:]\\)[^[:space:]]*[^.[:space:]]\\)?")
                          others)
                  nil
                  t)))
          (goto-char (match-beginning 1))
        (goto-char (point-min))))))

(defun eaws-popup-describe-function (function)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (with-no-warnings ; display-buffer-function is obsolete
      (let ((display-buffer-alist '(("" display-buffer-use-some-window)))
            (display-buffer-function nil)
            (display-buffer-overriding-action nil)
            (help-window-select nil))
        (describe-function function)))
    (fit-window-to-buffer)
    (other-window 1)
    (setq eaws-popup-previous-winconf winconf)
    (eaws-popup-help-mode)))

(defun eaws-popup-info ()
  "Show the popup manual."
  (interactive)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (info "(eaws-popup.info)Usage")
    (eaws-popup-help-mode)
    (setq eaws-popup-previous-winconf winconf))
  (eaws-popup-help-mode)
  (fit-window-to-buffer (next-window)))

(define-minor-mode eaws-popup-help-mode
  "Auxiliary minor mode used to restore previous window configuration.
When some sort of help buffer is created from within a popup,
then this minor mode is turned on in that buffer, so that when
the user quits it, the previous window configuration is also
restored."
  :keymap '(([remap Man-quit]    . eaws-popup-quit)
            ([remap Info-exit]   . eaws-popup-quit)
            ([remap quit-window] . eaws-popup-quit)))

;;; Modes

(define-derived-mode eaws-popup-mode fundamental-mode "EawsPopup"
  "Major mode for infix argument popups."
  :mode 'eaws-popup
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local scroll-margin 0)
  (setq-local eaws-popup-show-common-commands eaws-popup-show-common-commands)
  (hack-dir-local-variables-non-file-buffer))

(put 'eaws-popup-mode 'mode-class 'special)

(defun eaws-popup-default-setup (val def)
  (if (--when-let (eaws-popup-get :sequence-predicate)
        (funcall it))
      (eaws-popup-put :actions (eaws-popup-convert-actions
                                 val (eaws-popup-get :sequence-actions)))
    (let ((vars (plist-get def :variables)))
      (when (functionp vars)
        (setq vars (funcall vars)))
      (when vars
        (eaws-popup-put :variables (eaws-popup-convert-variables val vars))))
    (eaws-popup-put :switches (eaws-popup-convert-switches
                                val (plist-get def :switches)))
    (eaws-popup-put :options  (eaws-popup-convert-options
                                val (plist-get def :options)))
    (eaws-popup-put :actions  (eaws-popup-convert-actions
                                val (plist-get def :actions)))))

(defun eaws-popup-mode-setup (popup mode)
  (setq eaws-previous-popup eaws-current-popup)
  (let ((val (symbol-value (plist-get (symbol-value popup) :variable)))
        (def (symbol-value popup)))
    (eaws-popup-mode-display-buffer (get-buffer-create
                                      (format "*%s*" popup))
                                     (or mode 'eaws-popup-mode))
    (setq eaws-this-popup popup)
    (if (bound-and-true-p eaws-popup-setup-hook) ; obsolete
        (run-hook-with-args 'eaws-popup-setup-hook val def)
      (funcall (or (eaws-popup-get :setup-function)
                   'eaws-popup-default-setup)
               val def)))
  (eaws-refresh-popup-buffer)
  (fit-window-to-buffer nil nil (line-number-at-pos (point-max))))

(defun eaws-popup-mode-display-buffer (buffer mode)
  (let ((winconf (current-window-configuration)))
    (select-window (display-buffer buffer eaws-popup-display-buffer-action))
    (funcall mode)
    (setq eaws-popup-previous-winconf winconf)))

(defvar eaws-refresh-popup-buffer-hook nil
  "Hook run by `eaws-refresh-popup-buffer'.

The hook is run right after inserting the representation of the
popup events but before optionally inserting the representation
of events shared by all popups and before point is adjusted.")

(defun eaws-refresh-popup-buffer ()
  (let* ((inhibit-read-only t)
         (button (button-at (point)))
         (prefix (and button (button-get button 'prefix)))
         (event  (and button (button-get button 'event))))
    (erase-buffer)
    (save-excursion
      (--if-let (eaws-popup-get :refresh-function)
          (funcall it)
        (eaws-popup-insert-section 'eaws-popup-variable-button)
        (eaws-popup-insert-section 'eaws-popup-switch-button)
        (eaws-popup-insert-section 'eaws-popup-option-button)
        (eaws-popup-insert-section 'eaws-popup-action-button))
      (run-hooks 'eaws-refresh-popup-buffer-hook)
      (when eaws-popup-show-common-commands
        (eaws-popup-insert-command-section
         'eaws-popup-internal-command-button
         eaws-popup-common-commands)))
    (set-buffer-modified-p nil)
    (when event
      (while (and (ignore-errors (forward-button 1))
                  (let ((b (button-at (point))))
                    (or (not (equal (button-get b 'prefix) prefix))
                        (not (equal (button-get b 'event)  event)))))))))

;;; Draw

(defvar eaws-popup-min-padding 3
  "Minimal amount of whitespace between columns in popup buffers.")

(defun eaws-popup-insert-section (type &optional spec heading)
  (if (not spec)
      (progn (setq spec (eaws-popup-get (button-type-get type 'property)))
             (when spec
               (if (or (stringp (car spec))
                       (functionp (car spec)))
                   (--each (--partition-by-header
                            (or (stringp it) (functionp it))
                            spec)
                     (eaws-popup-insert-section type (cdr it) (car it)))
                 (eaws-popup-insert-section type spec))))
    (let* ((formatter (button-type-get type 'formatter))
           (items (mapcar (lambda (ev)
                            (and ev (or (funcall formatter type ev) '(""))))
                          (or spec (eaws-popup-get
                                    (button-type-get type 'property)))))
           (maxcols (button-type-get type 'maxcols))
           (pred (eaws-popup-get :sequence-predicate)))
      (if (and pred (funcall pred))
          (setq maxcols nil)
        (cl-typecase maxcols
          (keyword (setq maxcols (eaws-popup-get maxcols)))
          (symbol  (setq maxcols (symbol-value maxcols)))))
      (when (functionp maxcols)
        (setq maxcols (funcall maxcols heading)))
      (when items
        (if (functionp heading)
            (when (setq heading (funcall heading))
              (insert heading ?\n))
          (unless heading
            (setq heading (button-type-get type 'heading)))
          (insert (propertize heading 'face 'eaws-popup-heading))
          (unless (string-match "\n$" heading)
            (insert "\n")))
        (when heading
          (let ((colwidth
                 (+ (apply 'max (mapcar (lambda (e) (length (car e))) items))
                    eaws-popup-min-padding)))
            (dolist (item items)
              (unless (bolp)
                (let ((padding (- colwidth (% (current-column) colwidth))))
                  (if (and (< (+ (current-column) padding colwidth)
                              (window-width))
                           (< (ceiling (/ (current-column) (* colwidth 1.0)))
                              (or maxcols 1000)))
                      (insert (make-string padding ?\s))
                    (insert "\n"))))
              (unless (equal item '(""))
                (if item
                    (apply 'insert-button item)
                  (insert ?\s)))))
          (insert (if (= (char-before) ?\n) "\n" "\n\n")))))))

(defun eaws-popup-format-argument-button (type ev)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (concat
                               (--when-let (button-type-get type 'prefix)
                                 (char-to-string it))
                               (eaws-popup-event-keydsc ev))
                              'face 'eaws-popup-key))
           (?d . ,(eaws-popup-event-dsc ev))
           (?a . ,(propertize (eaws-popup-event-arg ev)
                              'face (if (eaws-popup-event-use ev)
                                        'eaws-popup-argument
                                      'eaws-popup-disabled-argument)))
           (?v . ,(let ((val (eaws-popup-event-val ev)))
                    (if (and (eaws-popup-event-use ev)
                             (not (equal val "")))
                        (propertize (format "\"%s\"" val)
                                    'face 'eaws-popup-option-value)
                      "")))))
        'type type 'event (eaws-popup-event-key ev)))

(defun eaws-popup-format-variable-button (type ev)
  (if (not (eaws-popup-event-arg ev))
      (eaws-popup-format-action-button 'eaws-popup-action-button ev)
    (list (format-spec
           (button-type-get type 'format)
           `((?k . ,(propertize (eaws-popup-event-keydsc ev)
                                'face 'eaws-popup-key))
             (?d . ,(funcall (eaws-popup-event-arg ev)))))
          'type type 'event (eaws-popup-event-key ev))))

(defun eaws-popup-format-variable
    (variable choices &optional default other width)
  (concat variable
          (if width (make-string (- width (length variable)) ?\s) " ")
          (eaws-popup-format-variable-1 variable choices default other)))

(defun eaws-popup-format-variable-1
    (variable choices &optional default other)
  "Print popup entry for git VARIABLE with possible CHOICES.
DEFAULT is git's default choice for VARIABLE.  OTHER is a git
variable whose value may be used as a default."
  (let ((local  (eaws-git-string "config" "--local"  variable))
        (global (eaws-git-string "config" "--global" variable)))
    (when other
      (setq other (--when-let (eaws-get other)
                    (concat other ":" it))))
    (concat
     (propertize "[" 'face 'eaws-popup-disabled-argument)
     (mapconcat
      (lambda (choice)
        (propertize choice 'face (if (equal choice local)
                                     'eaws-popup-option-value
                                   'eaws-popup-disabled-argument)))
      choices
      (propertize "|" 'face 'eaws-popup-disabled-argument))
     (when (or global other default)
       (concat
        (propertize "|" 'face 'eaws-popup-disabled-argument)
        (cond (global
               (propertize (concat "global:" global)
                           'face (cond (local
                                        'eaws-popup-disabled-argument)
                                       ((member global choices)
                                        'eaws-popup-option-value)
                                       (t
                                        'font-lock-warning-face))))
              (other
               (propertize other
                           'face (if local
                                     'eaws-popup-disabled-argument
                                   'eaws-popup-option-value)))
              (default
               (propertize (concat "default:" default)
                           'face (if local
                                     'eaws-popup-disabled-argument
                                   'eaws-popup-option-value))))))
     (propertize "]" 'face 'eaws-popup-disabled-argument))))

(defun eaws-popup-format-action-button (type ev)
  (let* ((dsc (eaws-popup-event-dsc ev))
         (fun (and (functionp dsc) dsc)))
    (when fun
      (setq dsc
            (-when-let (branch (funcall fun))
              (if (text-property-not-all 0 (length branch) 'face nil branch)
                  branch
                (eaws-branch-set-face branch)))))
    (when dsc
      (list (format-spec
             (button-type-get type 'format)
             `((?k . ,(propertize (eaws-popup-event-keydsc ev)
                                  'face 'eaws-popup-key))
               (?d . ,dsc)
               (?D . ,(if (and (not fun)
                               (eq (eaws-popup-event-fun ev)
                                   (eaws-popup-get :default-action)))
                          (propertize dsc 'face 'bold)
                        dsc))))
            'type type 'event (eaws-popup-event-key ev)))))

(defun eaws-popup-insert-command-section (type spec)
  (eaws-popup-insert-section
   type (mapcar (lambda (elt)
                  (list (car (where-is-internal (cadr elt)
                                                (current-local-map)))
                        (car elt)))
                spec)))

(defun eaws-popup-format-command-button (type elt)
  (nconc (eaws-popup-format-action-button
          type (make-eaws-popup-event :key (car  elt)
                                       :dsc (cadr elt)))
         (list 'function (lookup-key (current-local-map) (car elt)))))

;;; Utilities

(defun eaws-popup-import-file-args (args files)
  (if files
      (cons (concat "-- " (mapconcat #'identity files ",")) args)
    args))

(defun eaws-popup-export-file-args (args)
  (let ((files (--first (string-prefix-p "-- " it) args)))
    (when files
      (setq args  (remove files args))
      (setq files (split-string (substring files 3) ",")))
    (list args files)))

(defconst eaws-popup-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(eaws-define-popup\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode eaws-popup-font-lock-keywords)

(provide 'eaws-popup)
;;; eaws-popup.el ends here
