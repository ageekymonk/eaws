;;; eaws-section.el --- Emacs AWS Console  -*- lexical-binding: t; coding: utf-8 -*-

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

(require 'cl-lib)
(require 'dash)
(require 'eaws-mode)

(declare-function eaws-maybe-make-margin-overlay nil)
(defvar eaws-keep-region-overlay)

;;; Options

(defgroup eaws-section nil
  "Expandable sections."
  :link '(info-link "(eaws)Sections")
  :group 'eaws)

(defcustom eaws-section-show-child-count t
  "Whether to append the number of children to section headings.
This only applies to sections for which doing so makes sense."
  :group 'eaws-section
  :type 'boolean)

(defcustom eaws-section-movement-hook
  '(eaws-hunk-set-window-start
    eaws-log-maybe-update-revision-buffer
    eaws-log-maybe-show-more-commits)
  "Hook run by `eaws-section-goto'.
That function in turn is used by all section movement commands."
  :group 'eaws-section
  :type 'hook
  :options '(eaws-hunk-set-window-start
             eaws-status-maybe-update-revision-buffer
             eaws-status-maybe-update-blob-buffer
             eaws-log-maybe-update-revision-buffer
             eaws-log-maybe-update-blob-buffer
             eaws-log-maybe-show-more-commits))

(defcustom eaws-section-highlight-hook
  '(eaws-diff-highlight
    eaws-section-highlight
    eaws-section-highlight-selection)
  "Functions used to highlight the current section.
Each function is run with the current section as only argument
until one of them returns non-nil."
  :group 'eaws-section
  :type 'hook
  :options '(eaws-diff-highlight
             eaws-section-highlight
             eaws-section-highlight-selection))

(defcustom eaws-section-unhighlight-hook
  '(eaws-diff-unhighlight)
  "Functions used to unhighlight the previously current section.
Each function is run with the current section as only argument
until one of them returns non-nil.  Most sections are properly
unhighlighted without requiring a specialized unhighlighter,
diff-related sections being the only exception."
  :group 'eaws-section
  :type 'hook
  :options '(eaws-diff-unhighlight))

(defcustom eaws-section-set-visibility-hook
  '(eaws-section-set-visibility-from-cache)
  "Hook used to set the initial visibility of a section.
Stop at the first function that returns non-nil.  The value
should be `show' or `hide'.  If no function returns non-nil,
determine the visibility as usual, i.e. use the hardcoded
section specific default (see `eaws-insert-section')."
  :group 'eaws-section
  :type 'hook
  :options '(eaws-section-set-visibility-from-cache))

(defface eaws-section-highlight
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face for highlighting the current section."
  :group 'eaws-faces)

(defface eaws-section-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for section headings."
  :group 'eaws-faces)

(defface eaws-section-secondary-heading '((t :weight bold))
  "Face for section headings of some secondary headings."
  :group 'eaws-faces)

(defface eaws-section-heading-selection
  '((((class color) (background light)) :foreground "salmon4")
    (((class color) (background  dark)) :foreground "LightSalmon3"))
  "Face for selected section headings."
  :group 'eaws-faces)

;;; Core

(cl-defstruct eaws-section
  type value start content end hidden washer parent children)

(defvar-local eaws-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `eaws-insert-section' and you should
never modify it.")
(put 'eaws-root-section 'permanent-local t)

(defun eaws-current-section ()
  "Return the section at point."
  (or (get-text-property (point) 'eaws-section) eaws-root-section))

(defun eaws-section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (cons (cons (eaws-section-type section)
              (eaws-section-value section))
        (--when-let (eaws-section-parent section)
          (eaws-section-ident it))))

(defun eaws-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `eaws-section-ident'."
  (setq ident (reverse ident))
  (let ((section (or root eaws-root-section)))
    (when (eq (car (pop ident)) (eaws-section-type section))
      (while (and ident
                  (setq section
                        (--first
                         (and (eq    (caar ident) (eaws-section-type it))
                              (equal (cdar ident) (eaws-section-value it)))
                         (eaws-section-children section))))
        (pop ident))
      section)))

(defvar eaws-insert-section--current nil "For internal use only.")
(defvar eaws-insert-section--parent  nil "For internal use only.")
(defvar eaws-insert-section--oldroot nil "For internal use only.")

;;; Commands
;;;; Movement

(defun eaws-section-forward ()
  "Move to the beginning of the next visible section."
  (interactive)
  (if (eobp)
      (user-error "No next section")
    (let ((section (eaws-current-section)))
      (if (eaws-section-parent section)
          (let ((next (and (not (eaws-section-hidden section))
                           (not (= (eaws-section-end section) (1+ (point))))
                           (car (eaws-section-children section)))))
            (while (and section (not next))
              (unless (setq next (car (eaws-section-siblings section 'next)))
                (setq section (eaws-section-parent section))))
            (if next
                (eaws-section-goto next)
              (user-error "No next section")))
        (eaws-section-goto 1)))))

(defun eaws-section-backward ()
  "Move to the beginning of the current or the previous visible section.
When point is at the beginning of a section then move to the
beginning of the previous visible section.  Otherwise move to
the beginning of the current section."
  (interactive)
  (if (bobp)
      (user-error "No previous section")
    (let ((section (eaws-current-section)) children)
      (cond
       ((and (= (point) (1- (eaws-section-end section)))
             (setq children (eaws-section-children section)))
        (eaws-section-goto (car (last children))))
       ((and (eaws-section-parent section)
             (not (= (point) (eaws-section-start section))))
        (eaws-section-goto section))
       (t
        (let ((prev (car (eaws-section-siblings section 'prev))))
          (if prev
              (while (and (not (eaws-section-hidden prev))
                          (setq children (eaws-section-children prev)))
                (setq prev (car (last children))))
            (setq prev (eaws-section-parent section)))
          (cond (prev
                 (eaws-section-goto prev))
                ((eaws-section-parent section)
                 (user-error "No previous section"))
                ;; Eob special cases.
                ((not (get-text-property (1- (point)) 'invisible))
                 (eaws-section-goto -1))
                (t
                 (goto-char (previous-single-property-change
                             (1- (point)) 'invisible))
                 (forward-line -1)
                 (eaws-section-goto (eaws-current-section))))))))))

(defun eaws-section-up ()
  "Move to the beginning of the parent section."
  (interactive)
  (--if-let (eaws-section-parent (eaws-current-section))
      (eaws-section-goto it)
    (user-error "No parent section")))

(defun eaws-section-forward-sibling ()
  "Move to the beginning of the next sibling section.
If there is no next sibling section, then move to the parent."
  (interactive)
  (let ((current (eaws-current-section)))
    (if (eaws-section-parent current)
        (--if-let (car (eaws-section-siblings current 'next))
            (eaws-section-goto it)
          (eaws-section-forward))
      (eaws-section-goto 1))))

(defun eaws-section-backward-sibling ()
  "Move to the beginning of the previous sibling section.
If there is no previous sibling section, then move to the parent."
  (interactive)
  (let ((current (eaws-current-section)))
    (if (eaws-section-parent current)
        (--if-let (car (eaws-section-siblings current 'prev))
            (eaws-section-goto it)
          (eaws-section-backward))
      (eaws-section-goto -1))))

(defun eaws-section-goto (arg)
  (if (integerp arg)
      (progn (forward-line arg)
             (setq arg (eaws-current-section)))
    (goto-char (eaws-section-start arg)))
  (run-hook-with-args 'eaws-section-movement-hook arg))

(defun eaws-section-set-window-start (section)
  "Ensure the beginning of SECTION is visible."
  (unless (pos-visible-in-window-p (eaws-section-end section))
    (set-window-start (selected-window) (eaws-section-start section))))

(defun eaws-hunk-set-window-start (section)
  "When SECTION is a `hunk', ensure that its beginning is visible.
It the SECTION has a different type, then do nothing."
  (when (eq (eaws-section-type section) 'hunk)
    (eaws-section-set-window-start section)))

(defmacro eaws-define-section-jumper (name heading type &optional value)
  "Define an interactive function to go some section.
Together TYPE and VALUE identify the section.
HEADING is the displayed heading of the section."
  (declare (indent defun))
  `(defun ,name (&optional expand) ,(format "\
Jump to the section \"%s\".
With a prefix argument also expand it." heading)
     (interactive "P")
     (--if-let (eaws-get-section
                (cons (cons ',type ,value)
                      (eaws-section-ident eaws-root-section)))
         (progn (goto-char (eaws-section-start it))
                (when expand
                  (with-local-quit (eaws-section-show it))
                  (recenter 0)))
       (message ,(format "Section \"%s\" wasn't found" heading)))))

;;;; Visibility

(defun eaws-section-show (section)
  "Show the body of the current section."
  (interactive (list (eaws-current-section)))
  (setf (eaws-section-hidden section) nil)
  (-when-let (washer (eaws-section-washer section))
    (setf (eaws-section-washer section) nil)
    (let ((inhibit-read-only t)
          (eaws-insert-section--parent section)
          (content (eaws-section-content section)))
      (save-excursion
        (if (and content (< content (eaws-section-end section)))
            (funcall washer section) ; already partially washed (hunk)
          (goto-char (eaws-section-end section))
          (setf (eaws-section-content section) (point-marker))
          (funcall washer)
          (setf (eaws-section-end section) (point-marker)))))
    (eaws-section-update-highlight))
  (-when-let (beg (eaws-section-content section))
    (remove-overlays beg (eaws-section-end section) 'invisible t))
  (eaws-section-update-visibility-cache section)
  (dolist (child (eaws-section-children section))
    (if (eaws-section-hidden child)
        (eaws-section-hide child)
      (eaws-section-show child))))

(defun eaws-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (eaws-current-section)))
  (if (eq section eaws-root-section)
      (user-error "Cannot hide root section")
    (setf (eaws-section-hidden section) t)
    (-when-let (beg (eaws-section-content section))
      (let ((end (eaws-section-end section)))
        (remove-overlays beg end 'invisible t)
        (let ((o (make-overlay beg end)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'invisible t))))
    (when (memq (eaws-section-type section) '(unpulled unpushed))
      (eaws-section-cache-visibility section))))

(defun eaws-section-toggle (section)
  "Toggle visibility of the body of the current section."
  (interactive (list (eaws-current-section)))
  (if (eq section eaws-root-section)
      (user-error "Cannot hide root section")
    (goto-char (eaws-section-start section))
    (if (eaws-section-hidden section)
        (eaws-section-show section)
      (eaws-section-hide section))))

(defun eaws-section-toggle-children (section)
  "Toggle visibility of bodies of children of the current section."
  (interactive (list (eaws-current-section)))
  (goto-char (eaws-section-start section))
  (let* ((children (eaws-section-children section))
         (show (-any-p 'eaws-section-hidden children)))
    (dolist (c children)
      (setf (eaws-section-hidden c) show)))
  (eaws-section-show section))

(defun eaws-section-show-children (section &optional depth)
  "Recursively show the bodies of children of the current section.
With a prefix argument show children that deep and hide deeper
children."
  (interactive (list (eaws-current-section)))
  (eaws-section-show-children-1 section depth)
  (eaws-section-show section))

(defun eaws-section-show-children-1 (section &optional depth)
  (dolist (s (eaws-section-children section))
    (setf (eaws-section-hidden s) nil)
    (if depth
        (if (> depth 0)
            (eaws-section-show-children-1 s (1- depth))
          (eaws-section-hide s))
      (eaws-section-show-children-1 s))))

(defun eaws-section-hide-children (section)
  "Recursively hide the bodies of children of the current section."
  (interactive (list (eaws-current-section)))
  (mapc 'eaws-section-hide (eaws-section-children section)))

(defun eaws-section-show-headings (section)
  "Recursively show headings of children of the current section.
Only show the headings, previously shown text-only bodies are
hidden."
  (interactive (list (eaws-current-section)))
  (eaws-section-show-headings-1 section)
  (eaws-section-show section))

(defun eaws-section-show-headings-1 (section)
  (dolist (s (eaws-section-children section))
    (setf (eaws-section-hidden s) nil)
    (when (or (eaws-section-children s)
              (not (eaws-section-content s)))
      (eaws-section-show-headings-1 s))))

(defun eaws-section-cycle (section)
  "Cycle visibility of current section and its children."
  (interactive (list (eaws-current-section)))
  (goto-char (eaws-section-start section))
  (if (eaws-section-hidden section)
      (progn (eaws-section-show section)
             (eaws-section-hide-children section))
    (let ((children (eaws-section-children section)))
      (cond ((and (-any-p 'eaws-section-hidden   children)
                  (-any-p 'eaws-section-children children))
             (eaws-section-show-headings section))
            ((-any-p 'eaws-section-hidden-body children)
             (eaws-section-show-children section))
            (t
             (eaws-section-hide section))))))

(defun eaws-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (eaws-section-children eaws-root-section)))
    (cond ((and (-any-p 'eaws-section-hidden   children)
                (-any-p 'eaws-section-children children))
           (eaws-section-show-headings eaws-root-section))
          ((-any-p 'eaws-section-hidden-body children)
           (eaws-section-show-children eaws-root-section))
          (t
           (mapc 'eaws-section-hide children)))))

(defun eaws-section-cycle-diffs ()
  "Cycle visibility of diff-related sections in the current buffer."
  (interactive)
  (-when-let (sections
              (cond ((derived-mode-p 'eaws-status-mode)
                     (--mapcat
                      (when it
                        (when (eaws-section-hidden it)
                          (eaws-section-show it))
                        (eaws-section-children it))
                      (list (eaws-get-section '((staged)   (status)))
                            (eaws-get-section '((unstaged) (status))))))
                    ((derived-mode-p 'eaws-diff-mode)
                     (--filter (eq (eaws-section-type it) 'file)
                               (eaws-section-children eaws-root-section)))))
    (if (-any-p 'eaws-section-hidden sections)
        (dolist (s sections)
          (eaws-section-show s)
          (eaws-section-hide-children s))
      (let ((children (-mapcat 'eaws-section-children sections)))
        (cond ((and (-any-p 'eaws-section-hidden   children)
                    (-any-p 'eaws-section-children children))
               (mapc 'eaws-section-show-headings sections))
              ((-any-p 'eaws-section-hidden-body children)
               (mapc 'eaws-section-show-children sections))
              (t
               (mapc 'eaws-section-hide sections)))))))

(defun eaws-section-hidden-body (section &optional pred)
  (--if-let (eaws-section-children section)
      (funcall (or pred '-any-p) 'eaws-section-hidden-body it)
    (and (eaws-section-content section)
         (eaws-section-hidden  section))))

(defun eaws-section-invisible-p (section)
  "Return t if the SECTION's body is invisible.
When the body of an ancestor of SECTION is collapsed then
SECTION's body (and heading) obviously cannot be visible."
  (or (eaws-section-hidden section)
      (--when-let (eaws-section-parent section)
        (eaws-section-invisible-p it))))

(defun eaws-section-show-level (level)
  "Show surrounding sections up to LEVEL.
If LEVEL is negative, show up to the absolute value.
Sections at higher levels are hidden."
  (if (< level 0)
      (let ((s (eaws-current-section)))
        (setq level (- level))
        (while (> (1- (length (eaws-section-ident s))) level)
          (setq s (eaws-section-parent s))
          (goto-char (eaws-section-start s)))
        (eaws-section-show-children eaws-root-section (1- level)))
    (cl-do* ((s (eaws-current-section) (eaws-section-parent s))
             (i (1- (length (eaws-section-ident s))) (cl-decf i)))
        ((cond ((< i level) (eaws-section-show-children s (- level i 1)) t)
               ((= i level) (eaws-section-hide s) t))
         (eaws-section-goto s)))))

(defun eaws-section-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (eaws-section-show-level 1))

(defun eaws-section-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (eaws-section-show-level -1))

(defun eaws-section-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (eaws-section-show-level 2))

(defun eaws-section-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (eaws-section-show-level -2))

(defun eaws-section-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (eaws-section-show-level 3))

(defun eaws-section-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (eaws-section-show-level -3))

(defun eaws-section-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (eaws-section-show-level 4))

(defun eaws-section-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (eaws-section-show-level -4))

;;;; Auxiliary

(defun eaws-describe-section ()
  "Show information about the section at point.
This command is intended for debugging purposes."
  (interactive)
  (let ((section (eaws-current-section)))
    (message "%S %S %s-%s"
             (eaws-section-value section)
             (apply 'vector (mapcar 'car (eaws-section-ident section)))
             (marker-position (eaws-section-start section))
             (marker-position (eaws-section-end section)))))

;;; Match

(cl-defun eaws-section-match
    (condition &optional (section (eaws-current-section)))
  "Return t if SECTION matches CONDITION.
SECTION defaults to the section at point.  If SECTION is not
specified and there also is no section at point, then return
nil.
CONDITION can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [TYPE...]       matches if the first TYPE matches the type
                  of the section, the second matches that of
                  its parent, and so on.
  [* TYPE...]     matches sections that match [TYPE...] and
                  also recursively all their child sections.
  TYPE            matches sections of TYPE regardless of the
                  types of the parent sections.
Each TYPE is a symbol.  Note that it is not necessary to specify
all TYPEs up to the root section as printed by
`eaws-describe-type', unless of course you want to be that
precise."
  ;; For backward compatibility reasons SECTION can also be a
  ;; type-list as understood by `eaws-section-match-1'.  This
  ;; includes uses of the macros `eaws-section-when' and
  ;; `eaws-section-case' that did not get recompiled after
  ;; this function was changed.
  (and section
       (eaws-section-match-1 condition
                              (if (eaws-section-p section)
                                  (mapcar #'car (eaws-section-ident section))
                                section))))

(defun eaws-section-match-1 (condition type-list)
  (if (listp condition)
      (--first (eaws-section-match-1 it type-list) condition)
    (eaws-section-match-2 (if (symbolp condition)
                               (list condition)
                             (append condition nil))
                           type-list)))

(defun eaws-section-match-2 (l1 l2)
  (or (null l1)
      (if (eq (car l1) '*)
          (or (eaws-section-match-2 (cdr l1) l2)
              (and l2
                   (eaws-section-match-2 l1 (cdr l2))))
        (and l2
             (equal (car l1) (car l2))
             (eaws-section-match-2 (cdr l1) (cdr l2))))))

(defmacro eaws-section-when (condition &rest body)
  "If the section at point matches CONDITION, evaluate BODY.
If the section matches, then evaluate BODY forms sequentially
with `it' bound to the section and return the value of the last
form.  If there are no BODY forms, then return the value of the
section.  If the section does not match or if there is no section
at point, then return nil.
See `eaws-section-match' for the forms CONDITION can take."
  (declare (indent 1)
           (debug (sexp body)))
  `(--when-let (eaws-current-section)
     ;; Quoting CONDITION here often leads to double-quotes, which
     ;; isn't an issue because `eaws-section-match-1' implicitly
     ;; deals with that.  We shouldn't force users of this function
     ;; to not quote CONDITION because that would needlessly break
     ;; backward compatibility.
     (when (eaws-section-match ',condition it)
       ,@(or body '((eaws-section-value it))))))

(defmacro eaws-section-case (&rest clauses)
  "Choose among clauses on the type of the section at point.
Each clause looks like (CONDITION BODY...).  The type of the
section is compared against each CONDITION; the BODY forms of the
first match are evaluated sequentially and the value of the last
form is returned.  Inside BODY the symbol `it' is bound to the
section at point.  If no clause succeeds or if there is no
section at point, return nil.
See `eaws-section-match' for the forms CONDITION can take.
Additionally a CONDITION of t is allowed in the final clause, and
matches if no other CONDITION match, even if there is no section
at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  (let ((ident (cl-gensym "id")))
    `(let* ((it (eaws-current-section))
            (,ident (and it (mapcar 'car (eaws-section-ident it)))))
       (cond ,@(mapcar (lambda (clause)
                         `(,(or (eq (car clause) t)
                                `(and it (eaws-section-match-1
                                          ',(car clause) ,ident)))
                           ,@(cdr clause)))
                       clauses)))))
;;; Create

(defvar eaws-insert-section-hook nil
  "Hook run after `eaws-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro eaws-insert-section (&rest args)
  "Insert a section at point.
TYPE is the section type, a symbol.  Many commands that act on
the current section behave differently depending on that type.
Also if a variable `eaws-TYPE-section-map' exists, then use
that as the text-property `keymap' of all text belonging to the
section (but this may be overwritten in subsections).  TYPE can
also have the form `(eval FORM)' in which case FORM is evaluated
at runtime.
Optional VALUE is the value of the section, usually a string
that is required when acting on the section.
When optional HIDE is non-nil collapse the section body by
default, i.e. when first creating the section, but not when
refreshing the buffer.  Else expand it by default.  This can be
overwritten using `eaws-section-set-visibility-hook'.  When a
section is recreated during a refresh, then the visibility of
predecessor is inherited and HIDE is ignored (but the hook is
still honored).
BODY is any number of forms that actually insert the section's
heading and body.  Optional NAME, if specified, has to be a
symbol, which is then bound to the struct of the section being
inserted.
Before BODY is evaluated the `start' of the section object is set
to the value of `point' and after BODY was evaluated its `end' is
set to the new value of `point'; BODY is responsible for moving
`point' forward.
If it turns out inside BODY that the section is empty, then
`eaws-cancel-section' can be used to abort and remove all traces
of the partially inserted section.  This can happen when creating
a section by washing Git's output and Git didn't actually output
anything this time around.
\(fn [NAME] (TYPE &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp]
                   (&or [("eval" symbolp) &optional form form]
                        [symbolp &optional form form])
                   body)))
  (let ((s (if (symbolp (car args))
               (pop args)
             (cl-gensym "section"))))
    `(let* ((,s (make-eaws-section
                 :type ,(let ((type (nth 0 (car args))))
                          (if (eq (car-safe type) 'eval)
                              (cadr type)
                            `',type))
                 :value ,(nth 1 (car args))
                 :start (point-marker)
                 :parent eaws-insert-section--parent)))
       (setf (eaws-section-hidden ,s)
             (-if-let (value (run-hook-with-args-until-success
                              'eaws-section-set-visibility-hook ,s))
                 (eq value 'hide)
               (--if-let (and eaws-insert-section--oldroot
                              (eaws-get-section
                               (eaws-section-ident ,s)
                               eaws-insert-section--oldroot))
                   (eaws-section-hidden it)
                 ,(nth 2 (car args)))))
       (let ((eaws-insert-section--current ,s)
             (eaws-insert-section--parent  ,s)
             (eaws-insert-section--oldroot
              (or eaws-insert-section--oldroot
                  (unless eaws-insert-section--parent
                    (prog1 eaws-root-section
                      (setq eaws-root-section ,s))))))
         (catch 'cancel-section
           ,@(cdr args)
           (run-hooks 'eaws-insert-section-hook)
           (eaws-insert-child-count ,s)
           (set-marker-insertion-type (eaws-section-start ,s) t)
           (let* ((end (setf (eaws-section-end ,s) (point-marker)))
                  (map (intern (format "eaws-%s-section-map"
                                       (eaws-section-type ,s))))
                  (map (and (boundp map) (symbol-value map))))
             (save-excursion
               (goto-char (eaws-section-start ,s))
               (while (< (point) end)
                 (let ((next (or (next-single-property-change
                                  (point) 'eaws-section)
                                 end)))
                   (unless (get-text-property (point) 'eaws-section)
                     (put-text-property (point) next 'eaws-section ,s)
                     (when map
                       (put-text-property (point) next 'keymap map)))
                   (goto-char next)))))
           (if (eq ,s eaws-root-section)
               (eaws-section-show ,s)
             (setf (eaws-section-children (eaws-section-parent ,s))
                   (nconc (eaws-section-children (eaws-section-parent ,s))
                          (list ,s)))))
         ,s))))

(defun eaws-cancel-section ()
  (when eaws-insert-section--current
    (if (not (eaws-section-parent eaws-insert-section--current))
        (insert "(empty)\n")
      (delete-region (eaws-section-start eaws-insert-section--current)
                     (point))
      (setq eaws-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun eaws-insert-heading (&rest args)
  "Insert the heading for the section currently being inserted.
This function should only be used inside `eaws-insert-section'.
When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line when this function is used like this.
When called with arguments ARGS, which have to be strings, then
insert those strings at point.  The section should not contain
any text before this happens and afterwards it should again only
contain a single line.  If the `face' property is set anywhere
inside any of these strings, then insert all of them unchanged.
Otherwise use the `eaws-section-heading' face for all inserted
text.
The `content' property of the section struct is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end').  If the
value of `content' is nil, then the section has no heading and
its body cannot be collapsed.  If a section does have a heading,
then its height must be exactly one line, including a trailing
newline character.  This isn't enforced, you are responsible for
getting it right.  The only exception is that this function does
insert a newline character if necessary."
  (declare (indent defun))
  (when args
    (let ((heading (apply #'concat args)))
      (insert (if (text-property-not-all 0 (length heading) 'face nil heading)
                  heading
                (propertize heading 'face 'eaws-section-heading)))))
  (unless (bolp)
    (insert ?\n))
  ;;(eaws-maybe-make-margin-overlay)
  (setf (eaws-section-content eaws-insert-section--current) (point-marker)))

(defvar eaws-insert-headers--hook nil "For internal use only.")
(defvar eaws-insert-headers--beginning nil "For internal use only.")

(defun eaws-insert-headers (hooks)
  (let ((eaws-insert-section-hook
         (cons 'eaws-insert-remaining-headers
               (if (listp eaws-insert-section-hook)
                   eaws-insert-section-hook
                 (list eaws-insert-section-hook))))
        (eaws-insert-headers--hook hooks)
        wrapper)
    (setq eaws-insert-headers--beginning (point))
    (while (and (setq wrapper (pop eaws-insert-headers--hook))
                (= (point) eaws-insert-headers--beginning))
      (funcall wrapper))))

(defun eaws-insert-remaining-headers ()
  (if (= (point) eaws-insert-headers--beginning)
      (eaws-cancel-section)
    (eaws-insert-heading)
    (remove-hook 'eaws-insert-section-hook 'eaws-insert-remaining-headers)
    (mapc #'funcall eaws-insert-headers--hook)
    (insert "\n")))

(defun eaws-insert-child-count (section)
  "Modify SECTION's heading to contain number of child sections.
If `eaws-section-show-child-count' is non-nil and the SECTION
has children and its heading ends with \":\", then replace that
with \" (N)\", where N is the number of child sections.
This function is called by `eaws-insert-section' after that has
evaluated its BODY.  Admittedly that's a bit of a hack."
  ;; This has to be fast, not pretty!
  (let (content count)
    (when (and eaws-section-show-child-count
               (setq count (length (eaws-section-children section)))
               (> count 0)
               (setq content (eaws-section-content section))
               (eq (char-before (1- content)) ?:))
      (save-excursion
        (goto-char (- content 2))
        (insert (format " (%s)" count))
        (delete-char 1)))))

;;; Update

(defvar-local eaws-section-highlight-overlays nil)
(defvar-local eaws-section-highlighted-section nil)
(defvar-local eaws-section-highlighted-sections nil)
(defvar-local eaws-section-unhighlight-sections nil)

(defun eaws-section-update-region (_)
  "When the region is a valid section-selection, highlight them all."
  ;; At least that's what it does conceptually.  In actuality it just
  ;; returns a list of those sections, and it doesn't even matter if
  ;; this is a member of `eaws-region-highlight-hook'.  It probably
  ;; should be removed, but I want to make sure before removing it.
  (eaws-region-sections))

(defun eaws-section-update-highlight ()
  (let ((section (eaws-current-section)))
    (unless (eq section eaws-section-highlighted-section)
      (let ((inhibit-read-only t)
            (deactivate-mark nil)
            (selection (eaws-region-sections)))
        (mapc #'delete-overlay eaws-section-highlight-overlays)
        (setq eaws-section-highlight-overlays nil)
        (setq eaws-section-unhighlight-sections
              eaws-section-highlighted-sections)
        (setq eaws-section-highlighted-sections nil)
        (unless (eq section eaws-root-section)
          (run-hook-with-args-until-success
           'eaws-section-highlight-hook section selection))
        (--each eaws-section-unhighlight-sections
          (run-hook-with-args-until-success
           'eaws-section-unhighlight-hook it selection))
        (restore-buffer-modified-p nil)
        (unless (eq eaws-section-highlighted-section section)
          (setq eaws-section-highlighted-section
                (unless (eaws-section-hidden section) section))))
      (setq deactivate-mark nil))))

(defun eaws-section-highlight (section selection)
  "Highlight SECTION and if non-nil all sections in SELECTION.
This function works for any section but produces undesirable
effects for diff related sections, which by default are
highlighted using `eaws-diff-highlight'.  Return t."
  (cond (selection
         (eaws-section-make-overlay (eaws-section-start     (car selection))
                                     (eaws-section-end (car (last selection)))
                                     'eaws-section-highlight)
         (eaws-section-highlight-selection nil selection))
        (t
         (eaws-section-make-overlay (eaws-section-start section)
                                     (eaws-section-end   section)
                                     'eaws-section-highlight)))
  t)

(defun eaws-section-highlight-selection (_ selection)
  "Highlight the section-selection region.
If SELECTION is non-nil, then it is a list of sections selected by
the region.  The headings of these sections are then highlighted.
This is a fallback for people who don't want to highlight the
current section and therefore removed `eaws-section-highlight'
from `eaws-section-highlight-hook'.
This function is necessary to ensure that a representation of
such a region is visible.  If neither of these functions were
part of the hook variable, then such a region would be
invisible."
  (when (and selection
             (not (and (eq this-command 'mouse-drag-region))))
    (--each selection
      (eaws-section-make-overlay (eaws-section-start it)
                                  (or (eaws-section-content it)
                                      (eaws-section-end it))
                                  'eaws-section-heading-selection))
    t))

(defun eaws-section-make-overlay (start end face)
  ;; Yes, this doesn't belong here.  But the alternative of
  ;; spreading this hack across the code base is even worse.
  (when (and eaws-keep-region-overlay
             (memq face '(eaws-section-heading-selection
                          eaws-diff-file-heading-selection
                          eaws-diff-hunk-heading-selection)))
    (setq face (list :foreground (face-foreground face))))
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (push ov eaws-section-highlight-overlays)
    ov))

(defun eaws-section-goto-successor (section line char arg)
  (let ((ident (eaws-section-ident section)))
    (--if-let (eaws-get-section ident)
        (let ((start (eaws-section-start it)))
          (goto-char start)
          (unless (eq it eaws-root-section)
            (ignore-errors
              (forward-line line)
              (forward-char char))
            (unless (eq (eaws-current-section) it)
              (goto-char start))))
      (or (and (eq (eaws-section-type section) 'hunk)
               (-when-let (parent (eaws-get-section
                                   (eaws-section-ident
                                    (eaws-section-parent section))))
                 (let* ((children (eaws-section-children parent))
                        (siblings (eaws-section-siblings section 'prev))
                        (previous (nth (length siblings) children)))
                   (if (not arg)
                       (--when-let (or previous (car (last children)))
                         (eaws-section-goto it)
                         t)
                     (when previous
                       (eaws-section-goto previous))
                     (if (and (stringp arg)
                              (re-search-forward
                               arg (eaws-section-end parent) t))
                         (goto-char (match-beginning 0))
                       (goto-char (eaws-section-end (car (last children))))
                       (forward-line -1)
                       (while (looking-at "^ ")    (forward-line -1))
                       (while (looking-at "^[-+]") (forward-line -1))
                       (forward-line))))))
          (goto-char (--if-let (eaws-section-goto-successor-1 section)
                         (if (eq (eaws-section-type it) 'button)
                             (point-min)
                           (eaws-section-start it))
                       (point-min)))))))

(defun eaws-section-goto-successor-1 (section)
  (or (--when-let (pcase (eaws-section-type section)
                    (`staged 'unstaged)
                    (`unstaged 'staged)
                    (`unpushed 'unpulled)
                    (`unpulled 'unpushed))
        (eaws-get-section `((,it) (status))))
      (--when-let (car (eaws-section-siblings section 'next))
        (eaws-get-section (eaws-section-ident it)))
      (--when-let (car (eaws-section-siblings section 'prev))
        (eaws-get-section (eaws-section-ident it)))
      (--when-let (eaws-section-parent section)
        (or (eaws-get-section (eaws-section-ident it))
            (eaws-section-goto-successor-1 it)))))

;;; Visibility

(defvar-local eaws-section-visibility-cache nil)
(put 'eaws-section-visibility-cache 'permanent-local t)

(defun eaws-section-set-visibility-from-cache (section)
  "Set SECTION's visibility to the cached value.
Currently the cache can only be used to remember that a section's
body should be collapsed, not that it should be expanded.  Return
either `hide' or nil."
  (and (member (eaws-section-visibility-ident section)
               eaws-section-visibility-cache)
       'hide))

(cl-defun eaws-section-cache-visibility
    (&optional (section eaws-insert-section--current))
  (let ((ident (eaws-section-visibility-ident section)))
    (if (eaws-section-hidden section)
        (cl-pushnew ident eaws-section-visibility-cache :test #'equal)
      (setq eaws-section-visibility-cache
            (delete ident eaws-section-visibility-cache)))))

(defun eaws-section-update-visibility-cache (section)
  (setq eaws-section-visibility-cache
        (delete (eaws-section-visibility-ident section)
                eaws-section-visibility-cache)))

(defun eaws-section-visibility-ident (section)
  (let ((type  (eaws-section-type  section))
        (value (eaws-section-value section)))
    (cons type
          (cond ((not (memq type '(unpulled unpushed))) value)
                ((string-match-p "@{upstream}" value) value)
                ;; Unfortunately Git chokes on "@{push}" when the
                ;; value of `push.default' does not allow a 1:1
                ;; mapping.  But collapsed logs of unpushed and
                ;; unpulled commits in the status buffer should
                ;; remain invisible after changing branches.
                ;; So we have to pretend the value is constant.
                ((string-match-p "\\`\\.\\." value) "..@{push}")
                (t "@{push}..")))))

;;; Utilities

(cl-defun eaws-section-selected-p (section &optional (selection nil sselection))
  (and (not (eq section eaws-root-section))
       (or  (eq section (eaws-current-section))
            (memq section (if sselection
                              selection
                            (setq selection (eaws-region-sections))))
            (--when-let (eaws-section-parent section)
              (eaws-section-selected-p it selection)))))

(defun eaws-section-parent-value (section)
  (setq section (eaws-section-parent section))
  (when section (eaws-section-value  section)))

(defun eaws-section-siblings (section &optional direction)
  "Return a list of the sibling sections of SECTION.
If optional DIRECTION is `prev', then return siblings that come
before SECTION.  If it is `next', then return siblings that come
after SECTION.  For all other values, return all siblings
excluding SECTION itself."
  (-when-let (parent (eaws-section-parent section))
    (let ((siblings  (eaws-section-children parent)))
      (pcase direction
        (`prev  (cdr (member section (reverse siblings))))
        (`next  (cdr (member section siblings)))
        (_      (remq section siblings))))))

(defun eaws-region-values (&optional types multiple)
  "Return a list of the values of the selected sections.
Also see `eaws-region-sections' whose doc-string explains when a
region is a valid section selection.  If the region is not active
or is not a valid section selection, then return nil.  If optional
TYPES is non-nil then the selection not only has to be valid; the
types of all selected sections additionally have to match one of
TYPES, or nil is returned."
  (mapcar #'eaws-section-value (eaws-region-sections types multiple)))

(defun eaws-region-sections (&optional types multiple)
  "Return a list of the selected sections.
When the region is active and constitutes a valid section
selection, then return a list of all selected sections.  This is
the case when the region begins in the heading of a section and
ends in the heading of the same section or in that of a sibling
section.  If optional MULTIPLE is non-nil, then the region cannot
begin and end in the same section.
When the selection is not valid, then return nil.  In this case,
most commands that can act on the selected sections will instead
act on the section at point.
When the region looks like it would in any other buffer then
the selection is invalid.  When the selection is valid then the
region uses the `eaws-section-highlight' face.  This does not
apply to diffs where things get a bit more complicated, but even
here if the region looks like it usually does, then that's not
a valid selection as far as this function is concerned.
If optional TYPES is non-nil, then the selection not only has to
be valid; the types of all selected sections additionally have
to match one of TYPES, or nil is returned.  TYPES can also be a
single type, instead of a list of types."
  (when (region-active-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (sbeg (get-text-property rbeg 'eaws-section))
           (send (get-text-property rend 'eaws-section)))
      (when (and send
                 (not (eq send eaws-root-section))
                 (not (and multiple (eq send sbeg))))
        (let ((siblings (cons sbeg (eaws-section-siblings sbeg 'next)))
              sections)
          (when (and (memq send siblings)
                     (eaws-section-position-in-heading-p sbeg rbeg)
                     (eaws-section-position-in-heading-p send rend))
            (while siblings
              (push (car siblings) sections)
              (when (eq (pop siblings) send)
                (setq siblings nil)))
            (setq sections (nreverse sections))
            (when (and types (symbolp types))
              (setq types (list types)))
            (when (or (not types)
                      (--all-p (memq (eaws-section-type it) types) sections))
              sections)))))))

(defun eaws-section-position-in-heading-p (&optional section pos)
  "Return t if POSITION is inside the heading of SECTION.
POSITION defaults to point and SECTION defaults to the
current section."
  (unless section
    (setq section (eaws-current-section)))
  (unless pos
    (setq pos (point)))
  (and section
       (>= pos (eaws-section-start section))
       (<  pos (or (eaws-section-content section)
                   (eaws-section-end section)))
       t))

(defun eaws-section-internal-region-p (&optional section)
  "Return t if the region is active and inside SECTION's body.
If optional SECTION is nil, use the current section."
  (and (region-active-p)
       (or section (setq section (eaws-current-section)))
       (let ((beg (get-text-property (region-beginning) 'eaws-section)))
         (and (eq beg (get-text-property   (region-end) 'eaws-section))
              (eq beg section)))
       (not (or (eaws-section-position-in-heading-p section (region-beginning))
                (eaws-section-position-in-heading-p section (region-end))))
       t))

(defun eaws-section--backward-protected ()
  "Move to the beginning of the current or the previous visible section.
Same as `eaws-section-backward' but for non-interactive use.
Suppress `eaws-section-movement-hook', and return a boolean to
indicate whether a section was found, instead of raising an error
if not."
  (condition-case nil
      (let ((eaws-section-movement-hook nil))
        (eaws-section-backward)
        t)
    (user-error nil)))

(defun eaws-section--backward-find (predicate)
  "Move to the first previous section satisfying PREDICATE.
PREDICATE does not take any parameter and should not move
point."
  (let (found)
    (while (and (setq found (eaws-section--backward-protected))
                (not (funcall predicate))))
    found))

(defun eaws-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

(defun eaws-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.
Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member, then move it to the new location.
If optional AT is non-nil and a member of the hook list, then
add FUNCTION next to that instead.  Add before or after AT, or
replace AT with FUNCTION depending on APPEND.  If APPEND is the
symbol `replace', then replace AT with FUNCTION.  For any other
non-nil value place FUNCTION right after AT.  If nil, then place
FUNCTION right before AT.  If FUNCTION already is a member of the
list but AT is not, then leave FUNCTION where ever it already is.
If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.
HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'."
  (unless (boundp hook)
    (error "Cannot add function to undefined hook variable %s" hook))
  (or (default-boundp hook) (set-default hook nil))
  (let ((value (if local
                   (if (local-variable-p hook)
                       (symbol-value hook)
                     (unless (local-variable-if-set-p hook)
                       (make-local-variable hook))
                     (copy-sequence (default-value hook)))
                 (default-value hook))))
    (if at
        (when (setq at (member at value))
          (setq value (delq function value))
          (cond ((eq append 'replace)
                 (setcar at function))
                (append
                 (push function (cdr at)))
                (t
                 (push (car at) (cdr at))
                 (setcar at function))))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (when (eq append 'replace)
      (setq value (delq at value)))
    (if local
        (set hook value)
      (set-default hook value))))

(defun eaws-run-section-hook (hook)
  "Run HOOK, warning about invalid entries."
  (--if-let (-remove #'functionp (symbol-value hook))
      (progn
        (message "`%s' contains entries that are no longer valid.
%s\nUsing standard value instead.  Please re-configure hook variable."
                 hook
                 (mapconcat (lambda (sym) (format "  `%s'" sym)) it "\n"))
        (sit-for 5)
        (defvar eaws--hook-standard-value nil)
        (let ((eaws--hook-standard-value
               (eval (car (get hook 'standard-value)))))
          (run-hooks 'eaws---hook-standard-value)))
    (run-hooks hook)))

(provide 'eaws-section)
