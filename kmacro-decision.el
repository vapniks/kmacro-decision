;;; kmacro-decision.el --- Add conditional branching to keyboard macros

;; Filename: kmacro-decision.el
;; Description: Add conditional branching to keyboard macros
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-05-15 05:04:08
;; Version: 1.1
;; Last-Updated: 2013-05-15 05:04:08
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/kmacro-decision
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((el-x "1.0"))
;;
;; Features that might be required by this library:
;;
;; el-x
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1D6meUBuHXLxQNiBfaNKYRfWVVTTYU2okM
;;
;; This library changes keyboard macro query points into decision points or conditional
;; branches. A query point can be created by typing C-x q or entering the `kbd-macro-query'
;; command when entering a keyboard macro.
;; When the macro is replayed and the query point is reached the user will be prompted with
;; options to either quit the macro, continue the rest of the macro, enter recursive edit and
;; store a new macro, add a conditional branch (explained next), or replay a previously saved
;; (named) macro. The user may also recenter the window by pressing C-l.
;; If the user chooses to add a conditional branch they will be prompted for a condition form,
;; and an action to perform if that condition evaluates to non-nil. The action can be to quit the macro,
;; continue the macro, create a new macro for that condition, execute an elisp form or command, or replay
;; a previously saved macro.
;; If the condition evaluates to non-nil the next time the macro is replayed then the corresponding
;; action will be performed. If several conditions-action pairs are created for a given query point
;; then the conditions will be evaluated in the order in which they where created until one of them evaluates
;; to non-nil. If they all evaluate to nil then the user will be prompted as before to either quit, continue,
;; create/replay a macro, or add another condition-action pair.

;; By adding query points to the end of each newly created macro, macro decision trees can be built up
;; and complex automated operations performed.

;; Note: you can also use `kbd-macro-query' to choose a named macro to replay when not recording a macro
;; (in case you forgot the name). Also note that when prompted for a condition you can scroll forward through
;; the input history using M-n to get conditions for searching for strings/regexps. You can add to this list
;; by customizing `kmacro-decision-conditions'.

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `kmacro-decision-conditions'
;;    A list of conditions to be made available in the history list in calls to `kmacro-decision'
;;    default = (quote ("(search-forward \"??\" nil nil)" "(re-search-forward \"??\" nil nil)"))

;;; Installation:
;;
;; Put kmacro-decision.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'kmacro-decision)


;;; Change log:
;;	
;; 2013/05/15
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; Finish `kmacro-decision-menu', and integrate with `one-key-read-list' if available.
;;

;;; Require

(require 'el-x)                         ;needed for dflet
(require 'jb-misc-macros)               ;misc macros

;;; Code:

(defcustom kmacro-decision-conditions '("(search-forward \"??\" nil t)"
                                        "(re-search-forward \"??\" nil t)"
                                        "(search-forward (car kill-ring) nil t)"
                                        "(re-search-forward (car kill-ring) nil t)")
  "A list of conditions to be made available in the history list in calls to `kmacro-decision'"
  :type '(repeat string)
  :group 'kmacro)

;;;###autoload
(defun kmacro-decision nil
  "Prompt for an action to perform, or conditional branch to add to a query point in a keyboard macro.
This is a replacement for the `kbd-macro-query' command that comes with emacs.
When called from within a keyboard macro it offer the user the following choices:

1) Quit the macro
2) Continue executing the macro
3) Enter recursive edit and create a new named macro
4) Add a conditional branch
5) Replay a previously saved named macro

If the user chooses to add a conditional branch they will be further prompted for a condition form
and an action to perform if that form evaluates to non-nil the next time that the query point
is reached."
  (interactive)
  (if defining-kbd-macro
      nil
    ;; temporarily clear the currently executing macro
    (let* ((calling-kbd-macro executing-kbd-macro)
           (executing-kbd-macro nil)
           (defining-kbd-macro nil)
           (isvec (vectorp calling-kbd-macro)))
      ;; If the next char in the macro is C-g then continue executing the macro.
      (if (and (< executing-kbd-macro-index (length calling-kbd-macro))
               (eq (aref calling-kbd-macro executing-kbd-macro-index) 7))
          (setq executing-kbd-macro-index (1+ executing-kbd-macro-index))
        ;; otherwise prompt the user for a choice
        (let ((val (kmacro-decision-menu))
              (editfunc ;; Function for creating and returning a macro
               (lambda nil
                 ;; Need to ensure final macro in kmacro-ring is replaced at the end
                 (let* ((last-macro (copy-list (last kmacro-ring)))
                        (exitkey
                         (or (car (where-is-internal 'exit-recursive-edit))
                             (jb-untilnext
                              (read-key-sequence
                               "There is currently no keybinding for the `exit-recursive-edit' command!

Enter a global keybinding for this command: ")
                              (read-key-sequence "That key is already used! Try another: ")
                              (lambda (x) (not (key-binding x))))))
                        symbol)
                   (global-set-key exitkey 'exit-recursive-edit)
                   (kmacro-start-macro nil) ;start recording macro
                   ;; If end-kbd-macro is called just quit recursive-edit
                   (message "Press %s to finish" exitkey)
                   (dflet ((end-kbd-macro (x y) (exit-recursive-edit))
                           (kmacro-call-repeat-key nil))
                     (recursive-edit))
                   (end-kbd-macro nil #'kmacro-loop-setup-function) ;stop recording macro
                   (if (or (not last-kbd-macro)
                           (and last-kbd-macro (= (length last-kbd-macro) 0)))
                       (message "Ignore empty macro")
                     ;; prompt the user to name the macro
                     (setq symbol (call-interactively 'kmacro-decision-name-last-macro)))
                   ;; pop the calling macro back
                   (kmacro-pop-ring1)
                   ;; put last-macro back (if there was one)
                   (if last-macro
                       (nconc kmacro-ring last-macro))
                   symbol))))
          (cond ((eq val 'quit) (setq quit-flag t))
                ((eq val 'continue) nil)
                ((eq val 'edit) (funcall editfunc))
                ((eq val 'branch)
                 (let* ((condition (read-from-minibuffer
                                    "Condition: " nil nil nil nil
                                    kmacro-decision-conditions))
                        (action (kmacro-decision-menu t))
                        (resetmacro "(let* ((calling-kbd-macro executing-kbd-macro) (executing-kbd-macro nil)) ")
                        (revertmacro " (setq executing-kbd-macro calling-kbd-macro) (message nil))")
                        (actioncode
                         (concat
                          (cond ((eq action 'quit) "(keyboard-quit)")
                                ((eq action 'continue) "t")
                                ((eq action 'edit)
                                 (concat resetmacro "(funcall '"
                                         (prin1-to-string (funcall editfunc))
                                         ")" revertmacro))
                                ((eq action 'useredit 111))
                                ((eq action 'form)
                                 (concat resetmacro 
                                         (read-from-minibuffer
                                          "Elisp: " nil read-expression-map nil
                                          'read-expression-history)
                                         revertmacro))
                                ((eq action 'command)
                                 (concat resetmacro "(call-interactively '"
                                         (symbol-name (read-command "Command : ")) ")"
                                         revertmacro))
                                ((symbolp action)
                                 (concat resetmacro "(funcall '" (symbol-name action) ")" revertmacro)))
                          (unless (or (member action '(quit continue))
                                      (y-or-n-p "Continue with macro after performing this action?"))
                            " (keyboard-quit)")))
                        (pre (subseq calling-kbd-macro 0 executing-kbd-macro-index))
                        (condexists (and (> (length pre) 33)
                                         (equal (string-to-vector "(t (kmacro-decision)))")
                                                (subseq pre -26))))
                        (post (subseq calling-kbd-macro executing-kbd-macro-index))
                        (condcode
                         (concatenate 'vector
                                      (unless condexists
                                        (concatenate 'vector (kbd "M-:") "(cond "))
                                      "(" condition " " actioncode ") "
                                      "(t (kmacro-decision)))")))
                   (setq pre (if condexists (subseq pre 0 -26) (concatenate 'vector pre "")))
                   (setq last-kbd-macro (concatenate 'vector pre condcode post))))
                ((and val (symbolp val)) (funcall val))
                (t (message nil))))))))

(defun kmacro-decision-named-macros nil
  "Return list of all named keyboard macros."
  (cl-loop for elt being the symbols
           if (and (fboundp elt)
                   (or (stringp (symbol-function elt))
                       (vectorp (symbol-function elt))
                       (get elt 'kmacro)))
           collect elt))

;;;###autoload
(defun kmacro-decision-name-last-macro (symbol)
  "Like the builtin `kmacro-name-last-macro' but with better prompt for interactive use, and it returns the symbol.

Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid editor command."
  (interactive (let* ((usednames (mapcar 'symbol-name (kmacro-decision-named-macros)))
                      (defaultname (jb-untilnext "kbd-macro-1"
                                                 (progn (setq num (1+ num))
                                                        (concat "kbd-macro-" (number-to-string num)))
                                                 (lambda (x) (not (member x usednames)))
                                                 (num 1)))
                      (newname (jb-untilnext
                                (read-string "Name for last kbd macro: " defaultname)
                                (read-string "Symbol already used! Choose another name: " defaultname)
                                '(lambda (x) (and (not (string-equal x ""))
                                                  (let ((sym (intern-soft x)))
                                                    (or (not sym)
                                                        (and (fboundp sym)
                                                             (or (get sym 'kmacro)
                                                                 (stringp (symbol-function sym))
                                                                 (vectorp (symbol-function sym)))
                                                             (y-or-n-p "Overwrite existing kbd macro with this name?")))))))))
                 (list (intern newname))))
  (or last-kbd-macro (error "No keyboard macro defined"))
  (fset symbol (kmacro-lambda-form (kmacro-ring-head)))
  (put symbol 'kmacro t)
  symbol)

(defun* kmacro-decision-menu (&optional withcond)
  "Prompt the user for an action to perform at a query point in a keyboard macro.
If WITHCOND is non-nil then prompt for an action to perform for the previously entered
condition.
This function returns one of the following symbols 'continue, 'edit, 'new, 'branch, 'quit
or a symbol corresponding to a named keyboard macro."
  (let* ((kmacros (reverse (kmacro-decision-named-macros)))
         (nmacros (1- (length kmacros)))
         (prompts (append (list "Recenter window about cursor"
                                "Continue executing macro"
                                "Recursive edit now (C-M-c to finish)")
                          (if withcond '("Recursive edit when called" "Eval elisp" "Execute command") '("Add conditional branch"))
                          (mapcar (lambda (k) (format "%s" k)) kmacros)))
         (keys (append (list (kbd "C-l") (kbd "SPC") (kbd "RET")) (if withcond '("r" "e" "x") '("?"))))
         (forms (append '((recenter-top-bottom) 'continue 'edit)
                        (if withcond '('useredit 'form 'command) '('branch))
                        kmacros)))
    (jb-read-key-menu prompts forms (concat "Choose action to perform"
                                            (if t " when condition is non-nil:\n" ":\n")) nil keys)))

;;;###autoload
(defalias 'kbd-macro-query 'kmacro-decision
  (documentation 'kmacro-decision))

;;;###autoload
(defalias 'kmacro-name-last-macro 'kmacro-decision-name-last-macro
  (documentation 'kmacro-decision-name-last-macro))

(provide 'kmacro-decision)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "kmacro-decision.el" (buffer-name) (buffer-string) "update")

;;; kmacro-decision.el ends here
