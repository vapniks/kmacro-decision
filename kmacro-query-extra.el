;;; kmacro-query-extra.el --- Extra query functions for emacs keyboard macros

;; Filename: kmacro-query-extra.el
;; Description: Extra query functions for emacs keyboard macros
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-05-15 05:04:08
;; Version: 0.1
;; Last-Updated: 2013-05-15 05:04:08
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/kmacro-query-extra
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
;;;;


;;; Installation:
;;
;; Put kmacro-query-extra.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'kmacro-query-extra)

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x auto-document

;;
;; All of the above can customized by:
;;      M-x customize-group RET kmacro-query-extra RET
;;

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
;; Finish `kbd-macro-decision-menu', and integrate with `one-key-read-list' if available.
;; Create `kbd-macro-condition' function for inserting a cond form in a kbd macro
;; (need to think of good user interface for this).
;;

;;; Require
(require 'el-x)

;;; Code:

(defun kbd-macro-decision nil
  "Query user for another kbd macro to execute during execution of current kbd macro.
If called while defining a kbd macro then a query point will be inserted into the
kbd macro which will ask the user for a named kbd macro to execute at that point.
After executing the named kbd macro the calling macro will continue execution.

You should define and name some macros first using `kmacro-start-macro' (C-x ( or f3),
and `kmacro-name-last-macro' (C-x C-k n)."
  (interactive)
  (if defining-kbd-macro
      nil
    ;; temporarily clear the currently executing macro
    (let* ((executing-kbd-macro nil)
           (defining-kbd-macro nil)
           ;; prompt the user for a choice
           (val (kbd-macro-decision-menu)))
      (cond ((functionp val) (funcall val))
            ((eq val 'quit) (setq quit-flag t)) 
            ((eq val 'continue) nil)
            ((eq val 'edit) (recursive-edit)) 
            ((eq val 'new)
             ;; Create a new macro.
             ;; Save the macro at the end of kmacro-ring since
             ;; it will be removed when we start recording the new macro.
             (let ((last-macro (last kmacro-ring))
                   (ringlen (length kmacro-ring)))
               ;; start recording the macro
               (kmacro-start-macro nil)
               ;; if the user tries to finish the macro just quit recursive-edit
               (dflet ((end-kbd-macro (x y) (exit-recursive-edit))
                       (kmacro-call-repeat-key nil))
                 (recursive-edit))
               ;; now we can end the macro
               (end-kbd-macro nil #'kmacro-loop-setup-function)
               ;; ignore empty macros, prompt for a name for others
               (if (or (not last-kbd-macro)
                       (and last-kbd-macro (= (length last-kbd-macro) 0)))
                   (message "Ignore empty macro")
                 (call-interactively 'kmacro-name-last-macro))
               ;; pop the calling macro back
               (kmacro-pop-ring1)
               ;; put last-macro back
               (nconc kmacro-ring last-macro)))))))

(defun kbd-macro-decision-menu nil
  "Prompt the user for a kbd macro using a keyboard menu."
  (let* ((kmacros (cl-loop for elt being the symbols
                           if (and (fboundp elt)
                                   (or (stringp (symbol-function elt))
                                       (vectorp (symbol-function elt))
                                       (get elt 'kmacro)))
                           collect elt))
         (prompt (concat "C-g : Quit
SPC : Continue
RET : Enter recursive edit (C-M-c to exit)
C-n : Define new macro (C-M-c to exit)
"
                         (loop for i from 0 to (1- (length kmacros))
                               for kmacro = (nth i kmacros)
                               concat (format "%c   : %s\n" (+ 97 i) kmacro))))
         (key (read-key prompt)))
    (cond ((= key 32) 'continue)
          ((= key 13) 'edit)
          ((= key 14) 'new)
          ((and (> key 96)
                (< key (+ 97 (length kmacros))))
           (symbol-function (nth (- key 97) kmacros)))
          (t 'quit))))


(provide 'kmacro-query-extra)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "kmacro-query-extra.el" (buffer-name) (buffer-string) "update")

;;; kmacro-query-extra.el ends here
