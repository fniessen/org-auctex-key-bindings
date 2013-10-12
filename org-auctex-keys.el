;;; org-auctex-keys.el --- Support of many AUCTeX key bindings for Org documents

;; Copyright (C) 2013 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/org-auctex-key-bindings
;; Version: 20131012.1039
;; Keywords: org mode, latex, auctex, key bindings, shortcuts, emulation

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor modes implements many AUCTeX key bindings in Org documents!
;; ... a killer feature to ease the conversion of LaTeX users to Org mode.
;;
;; To use it, put the following in your Emacs configuration file:
;;
;;   (require 'org-auctex-keys)
;;
;; and enable it for any Org file with `M-x org-auctex-keys-minor-mode'.
;;
;; To get it automagically turned on in every Org buffer, add this in your
;; Emacs configuration file:
;;
;;   (add-hook 'org-mode-hook 'org-auctex-keys-minor-mode)
;;
;; Credit: Original idea from Denis Bitouzé.
;;
;; Thanks to Nicolas Richard for a patch of `org-auckeys-environment'!

;;; Code:

(require 'org)

;;---------------------------------------------------------------------------
;; user-configurable variables


;;---------------------------------------------------------------------------
;; internal variables

(defvar org-auctex-keys-minor-mode nil
  "Non-nil if using \"Org AUCTeX Keys\" mode as a minor mode of some other mode.
Use the command `org-auctex-keys-minor-mode' to toggle or set this variable.")

(defvar org-auctex-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'org-insert-heading)
    (define-key map (kbd "C-c C-j") 'org-insert-heading-respect-content)
    (define-key map (kbd "C-c C-f") 'org-auckeys-font)
    (define-key map (kbd "C-c C-e") 'org-auckeys-environment)
    (define-key map (kbd "C-c C-c") 'org-auckeys-export-dispatch)
    map)
  "Keymap for Org AUCTeX Keys minor mode.")

(easy-menu-define org-auctex-keys-minor-mode-menu org-auctex-keys-minor-mode-map
  "Menu used when Org AUCTeX Keys minor mode is active."
  '("AUCKeys"
    ["Section" org-insert-heading]
    ["Insert Item" org-insert-heading-respect-content]
    ("Insert Font"
     ["Emphasize"  (org-auckeys-font nil ?\C-e) :keys "C-c C-f C-e"]
     ["Bold"       (org-auckeys-font nil ?\C-b) :keys "C-c C-f C-b"]
     ["Typewriter" (org-auckeys-font nil ?\C-t) :keys "C-c C-f C-t"]
     ["Italic"     (org-auckeys-font nil ?\C-i) :keys "C-c C-f C-i"])
    ["Insert environment" org-auckeys-environment]
    ["Export..." org-export-dispatch]
    ))

(defvar org-auckeys-font-list
  '((?\C-e "/" "/")
    (?\C-b "*" "*")
    (?\C-i "/" "/")
    (?\C-t "=" "="))
  "Font commands used with org-auckeys.")

(defvar org-auckeys-mode-text " AUCKeys")

;;---------------------------------------------------------------------------
;; commands

;;;###autoload
(define-minor-mode org-auctex-keys-minor-mode
  "Minor mode to keep you efficient editing Org mode documents with AUCTeX key bindings."
  :group 'org-auctex-keys
  :lighter org-auckeys-mode-text
  :keymap org-auctex-keys-minor-mode-map
  (if org-auctex-keys-minor-mode
      (message "Org AUCTeX Keys minor mode enabled")
    (message "Org AUCTeX Keys minor mode disabled")))

;;;###autoload
(defun turn-off-org-auctex-keys ()
  (interactive)
  "Unconditionally turn off `org-auctex-keys-minor-mode'."
  (org-auctex-keys-minor-mode -1))

;;;###autoload
(defun turn-on-org-auctex-keys ()
  "Unconditionally turn off `org-auctex-keys-minor-mode'."
  (org-auctex-keys-minor-mode 1))

;;;###autoload
(defun toggle-org-auctex-keys ()
  "Toggle AUCKeys on/off.
If AUCKeys is enabled, turn it off.  Otherwise, turn it on."
  (interactive)
  (if org-auctex-keys-minor-mode
      (turn-off-org-auctex-keys)
    (turn-on-org-auctex-keys)))

;; make `C-+' toggle AUCKeys on/off
(define-key org-mode-map
  (kbd "C-+") 'toggle-org-auctex-keys)

(defun org-auckeys-describe-font-entry (entry)
  "A textual description of an ENTRY in `org-auckeys-font-list'."
  (concat (format "%16s  " (key-description (char-to-string (nth 0 entry))))
          (format "%8s %4s"
                  (nth 1 entry)
                  (nth 2 entry))))

(defun org-auckeys-font (replace what)
  "Insert template for font change command.

If REPLACE is not nil, replace current font.  WHAT determines the font
to use, as specified by `org-auckeys-font-list'."
  (interactive "*P\nc")
  (let* ((entry (assoc what org-auckeys-font-list))
         (before (nth 1 entry))
         (after (nth 2 entry)))
    (cond ((null entry)
           (let ((help (concat
                        "Font list:   "
                        "KEY        TEXTFONT\n\n"
                        (mapconcat 'org-auckeys-describe-font-entry
                                   org-auckeys-font-list "\n"))))
             (with-output-to-temp-buffer "*Help*"
               (set-buffer "*Help*")
               (insert help))))
          ;; (replace
          ;;  (funcall org-auckeys-font-replace-function before after))
          ((region-active-p)
           (save-excursion
             (cond ((> (mark) (point))
                    (insert before)
                    (goto-char (mark))
                    (insert after))
                   (t
                    (insert after)
                    (goto-char (mark))
                    (insert before)))))
          (t
           (insert before)
           (save-excursion
             (insert after))))))

(defun org-auckeys-completing-read (prompt collection &optional print-fun &rest other)
  "Read a string in the minibuffer, with completion.

COLLECTION can be any list of Lisp objects.

PRINT-FUN is used to show them in the minibuffer prompt -- by default, this is
`prin1-to-string'."
  (let ((print-fun (or print-fun #'prin1-to-string))
        (collection (mapcar
                     (lambda (choice)
                       (cons (funcall print-fun choice) choice))
                     collection)))
    (cdr
     (assoc
      (apply #'completing-read prompt (mapcar #'car collection) other)
      collection))))

(defun org-auckeys-environment (template)
  "Insert TEMPLATE at point.

Interactively, TEMPLATE is an element from `org-structure-template-alist'."
  (interactive (list (org-auckeys-completing-read
                      "Environment: "
                      org-structure-template-alist
                      (lambda (cell)
                        (let ((template (cadr cell)))
                          (if (string-match "\\`[^ \n]+" template)
                              (match-string 0 template)
                            template))))))
  (if template
      (org-complete-expand-structure-template (point) template)
    (message "Template not found in `org-structure-template-alist'")))

(defun org-auckeys-export-dispatch ()
  "Save buffer, execute/tangle code blocks, and export to HTML/PDF."
  (interactive)
  (let* ((orgfile (buffer-file-name))
         (pdffile (concat (file-name-base orgfile) ".pdf")))

    ;; ;; XXX query to save?
    ;; (save-buffer)

    (cond ((or (not (file-exists-p pdffile))
               (file-newer-than-file-p orgfile pdffile))
           (message "Command: Export to LaTeX")
           (sit-for 0.5)
           (org-latex-export-to-pdf)
           (message "Export to LaTeX: done"))
          (t
           (message "Command: View")
           (sit-for 0.5)
           (org-open-file pdffile)
           (message "")))))

;;---------------------------------------------------------------------------
;; that's it

(provide 'org-auctex-keys)

;; Local Variables:
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: "Version: "
;; time-stamp-end: "$"
;; no-byte-compile: t
;; End:

;;; org-auctex-keys.el ends here
