;;; org-auctex-keys.el --- Support of many AUCTeX key bindings for Org documents

;; Copyright (C) 2013 Fabrice Niessen

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; URL: https://github.com/fniessen/org-auctex-key-bindings
;; Version: 20131208.2232
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

;; This minor mode implements many AUCTeX key bindings in Org documents!
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

(defcustom org-auckeys-save-query t
  "*If non-nil, ask user for permission to save files before exporting."
  :group 'Org-AUCTeX-keys
  :type 'boolean)

;;---------------------------------------------------------------------------
;; internal variables

(defvar org-auctex-keys-minor-mode nil
  "Non-nil if using \"Org AUCTeX Keys\" mode as a minor mode of some other mode.
Use the command `org-auctex-keys-minor-mode' to toggle or set this variable.")

(defvar org-auctex-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'org-auckeys-insert-heading-respect-content)
    (define-key map (kbd "C-c C-j") 'org-auckeys-insert-heading)
    (define-key map (kbd "C-c C-f") 'org-auckeys-font)
    (define-key map (kbd "C-c C-e") 'org-auckeys-environment)
    (define-key map (kbd "C-c C-c") 'org-auckeys-export-dispatch)
    map)
  "Keymap for Org AUCTeX Keys minor mode.")

(easy-menu-define org-auctex-keys-minor-mode-menu org-auctex-keys-minor-mode-map
  "Menu used when Org AUCTeX Keys minor mode is active."
  '("AUCKeys"
    ["Section" org-auckeys-insert-heading-respect-content]
    ["Insert Item" org-auckeys-insert-heading]
    ("Insert Font"
     ["Emphasize"  (org-auckeys-font ?\C-e) :keys "C-c C-f C-e"]
     ["Bold"       (org-auckeys-font ?\C-b) :keys "C-c C-f C-b"]
     ["Typewriter" (org-auckeys-font ?\C-t) :keys "C-c C-f C-t"]
     ["Italic"     (org-auckeys-font ?\C-i) :keys "C-c C-f C-i"])
    ["Insert environment" org-auckeys-environment]
    ["Export to LaTeX / View" org-auckeys-export-dispatch]
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

(defun org-auckeys-insert-heading (&optional arg)
  "Insert a new heading or item with same depth at point.

With a prefix argument, use the standard command bound to `C-c C-s'."
  (interactive "p")
  (if (= arg 1)
      (org-insert-heading)
    ;; original function
    (funcall 'org-schedule (/ arg 4))))

(defun org-auckeys-insert-heading-respect-content (&optional arg)
  "Insert heading with `org-insert-heading-respect-content' set to t.

With a prefix argument, use the standard command bound to `C-c C-j'."
  (interactive "p")
  (if (= arg 1)
      (org-insert-heading-respect-content)
    ;; original function
    (funcall 'org-goto (/ arg 4))))

(defun org-auckeys-describe-font-entry (entry)
  "A textual description of an ENTRY in `org-auckeys-font-list'."
  (concat (format "%16s  " (key-description (char-to-string (nth 0 entry))))
          (format "%8s %4s"
                  (nth 1 entry)
                  (nth 2 entry))))

(defun org-auckeys-font (what &optional arg)
  "Insert template for font change command.

WHAT determines the font to use, as specified by `org-auckeys-font-list'.

With a prefix argument, use the standard command bound to `C-c C-f'."
  (interactive "*c\np")
  (let* ((entry (assoc what org-auckeys-font-list))
         (before (nth 1 entry))
         (after (nth 2 entry)))
    (if (= arg 1)
        (cond ((null entry)
               (let ((help (concat
                            "Font list:   "
                            "KEY        TEXTFONT\n\n"
                            (mapconcat 'org-auckeys-describe-font-entry
                                       org-auckeys-font-list "\n"))))
                 (with-output-to-temp-buffer "*Help*"
                   (set-buffer "*Help*")
                   (insert help))))
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
                 (insert after))))
      ;; original function
      (funcall 'org-forward-heading-same-level (/ arg 4)))))

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

(defun org-auckeys-environment (&optional arg)
  "Insert TEMPLATE at point.

Interactively, TEMPLATE is an element from `org-structure-template-alist'.

With a prefix argument, use the standard command bound to `C-c C-e'."
  (interactive "p")
  (if (= arg 1)
      (let ((template
             (org-auckeys-completing-read
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
    ;; original function
    (funcall 'org-export-dispatch (/ arg 4))))

(defun org-auckeys-export-dispatch (&optional arg)
  "Export to PDF or open the PDF file, depending on whether the file is up to date.

With a prefix argument, use the standard command bound to `C-c C-c'."
  (interactive "p")
  (let* ((orgfile (buffer-file-name))
         (pdffile (concat (file-name-base orgfile) ".pdf")))
    (if (= arg 1)
        (progn
          ;; save buffer
          (and (buffer-modified-p)
               (or (not org-auckeys-save-query)
                   (y-or-n-p (concat "Save file " orgfile "? ")))
               (save-excursion
                 (save-buffer)))
          ;; export or open
          (cond ((or (not (file-exists-p pdffile))
                     (file-newer-than-file-p orgfile pdffile))
                 (message "Command: Export to LaTeX/PDF")
                 (sit-for 0.5)
                 (when (file-exists-p pdffile)
                   (delete-file pdffile)) ; Avoid false message "All targets are
                                          ; up-to-date" (from Latexmk) when PDF
                                          ; file is still open in Acrobat Reader!
                 (org-latex-export-to-pdf)
                 (message "Export to LaTeX/PDF: Process completed."))
                (t
                 (message "Command: View")
                 (sit-for 0.5)
                 (org-open-file pdffile)
                 (message ""))))
      ;; original function
      (funcall 'org-ctrl-c-ctrl-c (/ arg 4)))))

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
