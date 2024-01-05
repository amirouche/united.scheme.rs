(require 'package)

;; todo
;;
;; - bind marks commands

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; elpy
;; http://elpy.readthedocs.org/en/latest/concepts.html
;;
;; bindings:
;; - M-. jump-to-def
;; - M-* back from last jump to def
;; - C-c C-c send buffer to interpreter

;; Customize dired

(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$" ".+\\.pyc"))

;; Bookmarks
(require 'bm)
(global-set-key (kbd "C-c b") 'bm-toggle)
(global-set-key (kbd "C-c n")   'bm-next)


;; Handy function for azoufzouf, insert yaz command character

(defun yaz ()
  (interactive)
  (insert "ⵣ"))

(global-set-key (kbd "C-x y") 'yaz)

(global-set-key (kbd "C-x C-r") 'ag-project)
(global-set-key (kbd "C-x r") 'ag)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Pretty print xml

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; Compile buffer

;; accept ansi escape sequence in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(global-set-key (kbd "C-x C-g") 'compile)

(setq-default indent-tabs-mode nil)

;; replace "yes or no" question by "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)
