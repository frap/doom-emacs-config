;;; $DOOMDIR/+markdown.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Markdown
;;
;; Doom Markdown configuration
;; ~/.config/emacs-doom/modules/lang/markdown/config.el


;; ---------------------------------------
;; Markdown key bindings

;; Changes
;; - move toggle prefix from `t' to `T'
;; - add table prefix `t'



(use-package! org-protocol
  :after org
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-find-file" :protocol "find-file" :function org-protocol-find-file :kill-client nil))
  (defun org-protocol-find-file (fname)
    "Process org-protocol://find-file?path= style URL."
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file f)
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))
