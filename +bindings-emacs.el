;;; ../../dev/emacs/practicalli-doom/+bindings-emacs.el -*- lexical-binding: t; -*-

;;;; window
(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

;;;; unbind-key
(global-unset-key (kbd "C-z")) ; unbind (suspend-frame)
(global-unset-key (kbd "C-x C-z")) ; also this

;; normal undo and redo
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; global-emacs bindings
(if IS-MAC (setq
            mac-right-command-modifier 'nil
            mac-command-modifier 'super
            mac-option-modifier 'meta
            mac-right-option-modifier 'nil
            ;;   mac-pass-control-to-system nil ;; what does this do?
            ))


;; ;;; Replace
(define-key esc-map "&"       'query-replace-regexp) ; redefined ESC-&.
;; (bind-key "M-<tab>"           'company-complete-common-or-cycle)
;;(bind-key "M-#"               'query-replace-regexp)
(bind-key "M-\""              'insert-pair)	; wrap text in quotes.

(use-package! crux)
;; Key binding vars
;;
;;global settings
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
;;(map! "C-x C-f" #'counsel-find-file
;;      "C-x C-c" #'save-buffers-kill-terminal)

(defun cut-region (beg end)
  "Copies the text to the kill buffer and deletes the selected region."
  (interactive "r")
  (copy-region-as-kill beg end)
  (delete-region beg end))

(global-set-key (kbd "s-x") 'cut-region)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-e") 'eval-region)
(global-set-key (kbd "s-b") 'eval-buffer)
(global-set-key (kbd "s-c") 'ns-copy-including-secondary)
;;clipboard yank
;;(global-set-key (kbd "M-v") 'clipboard-yank)

;; Activate occur easily inside isearch
;;(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x ;") 'uncomment-region)
;; replace buffer-menu with ibuffer
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Magit creates some global keybindings by default
;; but it's a nice to complement them with this one
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;;;; text-modification
(bind-key "M-Q"               'unfill-paragraph)
(bind-key "C-x C-z"           'toggle-truncate-lines) ; long lines go off the screen
(bind-key "C-S-R"             'rename-file)
(bind-key "C-c D"             'delete-current-file-and-buffer)

;;;; Buffers
(global-set-key [C-left] 'previous-buffer)
(global-set-key [C-right] 'next-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)

;;;; window
(bind-key "C-,"               'prev-window)
(bind-key "C-."               'other-window)
;;(bind-key "C-x 3"             'split-and-follow-vertically)
;;(bind-key "C-x 2"             'split-and-follow-horizontally)


;;; Interactive-bindings
;;;; resume/run previous cmnd
(bind-key "C-r"
          #'(lambda () (interactive)
              (eval (car command-history))))

;;;smartparens
(map!
 (:after smartparens
         (:map smartparens-mode-map
               [C-M-a]   #'sp-beginning-of-sexp
               [C-M-e]   #'sp-end-of-sexp
               [C-M-f]   #'sp-forward-sexp
               [C-M-b]   #'sp-backward-sexp
               [C-M-k]   #'sp-kill-sexp
               [C-M-t]   #'sp-transpose-sexp

               [C-<right>] #'sp-forward-slurp-sexp
               [C-<left>]  #'sp-forward-barf-sexp

               [M-left]    #'sp-beginning-of-sexp
               [M-right]   #'sp-end-of-sexp
               [M-up]      #'sp-backward-up-sexp
               [M-down]    #'sp-backward-down-sexp
               [s-down]    #'sp-down-sexp
               [s-up]      #'sp-up-sexp
               [s-left]    #'sp-backward-sexp
               [s-right]   #'sp-forward-sexp
               [M-s-right] #'sp-next-sexp
               [M-s-left]  #'sp-previous-sexp
               )))

;;;;; transpose
(bind-key "M-t" nil) ; remove the old keybinding
(bind-key "M-t c"             'transpose-chars)
(bind-key "M-t w"             'transpose-words)
(bind-key "M-t t"             'transpose-words)
(bind-key "M-t M-t"           'transpose-words)
(bind-key "M-t l"             'transpose-lines)
(bind-key "M-t e"             'transpose-sexps)
(bind-key "M-t s"             'transpose-sentences)
(bind-key "M-t p"             'transpose-paragraphs)
