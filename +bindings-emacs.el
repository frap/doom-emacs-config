;;; ../../dev/emacs/practicalli-doom/+bindings-emacs.el -*- lexical-binding: t; -*-

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

;; global-emacs bindings
(if IS-MAC (setq
             mac-right-command-modifier 'nil
             mac-command-modifier 'super
             mac-option-modifier 'meta
             mac-right-option-modifier 'nil
          ;;   mac-pass-control-to-system nil ;; what does this do?
             ))

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
