;;; ../../dev/emacs/practicalli-doom/+bindings-emacs.el -*- lexical-binding: t; -*-

;;;; window
(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
            ))

(global-set-key [remap dabbrev-expand] #'hippie-expand)
;; ;;; Replace
;;(define-key esc-map "&"       'query-replace-regexp) ; redefined ESC-&.
;; (bind-key "M-<tab>"           'company-complete-common-or-cycle)
;;(bind-key "M-#"               'query-replace-regexp)
(bind-key "M-\""              'insert-pair)	; wrap text in quotes.

;;(use-package! crux)
;; Key binding vars
;;
;;global settings
;; (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
;; (global-set-key (kbd "C-c o") #'crux-open-with)
;; (global-set-key [(shift return)] #'crux-smart-open-line)
;; (global-set-key (kbd "s-r") #'crux-recentf-find-file)
;; (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
;; (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
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
;;(bind-key "C-c D"             'delete-current-file-and-buffer)

;;;; Buffers
;;(global-set-key [C-left] 'previous-buffer)
;;(global-set-key [C-right] 'next-buffer)
                                        ;(global-set-key (kbd "M-n") 'next-buffer)
;;(global-set-key (kbd "M-p") 'previous-buffer)

;;;; window
(bind-key [C-left]        'prev-window)
(bind-key [C-right]       'other-window)
;;(bind-key "C-x 3"             'split-and-follow-vertically)
;;(bind-key "C-x 2"             'split-and-follow-horizontally)


;;; Interactive-bindings
;;;; resume/run previous cmd
(bind-key "C-r"
          #'(lambda () (interactive)
              (eval (car command-history))))

;;;smartparens
(use-package! smartparens
  :config
  ;; Enable smartparens strict mode for Clojure and Emacs Lisp
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

  ;; Load the default smartparens configuration
  (require 'smartparens-config)

  (map! :map smartparens-mode-map
        ;; Movement
        "C-M-f"   #'sp-forward-sexp
        "C-M-b"   #'sp-backward-sexp
        "C-M-u"   #'sp-backward-up-sexp
        "C-M-d"   #'sp-down-sexp
        "C-M-p"   #'sp-backward-down-sexp
        "C-M-n"   #'sp-up-sexp
        "C-M-k"   #'sp-kill-sexp
        "C-M-t"   #'sp-transpose-sexp

        ;; Slurping and barfing
        "C-)"         #'sp-forward-slurp-sexp
        "C-<right>"   #'sp-forward-slurp-sexp
        "C-("         #'sp-backward-slurp-sexp
        "C-M-<right>" #'sp-backward-slurp-sexp
        "M-)"         #'sp-forward-barf-sexp
        "C-<left>"    #'sp-forward-barf-sexp
        "M-("         #'sp-backward-barf-sexp
        "C-M-<left>"  #'sp-backward-barf-sexp

        ;; Wrapping
        "M-["     #'sp-wrap-square
        "M-{"     #'sp-wrap-curly
        "M-\""    #'sp-wrap-doublequote

        ;; Splicing
        "M-s"    #'sp-splice-sexp
        "M-S"    #'sp-split-sexp
        "M-j"    #'sp-join-sexp
        ))

;; A Spacemacs like Lisp state menu (without the transient state)

(map! :leader
      (:prefix ("k". "Smartparens")
       :desc "Slurp forward" "s" #'sp-forward-slurp-sexp
       :desc "Slurp backward" "S" #'sp-backward-slurp-sexp
       :desc "" "$"   #'sp-end-of-sexp
       (:prefix ("`" . "Hybrid"))
       :desc "Kill" "k" #'sp-kill-hybrid-sexp
       :desc "Push" "p" #'sp-push-hybrid-sexp
       :desc "Slurp" "s" #'sp-slurp-hybrid-sexp
       :desc "Transpose" "t" #'sp-transpose-hybrid-sexp
       :desc "Absorb" "a" #'sp-absorb-sexp
       :desc "Barf forward" "b" #'sp-forward-barf-sexp
       :desc "Barf backward" "B" #'sp-backward-barf-sexp
       :desc "Convoluted" "c" #'sp-convolute-sexp
       (:prefix ("d" . "Delete")
        :desc "Symbol" "s" #'sp-kill-symbol
        :desc "Symbol Backward" "S" #'sp-backward-kill-symbol
        :desc "Word" "w" #'sp-kill-word
        :desc "Word Backward" "W" #'sp-backward-kill-word
        :desc "Kill" "x" #'sp-kill-sexp
        :desc "Kill Backward" "X" #'sp-backward-kill-sexp)
       :desc "Splice" "e" #'sp-splice-sexp-killing-forward
       :desc "Splice Backward" "E" #'sp-splice-sexp-killing-backward
       :desc "Symbol Backward" "h" #'sp-backward-symbol
       :desc "Sexp Backward" "H" #'sp-backward-sexp
       :desc "Join" "j" #'sp-join-sexp
       :desc "Sexp Forward" "l" #'sp-forward-sexp
       :desc "Sexp Forward" "L" #'sp-forward-sexp
       :desc "Raise" "r" #'sp-raise-sexp
       :desc "Slurp" "s" #'sp-forward-slurp-sexp
       :desc "Slurp Backward" "S" #'sp-backward-slurp-sexp
       :desc "Transpose" "t" #'sp-transpose-sexp
       :desc "Up Backward" "U" #'sp-backward-up-sexp
       (:prefix ("w" . "Wrap")
        :desc "()" "(" #'sp-wrap-round
        :desc "{}" "{" #'sp-wrap-curly
        :desc "[]" "[" #'sp-wrap-square
        :desc "Round" "w" #'sp-wrap-round
        :desc "Curly" "c" #'sp-wrap-curly
        :desc "Square" "s" #'sp-wrap-square
        :desc "Unwrap" "u" #'sp-unwrap-sexp)
       :desc "Copy sexp" "y" #'sp-copy-sexp))

;;;;; transpose
;; (bind-key "M-t" nil) ; remove the old keybinding
;; (bind-key "M-t c"             'transpose-chars)
;; (bind-key "M-t w"             'transpose-words)
;; (bind-key "M-t t"             'transpose-words)
;; (bind-key "M-t M-t"           'transpose-words)
;; (bind-key "M-t l"             'transpose-lines)
;; (bind-key "M-t e"             'transpose-sexps)
;; (bind-key "M-t s"             'transpose-sentences)
;; (bind-key "M-t p"             'transpose-paragraphs)

;;; multiple-cursors

;; Remember `er/expand-region' is bound to M-2!
(global-set-key (kbd "C-M-j") 'mc/mark-all-dwim) ; both marked and unmarked region. multiple presses
;; disable cursors C-g first press unmarks regions. 2n press disables mc (M-x mc/keyboard-quit)

;; for continuous lines: Mark lines, the create cursors. Can be mid-line
(global-set-key (kbd "C-M-c") #'mc/edit-lines)

;; Expand region
(global-set-key (kbd "C-M-l") #'er/expand-region)
;; insert a new line C-j

;; Select region first, then create cursors
(global-set-key (kbd "C-M-/") #'mc/mark-all-like-this) ; select text 1st. finds all occurrences
(global-set-key (kbd "C-M-,") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-.") #'mc/mark-next-like-this)

;; Skip the match and move to next one
(global-set-key (kbd "C-M-<") #'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-M->") #'mc/skip-to-next-like-this)
