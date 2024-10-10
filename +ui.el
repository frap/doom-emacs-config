;;; package --- +ui.el -*- lexical-binding: t; -*-

;;; Commentary:
(setq doom-theme (if (equal (system-name) "Cable_Guy") 'modus-vivendi-tinted 'doom-1337))
;; Practicalli Logo on startup dashboard
(setq fancy-splash-image "~/.config/doom/images/practicalli-logo-dark.svg")


(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Open Doom Emacs maximised
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
;; - add current workspace name
;; - add major mode icon
(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit (- fill-column 10)))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Completion results order by history of use and then alphabetical
;; - does not work for autocomplete popups
;; (setq vertico-sort-function 'vertico-sort-history-alpha)


(provide '+ui)
