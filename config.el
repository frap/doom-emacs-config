;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq
 user-full-name "Andrés Gasson"
 user-mail-address "gas@troveplatform.co.nz"
 github-account-name "frap")



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/logseq/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; --------------------------------------------------
;; Practicalli Configuration
;; Search https://discourse.doomemacs.org/ for example configuration

(setq doom-theme (if (equal (system-name) "Cable_Guy") 'modus-operandi 'doom-1337))
;; Practicalli Logo on startup dashboard
(setq fancy-splash-image "~/.config/doom/images/practicalli-logo-dark.svg")

;; Open Doom Emacs maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
;; - add current workspace name
;; - add major mode icon
(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit (- fill-column 10)))

;;undo using tree
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)
;; Projects
;; Define a project path to discover projects using SPC Tab D
;; https://docs.projectile.mx/projectile/usage.html
;; (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
(setq projectile-project-search-path '(("~/work" . 2)  ("~/.config" . 1) ("~/dev/frap" . 3)))

;; Disable projectile cache - saves requirement to invalidate cache when moving files
(setq ;; projectile-enable-caching nil
 projectile-sort-order 'recentf )

;; GNU TRAMP Configuration
(setq tramp-default-method "ssh"                         ; Default to SSH, that's what I primarily use
      tramp-terminal-type "tramp"                        ; Let other terminal know what client I'm connecting with (might need to configure server)
      tramp-auto-save-directory "$XDG_CACHE_HOME/tramp/" ; Send Tramp info into XDG Cache directory on machine
      tramp-chunksize 2000)                              ; Resonable Chunk size for speed and disk space in mind

;;
;; Delete whitespace on save, including in markdow-mode
(setq ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode diff-mode))

;; Completion results order by history of use and then alphabetical
;; - does not work for autocomplete popups
;; (setq vertico-sort-function 'vertico-sort-history-alpha)

;; ---------------------------------------


;; ---------------------------------------
;; Additional Configuration

;; Which-key and Evil Key Bindings - Spacemacs style
(load! "+bindings-emacs")

;; LSP Configuration
(load! "+lsp")

;; Magit and Version Control
(load! "+git")

;; Markdown mode
;;(load! "+markdown.el")

;; Org-mode  configuraiton
(load! "+org-mode")

;; general coding like AI!
(load! "+coding")

;; Clojure mode & Cider Configuration + key bindings
(load! "+clojure")

;; Typescript and treesitter
(load! "+javascript")

;; Configure packages outside of Doom modules
;; - keycast
;;(load! "+package-config")

;; ---------------------------------------
