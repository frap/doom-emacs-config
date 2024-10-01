;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq
 user-full-name "Andrés Gasson"
 user-mail-address "gas@troveplatform.co.nz"
 github-account-name "frap")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-personal/")

;; --------------------------------------------------
;; Gas Doom Configuration
;; Search https://discourse.doomemacs.org/ for example configuration

;; Load Gas UI enhancements

(load! "+ui")

;;Load basic edit file stuff
(load! "+editor")

;; Org-mode  configuraiton
(load! "+org-mode")
(load! "+org-present")

;; general data coding yaml and copilot and parens
(load! "+coding")
;; LSP Configuration
(load! "+lsp")

;; Clojure mode & Cider Configuration + key bindings
(load! "+clojure")

;; Typescript and treesitter
(load! "+javascript")

;; Magit and tramp
(load! "+cmd-tools")

;; my Emacs Key Bindings
(load! "+bindings-emacs")

;; Configure packages outside of Doom modules
;; - keycast
(load! "+package-config")
