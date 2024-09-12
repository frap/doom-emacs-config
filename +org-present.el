;;; ../../dev/emacs/practicalli-doom/+org-present.el -*- lexical-binding: t; -*-
(defvar gas/fixed-width-font "JetBrains Mono"
  "The font to use for monospaced (fixed width) text.")

(defvar gas/variable-width-font "Iosevka Aile"
  "The font to use for variable-pitch (document) text.")

;; NOTE: These settings might not be ideal for your machine, tweak them as needed!
(set-face-attribute 'default nil :font gas/fixed-width-font :weight 'light :height 130)
(set-face-attribute 'fixed-pitch nil :font gas/fixed-width-font :weight 'light :height 140)
(set-face-attribute 'variable-pitch nil :font gas/variable-width-font :weight 'light :height 1.3)

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font gas/variable-width-font :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font gas/variable-width-font :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; Centering Org Documents --------------------------------

;; Configure fill width
(setq visual-fill-column-width 120
      visual-fill-column-center-text t)


(defun gas/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun gas/org-present-start ()
  ;; Load up doom-palenight for the System Crafters look
  (load-theme 'doom-palenight t)

  ;; hide uneeded UI elements
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)

  (display-line-numbers-mode -1)
  ;; remove mouse cursor form highlighting
  (lambda () (remove-text-properties
              (point-min) (point-max) '(mouse-face t)))

  ;; let the desktop background show through
  (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

  ;; turn off highlights
  (setq global-hl-line-modes '(prog-modes markdown-mode))
  (custom-set-faces
   '(default ((t (:background "#000000"))))
   '(hl-line ((t (:background "#000000"))))
   )
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 5.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)
  (setq org-image-align :center)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun gas/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  (menu-bar-mode 1)
  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; Turn on variable pitch fonts in Org Mode buffers
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'gas/org-present-start)
(add-hook 'org-present-mode-quit-hook 'gas/org-present-end)
(add-hook 'org-present-after-navigate-functions 'gas/org-present-prepare-slide)


;; reveal
(load-library "ox-reveal")
;; Wrap <img> tag in a <figure> tag
(setq org-html-html5-fancy t
      org-html-doctype "html5")
