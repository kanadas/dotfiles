;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tomasz Kanas"
      user-mail-address "kanas.tomasz@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

 (setq doom-theme 'doom-acario-dark)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-Iosvkem)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'doom-sourcerer)
;; (setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; some useful defaults
(setq compile-history "make -k ")
(setq auto-save-default t)
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Autoask which buffer after split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)
;; enable c-default-style
(setq-default c-basic-offset 'set-from-style)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "why3.el")

;;C, C++, Objective-C, Java, etc.
(after! cc-mode
  (setq
   c-default-style '((c-mode . "linux") (c++-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))
   )
  (defun my-open-block-c-mode (id action context)
    (when (eq action 'insert)
      (newline)
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode)))
  (sp-local-pair 'cc-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
  (load "/usr/share/clang/clang-format.el")
  (define-key c-mode-base-map (kbd "C-c F") 'clang-format-region)
  (define-key c-mode-base-map (kbd "C-c f") 'clang-format-buffer)
  (setq clang-format-style-option "file")
  )

(after! rtags
  (map! "M-?" #'rtags-find-symbol-at-point)
  (map! "M-/" #'rtags-find-references-at-point)
  )

;;TODO check if it works
;;(after! lsp-java
;;  (setq lsp-java-content-provider-preferred "fernflower")
;;  )

(after! flycheck
  (add-hook 'flycheck-error-list-mode-hook 'visual-line-mode)
  )

;;LATEX
(after! latex
  (defun TeX-insert-single-quote (arg)
    (interactive "p")
    (cond
     (mark-active
      (let ((skeleton-end-newline nil))
        (skeleton-insert `(nil ?` _ ?') -1)))
     ((or (looking-at "\\<")
          (looking-back "^\\|\\s-\\|`"))
      (insert "`"))
     (t
      (self-insert-command arg))))
  (defun my-latex-mode-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands)) company-backends))
    )
  ;; (setq +latex-viewers '(pdf-tools)) ; commment this line to use zathura as latex viewer
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (local-set-key "'" 'TeX-insert-single-quote)
               ))
  (defadvice TeX-insert-quote (around wrap-region activate)
    (cond
     (mark-active
      (let ((skeleton-end-newline nil))
        (skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
     ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
      (forward-char (length TeX-open-quote)))
     (t ad-do-it)
     )
    )
  (put 'TeX-insert-quote 'delete-selection nil)
  (add-hook 'TeX-mode-hook 'my-latex-mode-setup)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook '(lambda ()
                              (add-hook 'after-save-hook '(lambda ()
                                                            (TeX-command "LaTeX" 'TeX-master-file)
                                                            )
                                        )
                              )
            )
  ;;smartparens
  (sp-local-pair 'LaTeX-mode "\\left(" "\\right)" :trigger "\\l(")
  (sp-local-pair 'LaTeX-mode "\\left[" "\\right]" :trigger "\\l[")
  (sp-local-pair 'LaTeX-mode "[" ")")
  (sp-local-pair 'LaTeX-mode "(" "]")
  )

;; COQ
(after! coq
  (define-key proof-mode-map (kbd "<M-return>") 'proof-assert-next-command-interactive)
  (local-set-key (kbd "M-u") 'proof-undo-last-successful-command)
  (setq coq-accept-proof-using-suggestion 'always)
  (set-face-background proof-locked-face "190033")
  )

(after! company-coq
  (define-key company-coq-map (kbd "<M-return>") nil)
  (define-key outline-mode-map (kbd "<normal-state> <M-return>") nil)
  )
