;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;; Packages

(eval-when-compile (require 'use-package))

(use-package diminish
  :ensure t
  )

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config (ivy-mode)
  )

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config (counsel-mode)
  )

(use-package swiper
  :ensure t
  :diminish swiper-mode
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 )
  )

(use-package company
  :ensure t
  :init
  (use-package company-math
    :ensure t
    :mode ("\\.tex\\'" . TeX-latex-mode)
    )
  (use-package company-octave
    :load-path "lisp/company-octave"
;    :mode ("\\.m\\'" . company-mode)
    )
  :config
  (global-company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  )

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode)
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )

(use-package tex                   ; auctex
;  :load-path "site-lisp/auctex/"
;  :defines (latex-help-cmd-alist latex-help-file)
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  
  :init
  (use-package magic-latex-buffer
    :disabled t
    :ensure t
    :mode (("\\.tex\\'" . TeX-latex-mode))
    )

  :preface
  (defun TeX-insert-single-quote (arg)
    (interactive "p")
    (cond
     (mark-active
      (let ((skeleton-end-newline nil))
	(skeleton-insert
	 `(nil ?` _ ?') -1)))
     ((or (looking-at "\\<")
	  (looking-back "^\\|\\s-\\|`"))
      (insert "`"))
     (t
      (self-insert-command arg))))
  (defun my-latex-mode-setup ()
    (setq-local company-backends
		(append '((company-math-symbols-latex company-latex-commands)) company-backends))
    )
  
  :config
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
  (setq TeX-view-program-selection
	'(
	  ((output-dvi has-no-display-manager) "dvi2tty")
	  ((output-dvi style-pstricks) "dvips and gv")
	  (output-dvi "xdvi")
	  (output-pdf "PDF Tools")
	  (output-html "xdg-open")
	  )
	)
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
  )

(use-package pdf-tools
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown")
  )

(use-package ess
  :ensure t
  )

(use-package poly-markdown
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
	 ("\\.rmd\\'" . poly-markdown+r-mode)
	 )
  :preface
  (defun polymode-eval-and-switch-region ()
    (interactive)
    (polymode-eval-region-or-chunk)
    (polymode-next-chunk 1)
    )
  :bind (:map polymode-mode-map
	      ("M-<RET>" . polymode-eval-and-switch-region)
	      )
  :init
  (use-package poly-R
    :ensure t
    )
  )

(use-package ein
  :ensure t
  :commands (ein:jupyter-server-start ein:notebooklist-open)
  :mode (("\\.ipynb\\'" . ein:ipynb-mode))
  :preface
  (setq ein:worksheet-enable-undo t)
  (setq ein:polymode t)
  :config
  (elpy-enable)
  )

(use-package elpy
  :ensure t
  :commands (pyvenv-workon elpy-enable)
  :mode (("\\.py\\'" . python-mode))
  :config
  (elpy-enable)
  (setenv "WORKON_HOME" "/home/tkanas/.emacs.d/elpy")
  (pyvenv-workon "global")
  )

(use-package octave-mode
  :mode (("\\.m\\'". octave-mode))
  :preface
  (setq octave-auto-indent 1)
  :bind (:map octave-mode-map
	      ("M-<RET>" . octave-send-line)
	      )
  :init
  (setq octave-send-line-auto-forward t)
  )

(use-package tuareg
  :ensure t
  :mode (("\\.ml\\'" . tuareg-mode)
	 ("\\.mli\\'" . tuareg-mode)
	 ("\\.mlp\\'" . tuareg-mode))
  :init
  (use-package utop
    :ensure t
    :preface
    (setq utop-command "opam config exec -- utop -emacs")
    :init
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    )
  )

(use-package merlin
  :ensure t
  :hook
  ;; Start merlin on ml files
  ((reason-mode tuareg-mode caml-mode) . merlin-mode)
  :preface
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam)))
  )

;;;Config

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(set-keyboard-coding-system nil)

; Parenthnesses
(setq show-paren-delay 0)
(add-hook 'prog-mode-hook 'show-paren-mode)

; Line numbers
(global-display-line-numbers-mode 1)

; Remove bars
(menu-bar-mode -1)
(toggle-scroll-bar  -1)
(tool-bar-mode -1)

;Spellcheck
(setq ispell-program-name "aspell")
(defun toggle-spell-en ()
  (interactive)
  (setq ispell-local-dictionary "en_US")
;  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
;  (flyspell-buffer)
  )
(defun toggle-spell-pl ()
  (interactive)
  (setq ispell-local-dictionary "pl")
;  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=pl_PL"))
;  (flyspell-buffer)
  )
(global-set-key (kbd "<f6>") 'toggle-spell-en)
(global-set-key (kbd "<f7>") 'toggle-spell-pl)

; Verilog 
(setq verilog-indent-level 4)
(setq verilog-indent-level-module 4)
(setq verilog-indent-level-declaration 4)
(setq verilog-indent-level-directive 4)
(setq verilog-indent-level-behavioral 4)
(setq verilog-auto-newline nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#000000" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(beacon-color "#d54e53")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(enable-recursive-minibuffers t)
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
   (quote
    (utop merlin tuareg poly-R poly-markdown ess markdown-mode company-math diminish magit elpy ein counsel flycheck use-package-hydra company-mode pdf-tools auctex-latexmk auctex color-theme-sanityinc-tomorrow ivy)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
