
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(eval-when-compile (require 'use-package))

(use-package ivy
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(use-package company
  :ensure t
  :init
  (;add-hook 'after-init-hook
   global-company-mode)
  )

(use-package company-math)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(use-package tex                   ; auctex
;  :load-path "site-lisp/auctex/"
;  :defines (latex-help-cmd-alist latex-help-file)
  :ensure company
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq TeX-view-program-selection
	'(
	  ((output-dvi has-no-display-manager) "dvi2tty")
	  ((output-dvi style-pstricks) "dvips and gv")
	  (output-dvi "xdvi")
	  (output-pdf "PDF Tools")
	  (output-html "xdg-open")
	  )
	)

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
  (add-hook 'LaTeX-mode-hook
	    '(lambda ()
	       (local-set-key "'" 'TeX-insert-single-quote)
	       ))
  (defun my-latex-mode-setup ()
    (setq-local company-backends
		(append '((company-math-symbols-latex company-latex-commands)) company-backends))
    )
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

;(use-package auctex-latexmk
;  :ensure auctex
;  :mode ("\\.tex\\'" . TeX-latex-mode)
;  :init
;  (auctex-latexmk-setup)
;  (add-hook 'auto-save-hook
;	    (lambda ()
;	      (when (string= major-mode 'latex-mode)
;		(TeX-run-latexmk "LaTeX"
;				 (format "latexmk %s" (buffer-file-name))
;				 (file-name-base (buffer-file-name))))))
;  )

(use-package pdf-tools)


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(set-keyboard-coding-system nil)

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
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(beacon-color "#cc6666")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (flycheck use-package-hydra company-mode pdf-tools auctex-latexmk auctex color-theme-sanityinc-tomorrow ivy)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
