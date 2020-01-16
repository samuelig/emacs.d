;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file setups the configuration and automatically install packages.

;;; Code:

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize) ;; You might already have this line

(defconst demo-packages
  '(company-irony
    glsl-mode
    editorconfig
    pdf-tools
    company-irony-c-headers
    magit-gh-pulls
    magit-todos
    multiple-cursors
    use-package
    speedbar
    markdown-mode
    xcscope))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)

  (global-company-mode t)
  )

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  ;; Use compilation database first, clang_complete as fallback.
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                  irony-cdb-clang-complete))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
  )


(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-modei-hook #'irony-eldoc))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  )


(defun my/python-mode-hook ()
  "Add company-jedi as company backend."
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))


(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; Global configuration

; Show line numbers
(global-linum-mode 1)
; Get rid of the startup message
(setq inhibit-startup-message t)
; Show file full path in title bar
(setq-default frame-title-format
	      (list '((buffer-file-name " %f"
					(dired-directory
					 dired-directory
					 (revert-buffer-function " %b"
								 ("%b - Dir:  " default-directory)))))))
;; Shows parenthesis
(show-paren-mode 1)
;; Shows column number
(column-number-mode 1)
; No toolbar
(progn
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
					;  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  )


;; Setup a theme

(use-package doom-themes
  :ensure t)
(use-package doom-modeline
  :ensure t)
(require 'doom-modeline)
; Initialize doom-modeline.
(require 'doom-modeline)
(doom-modeline-mode 1)
(load-theme 'doom-one t)


;; Functions

(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary."
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compile window exists
 	(progn
 	  (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
 	  (kill-buffer "*compilation*") ; and kill the buffers
 	  )
      )
    (call-interactively 'compile)
    (enlarge-window 20)
    )
  )

(global-set-key [f5] 'my-recompile)


;; Config packages

(require 'xcscope)
(add-hook 'c-mode-common-hook' (lambda () (require 'xcscope)))
(cscope-setup)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Nextcloud/tasks_2019.org")))
 '(package-selected-packages
   (quote
    (magit-todos multiple-cursors eww-lnum company-c-headers magit-gh-pulls glsl-mode pdf-tools editorconfig company-irony irony)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors
	   (require
	    (quote whitespace))
	   (whitespace-mode 1))
     (whitespace-line-column . 79)
     (whitespace-style face indentation)
     (eval progn
	   (c-set-offset
	    (quote case-label)
	    (quote 0))
	   (c-set-offset
	    (quote innamespace)
	    (quote 0))
	   (c-set-offset
	    (quote inline-open)
	    (quote 0)))
     (eval progn
	   (c-set-offset
	    (quote innamespace)
	    (quote 0))
	   (c-set-offset
	    (quote inline-open)
	    (quote 0)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(global-linum-mode 0)
(pdf-tools-install)

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(require 'irony)
(add-to-list 'irony-supported-major-modes 'glsl-mode)

(editorconfig-mode 1)

(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(setq magit-gh-pulls-pull-detail-limit 1000)

(require 'multiple-cursors)
(global-set-key (kbd "C-c M-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "<f8>") 'speedbar)

(require 'magit-todos)
(require 'hl-todo)

;; Miscelanea config

;; Fixes issues with magit-gh-pulls on Emacs 25
(setq gnutls-log-level 2)

;; Insert <tab> whenever I press the key
(defun my-self-insert-command ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "<backtab>") 'my-self-insert-command);

; Org-mode: switch entry to DONE when all subentries are done.

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

; Set compile directory with M-X
(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given
directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))

(global-set-key (kbd "M-X") 'in-directory)

; Encryption support
(require 'epa-file)
(epa-file-enable)
;;; init.el ends here
