; (setq c-default-style "linux")

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" .  "http://elpa.gnu.org/packages/")))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize) ;; You might already have this line

(defconst demo-packages
  '(company-irony
    glsl-mode
    editorconfig
    pdf-tools
    company-irony-c-headers
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

(setq bell-volume 0)

(global-linum-mode 1)
;(require 'git)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
  (eval-after-load 'company
    '(add-to-list
       'company-backends '(company-irony-c-headers company-irony)))

; Get rid of the startup message
(setq inhibit-startup-message t)
; Show file full path in title bar
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory)))))))
; Shows parenthesis
(show-paren-mode 1)
; Shows column number
(column-number-mode 1)
; Change default colors
(set-background-color "grey14")
(set-foreground-color "white")
(set-cursor-color "white")
; No toolbar
(progn
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;  (menu-bar-mode -1)
  (scroll-bar-mode -1)
)

(require 'xcscope)
 (add-hook 'c-mode-common-hook' (lambda () (require 'xcscope)))
(cscope-setup)

(add-to-list 'load-path "~/.emacs/emacs.d")    ; This may not be appeared if you have already added.
;(require 'auto-complete-config)
;(ac-config-default)

(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))

(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
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
(defun my-next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun my-previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )
(global-set-key (kbd "C-n") 'my-next-error)
(global-set-key (kbd "C-p") 'my-previous-error)

(global-set-key [f5] 'my-recompile)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("/home/siglesias/ownCloudPersonal/tasks.org")))
 '(package-selected-packages
   (quote
    (company-c-headers auto-complete glsl-mode pdf-tools editorconfig company-irony irony)))
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

;; ido-mode
(setq ido-use-filename-at-point t)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)
(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " " 'ido-exit-minibuffer))

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
