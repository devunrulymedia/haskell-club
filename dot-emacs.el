;;
;; some configuration for nice(ish) Emacs+Haskell setup
;;
;; to enable the config, please symlink/move it to ~/.emacs.el
;; first run requires internet connectivity
;;
;; tested with Emacs24 - http://emacsformacosx.com/
;;
;; make sure you have Haskell + ghc-mod configured as well
;;
;; haskell platform: http://www.haskell.org/platform/mac.html
;; cabal update
;; cabal install ghc-mod
;;
;; (after installing it, confirm that path matches the one later in config)

(set-language-environment "UTF-8")

;; helpers for lazy people
(defun load-if-exists (t-file-name)
  (when (file-exists-p t-file-name)
    (load t-file-name)))

(defun url-ensure-file (url file-name)
  (unless (file-exists-p file-name)
    (url-copy-file url file-name)))

;; package.el - one of package mgmt things for emacs
(when (eq emacs-major-version 23)
  (url-ensure-file "http://bit.ly/pkg-el23" (expand-file-name "~/.emacs.d/package.el")))
(when (>= emacs-major-version 24)
  (url-ensure-file "http://bit.ly/pkg-el" (expand-file-name "~/.emacs.d/package.el")))
(load-if-exists (expand-file-name "~/.emacs.d/package.el"))

;; update path below if your versions differ
(add-to-list 'load-path (expand-file-name "~/Library/Haskell/ghc-7.4.2/lib/ghc-mod-1.11.3/share"))
(add-to-list 'exec-path (expand-file-name "~/Library/Haskell/bin"))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p '(haskell-mode ghci-completion auto-complete flymake flymake-cursor))
  (when (not (package-installed-p p))
    (package-install p)))

(require 'ghci-completion)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'flymake-mode)
(add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(require 'flymake-cursor)
(eval-after-load 'flymake '(require 'flymake-cursor))

(require 'auto-complete-config)
(ac-config-default)
(add-hook 'auto-complete-mode-hook
          (lambda ()
            (setq completion-at-point-functions '(auto-complete))))

;; usefull/needed on MACs with UK keyboard
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; some opinionated tweaks
(iswitchb-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(file-name-shadow-mode 1)
(size-indication-mode)
(savehist-mode 1)
(custom-set-variables
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(next-line-add-newlines nil)
 '(tool-bar-mode nil)
 '(toolbar-visible-p nil))

;; keymaps
(define-key global-map (kbd "M-SPC") 'dabbrev-expand)
(define-key global-map (kbd "C-<tab>") '(lambda () (interactive) (select-window (next-window))))
(define-key global-map (kbd "C-S-<iso-lefttab>") '(lambda () (interactive) (select-window (previous-window))))
(define-key global-map (kbd "ESC C-<tab>") '(lambda () (interactive) (select-frame-set-input-focus (next-frame))))
(define-key global-map (kbd "M-g") 'goto-line)
