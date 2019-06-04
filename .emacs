(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)


;;   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;  (when (< emacs-major-version 24)
  ;;  ;; For important compatibility libraries like cl-lib
    ;;(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (purple-haze)))
 '(custom-safe-themes
   (quote
    ("7023f8768081cd1275f7fd1cd567277e44402c65adfe4dc10a3a908055ed634d" default)))
 '(package-selected-packages
   (quote
    (go-mode purple-haze-theme paredit racket-mode haskell-mode proof-general)))
 '(safe-local-variable-values
   (quote
    ((TeX-command-default . "latexmk")
     (TeX-master . t)))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Racket-Mode

;; Allow unicode symbols to be input
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)


;; Make "C-c r" run the program
(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;; Make "TAB" indent
(setq tab-always-indent 'complete)

;; note: this changes the behavior of Emacs' stnadard `indent-for-tab-command`
;; to which TAB is bound by default in the racket-mode edit and REPL modes

;; smartparens
(require 'smartparens-config)

;; start with line numbers enabled in mode: relative
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; enabling AC globally, except for minibuffer
(global-auto-complete-mode t)


;-------------AUTO-COMPLETE------------;
(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))


;-------------LATEX PREVIEW PANE------------;
(latex-preview-pane-enable)
