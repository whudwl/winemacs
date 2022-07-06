;; git needs to be available in PATH
;; altering exec-path is not enough, as when bootstraping straight.el will start a new emacs instance
(unless (executable-find "git") 
  (error "git is not found"))

;; bootstrap straight.el 
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap `use-package'
(setq-default use-package-always-defer t ; Always defer load package to speed up startup time
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; Integration straight with use-package
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save
                                         find-when-checking))

(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package emacs
  :bind (("C-2" . set-mark-command)
	 ("M-t" . mode-line-other-buffer)))
