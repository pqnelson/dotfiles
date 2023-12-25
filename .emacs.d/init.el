(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ))
(package-initialize)
;; package-archives
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package slime
             :defer t
             :ensure t)

(use-package polymode
             :defer t
             :ensure t)

(use-package yasnippet
             :ensure t
             :init (yas-global-mode 1))

;; (require 'poly-R)
;;(require 'poly-markdown)
(use-package poly-markdown
             :defer t
             :ensure t
             :config
             (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
             (add-hook 'poly-markdown+r-mode-hook 'turn-on-auto-fill))
;;;; random config preferences
(setq-default fill-column (if (null window-system) 67 72))
(turn-on-auto-fill)
(setq-default auto-fill-function 'do-auto-fill)

(use-package paren
             :defer t)
;; (require 'paren)
(global-font-lock-mode 1)
(show-paren-mode 1) ;; highlight matching parentheses

(setq-default indent-tabs-mode nil) ;; soft tabs
(setq-default tab-width 4)
(setq-default default-tab-width 4)
;; data-directory
;; show line/column numbers, time in minor mode
(display-time-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;;; browse the intertubes!
;; (require 'w3m-load)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a www browser to show a URL." t)
;; (global-set-key "\C-xm" 'browse-url-at-point)

;; (defun search-wikipedia (phrase)
;;   (interactive "sTerm:")
;;   (let ((url (concat
;;               "https://en.m.wikipedia.org/w/index.php?search=" phrase)))
;;     (w3m-goto-url url)))

;; decent color scheme
(when window-system
  (set-face-attribute 'default (selected-frame) :height 100)
  (load-theme 'manoj-dark)
  ;; (load-theme 'deeper-blue)
  )

;; disable scroll bar
(defun disable-scroll-bar ()
  (unless (null window-system)
    (toggle-scroll-bar -1)))

(use-package markdown-mode
             :defer t
             :ensure t
             :config
             (add-hook 'gfm-mode-hook 'turn-on-auto-fill)
             (autoload 'gfm-mode "markdown-mode"
                       "Major mode for editing GitHub flavored markdown files." t)
             (add-to-list 'auto-mode-alist '("README\\.md" . gfm-mode))
             (add-to-list 'auto-mode-alist '("\\.md" . gfm-mode)))

;;;; erc
(use-package erc
             :defer t
             :config
             (setq erc-server "irc.libera.chat" ;; "irc.freenode.net"
	               erc-port 6667
	               erc-nick "thmprover")
             
             ;; (erc-autojoin-mode 1)
             (setq erc-autojoin-channels-alist '(("libera.chat"
                                                  "#commonlisp" "#coq")
                                                 ("freenode.net"
				                                  "#proglangdesign" "#lisp")))
             (global-set-key "\C-cef" (lambda ()
			                            (interactive)
			                            (erc :server "irc.libera.chat" ;; "irc.freenode.net"
				                             :port "6667"
				                             :nick "thmprover")))
             ;; (erc-timestamp-mode 1)
             (setq erc-kill-buffer-on-part t)
             (require 'erc-match)
	         (setq erc-keywords '("thmprover"))
	         (erc-match-mode 1))


(defun random-choice (list)
  (nth (random (length list)) list))

(defun erc-cmd-LATER (&rest ignore)
  (interactive)
  (erc-cmd-QUIT
   (random-choice
    '("...and miles to go before I sleep."
      "Another long day's journey into night."
      "Up, up, and away"
      "Goodnight, ladies, good night, sweet ladies, good night, good night" ;; Ophelia, in Hamlet
      "This parting was well made" ;; Shakespeare, Julius Caesar
      "[Exit, pursued by bear]"
      "For He Was Great of Heart" ;; Othello
      "For Here, I Hope, Begins Our Lasting Joy" ;; Shakespeare, Henry VI (Pt III)
      "And Let's Away, to Part the Glories of This Happy Day" ;; Shakespeare, Julius Caesar
      ))))

;;;; openers
(defun open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun recompile-init ()
  (interactive)
  (byte-recompile-file "~/.emacs.d/init.el" t))

(defun open-journal ()
  (interactive)
  (find-file "~/.emacs.d/journal.tex"))

(defun open-org-notes ()
  (interactive)
  (find-file "~/org/notes.org"))

(defun open-brainiac-src ()
  (interactive)
  (find-file "~/lisp/auto-pse/src/main.lisp"))

;; (use-package clojure-mode)

;;;; org-mode style
(use-package org
             :ensure t
             :defer t
             :config
             (require 'org-tempo)
             (add-to-list 'org-structure-template-alist '("xca" . "exercise"))
             (add-to-list 'org-structure-template-alist '("ex" . "example"))
             (add-to-list 'org-structure-template-alist '("pz" . "puzzle"))
             (add-to-list 'org-structure-template-alist '("lem" . "lemma"))
             (add-to-list 'org-structure-template-alist '("prop" . "proposition"))
             (add-to-list 'org-structure-template-alist '("pf" . "proof"))
             (add-to-list 'org-structure-template-alist '("thm" . "theorem"))
             (add-to-list 'org-structure-template-alist '("d" . "definition"))
             (org-babel-do-load-languages
              'org-babel-load-languages
              '((lisp . t)
                (scheme . t)
                (emacs-lisp . t)))
             (require 'ob-scheme)
             ;; HACK: Do NOT treat "<" or ">" as delimiters in
             ;; paren-mode, for lisp or org-mode. Well, in Lisp, I
             ;; do use "foo->bar" and sometimes "<class-name>", so
             ;; treat them as word constituents (so "foo->bar" is
             ;; highlighted, as opposed to "foo->" without
             ;; highlighting "bar").
             (modify-syntax-entry ?< "w" lisp-mode-syntax-table)
             (modify-syntax-entry ?> "w" lisp-mode-syntax-table)
             (modify-syntax-entry ?< "." org-mode-syntax-table)
             (modify-syntax-entry ?> "." org-mode-syntax-table)
)


;; (require 'htmlize)
(use-package htmlize
             :defer t
             :ensure t
             :config
             (setq org-html-htmlize-output-type 'css)
             (setq org-html-htmlize-font-prefix "org-")
             (setq org-src-preserve-indentation t)
             (setq org-src-fontify-natively t)
             )
;; (setq org-html-htmlize-output-type 'css)
;; (setq org-html-htmlize-font-prefix "org-")
;; (setq org-src-preserve-indentation t)
;; (setq org-src-fontify-natively t)

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-adapt-indentation nil)

(defun alex.org/headers ()
  (interactive)
  (goto-char 0)
  (insert "#+TITLE: \n")
  (insert "#+AUTHOR: Alex Nelson\n")
  (insert "#+EMAIL: pqnelson@gmail.com\n")
;;  (insert "#+DATE: " (format-time-string "%B %e, %Y") "\n")
  (insert "#+LANGUAGE: en\n")
  (insert "#+OPTIONS: H:5\n")
  (insert "#+HTML_DOCTYPE: html5\n")
  (insert "# Created " (format-time-string "%A %B %e, %Y at %l:%M%p") "\n\n"))

;;;; MMIX
(defun load-if-exists (file)
  (when (file-exists-p file)
    (load file)))

(when (file-exists-p "~/.emacs.d/mmix-mode.el")
  (load-if-exists (expand-file-name "~/.emacs.d/mmix-mode.el"))
  (autoload 'mmix-mode "mmix-mode" "Major mode for editing MMIX files" t)
  (setq auto-mode-alist (cons '("\\.mms" . mmix-mode)
                              auto-mode-alist)))

;;;; common lisp helpers
;(setq inferior-lisp-program "/home/alex/src/ccl/armcl")
(setq inferior-lisp-program "/usr/bin/sbcl")

(use-package paredit
             :defer t
             :ensure t
             :config
             (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of lisp Code." t)
             (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
             (add-hook 'lisp-mode-hook #'enable-paredit-mode)
             (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
             (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package eldoc
             :defer t
             :ensure t
             :config
             (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
             (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1))))

(use-package slime
             :ensure t
             :defer t
             :config
             ;; (slime-setup '(slime-fancy slime-banner slime-repl-ansi-color))
             (setq slime-net-coding-system 'utf-8-unix)
             (require 'slime-autoloads)
             (when (file-exists-p "~/quicklisp/slime-helper.elc")
               (load (expand-file-name "~/quicklisp/slime-helper.elc")))
             (load-if-exists (expand-file-name "~/.emacs.d/slime-repl-ansi-color/slime-repl-ansi-color.el"))
             ;; In slime, run
             ;; > (ql:quickload :clhs)
             ;; and then
             ;; > (clhs:print-emacs-setup-form)
             ;; Which should tell you to run
             ;; > (clhs:install-clhs-use-local)
             ;; and then adding the next line should make everything work fine
             (load-if-exists "/home/alex/quicklisp/clhs-use-local.elc")
             )
(set-language-environment "UTF-8")
(setq default-enable-multibyte-characters t)

(defun alex/slime-eval ()
  (interactive)
  (if (slime-connected-p)
      (slime-eval-buffer)
      (slime)))
;; (global-set-key [f12] 'alex/slime-eval)

(defun parent-dir (directory)
  (file-name-directory
   (directory-file-name directory)))

(defun alex.lisp/find-asdf-dir-iter (directory depth)
  (when (> depth 0)
    (unless (string-equal "/" directory)
      (if (directory-files directory nil "asd$")
          directory
        (alex.lisp/find-asdf-dir-iter (parent-dir directory)
                                      (1- depth))))))

(defun alex.lisp/system-name (asdf-dir)
  (file-name-base
   (directory-file-name asdf-dir)))

(defun alex.lisp/package-trim-container (relative-path-dir)
  (mapconcat 'identity (rest (split-string relative-path-dir "/")) "/"))

(defun alex.lisp/package-prefix (path)
  (let ((asdf-dir (alex.lisp/find-asdf-dir-iter (file-name-directory path) 10)))
    (concat (alex.lisp/system-name asdf-dir)
            "/"
            (file-name-directory
             (alex.lisp/package-trim-container
              (file-relative-name
               path
               (file-name-as-directory asdf-dir)))))))

;; (alex.lisp/package-name "/home/alex/src/brainiac/src/math/bch-tests.lisp")
;; => "brainiac.math.bch-tests"
(defun alex.lisp/package-name (path)
  (let ((prefix (alex.lisp/package-prefix path))
        (package (file-name-base path)))
    (if prefix
        (replace-regexp-in-string "/" "."
                                  (concat prefix package))
      package)))

(defun alex.lisp/header ()
  (interactive)
  (goto-char 0)
  (let* ((package (alex.lisp/package-name (buffer-file-name))))
    (insert "(defpackage #:" package "\n  (:use #:cl))\n")
    (insert "(in-package #:" package ")\n")))

(add-to-list 'slime-contribs 'slime-cl-indent)
(setq lisp-indent-function 'common-lisp-indent-function)
;; (setq common-lisp-style-default "sbcl")

(use-package autoinsert
             :ensure t
             :defer t
             :config
             (push '(lisp-mode . alex.lisp/header) auto-insert-alist)
             (push '(org-mode . alex.org/headers) auto-insert-alist)
             (auto-insert-mode +1))

;;;; ESS configuration
(use-package ess
             :defer t
             :ensure t
             :config
             (setq ess-language "R")
             (setq ess-default-style 'C++)
             (setq ess-indent-offset 4)
             )

(defun alex/ess-eval ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))

(defun alex/ess-insert-code-fence ()
  (interactive)
  (insert "```{r}\n")
  (insert "\n")
  (insert "```")
  (forward-line -1))


(use-package poly-R
             :defer t
             :ensure t
             :config
             (add-hook 'poly-markdown+r-mode-hook
                       '(lambda ()
                         (local-set-key (kbd "C-c C-e") 'polymode-eval-region-or-chunk)
                         (local-set-key (kbd "C-c t") 'alex/ess-insert-code-fence))))



(defun new-pa-page (filename)
  (interactive "sFilename:")
  (let ((path (concat
               "~/src/political-arithmetic/_posts/"
               (format-time-string "%Y-%m-%e-")
               filename ".Rmd")))
    (find-file path)
    (insert "---\n")
    (insert "title: \"\"\n")
    (insert "author: \"Alex Nelson\"\n")
    (insert (format-time-string "date: \"%m/%e/%Y\"\n"))
    (insert "output:\n")
    (insert "  md_document:\n")
    (insert "    variant: gfm\n")
    (insert "    toc: true\n")
    (insert "    toc_depth: 4\n")
    (insert "---\n\n```{r setup, include=FALSE}\n")
    (insert "library(tidyverse)\n")
    (insert "library(rmarkdown)\n")
    (insert "```\n\n# Introduction\n\n")))

(global-set-key [f10] 'new-pa-page)

;; temporary bugfix until Emacs 26.3 or greater released
;; (setq gnutls-log-level 1)
;; (advice-add 'gnutls-available-p :override #'ignore)

;; once a message is sent, don't keep it around
(setq message-kill-buffer-on-exit t)

;; (setq gnutls-algorithm-priority "NORMAL:%COMPAT")
;; (setq smtp-debug-info t
;;       smtp-debug-verb t)


;;;; c-mode
(setq-default c-basic-offset 4)
(setq-default c-default-style "java")

(when (file-exists-p "/usr/local/share/emacs/site-lisp/acsl.el")
  (autoload 'acsl-mode "acsl" "Major mode for editing ACSL code" t)
  ;; (load-library "frama-c-recommended")
  (load-library "frama-c-init")
  (load-library "frama-c-dev")
  ;; ACSL requires working out a bit of the logic in header files, but to
  ;; separate out the logic from the code, we conventionally store this in
  ;; ".acsl" files. They should have C header mode.
  (add-to-list 'auto-mode-alist '("\\.acsl\\'" . acsl-mode)))

;; add abbrev-mode
;; then on "()" typing ";ife" would insert the skeleton?
;; (define-abbrev c-mode-abbrev-table ";ife"
;;   "" 'c-skeleton-if-else-endif)

(define-skeleton c-skeleton-if-else-endif
  "Insert an if-else region" nil
  > "if (" _ ") {" \n
  "} else {" \n
  "}")

;;;; Standard ML
(use-package sml-mode
    :ensure t
    :defer t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\|fun\\)\\'" . sml-mode))

;;;; twelf
(setq twelf-root "/home/alex/src/twelf/")
(load-if-exists (concat twelf-root "emacs/twelf-init.el"))

;;;; OCaml
(use-package tuareg
             :defer t
             :ensure t)

(use-package merlin
             :defer t
             :ensure t)

;;;; Coq related
(defun git-exists? ()
  (zerop (shell-command "git --version")))

(load-library "cl-extra")
(unless (file-exists-p "~/.emacs.d/PG/")
  (when (git-exists?)
    (shell-command "git clone https://github.com/ProofGeneral/PG ~/.emacs.d/PG/")
    (shell-command "make -C ~/.emacs.d/lisp/PG")))

(load-if-exists "~/.emacs.d/PG/generic/proof-site")
;; when coq is installed
(when (file-exists-p "/usr/bin/coqtop")
  (setq-default coq-prog-name "/usr/bin/coqtop -emacs"))

(load-file "~/.emacs.d/mizar.el")
(setq load-path (cons (substitute-in-file-name "$MIZFILES") load-path))
(autoload 'mizar-mode "mizar" "Major mode for editing Mizar articles." t)
;(autoload 'mmlquery-decode "mizar")
;(autoload 'mmlquery-mode "mizar")
(setq auto-mode-alist (append '(("\\.miz" . mizar-mode)
                                ("\\.abs" . mizar-mode))
                              auto-mode-alist))
;(setq format-alist
;      (append  '((text/mmlquery "Extended MIME text/mmlquery format."
;                  "::[ \t]*Content-[Tt]ype:[]*text/mmlquery"
;                  mmlquery-decode nil nil mmlquery-mode))
;               format-alist))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(merlin tuareg sml-mode poly-R ess paredit htmlize poly-markdown yasnippet polymode slime use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar blath-dir "~/src/blath/")

(defun count-blath-files ()
  (length (directory-files (concat blath-dir "tex/") nil "\\.tex\\'")))

(defun blath ()
  (interactive)
  (let ((string (read-string "New file name: " nil)))
    (find-file (concat blath-dir "tex/"
                       (format "%03d" (1+ (count-blath-files)))
                       "-"
                       string
                       ".tex"))))

(load-file "~/.emacs.d/forest-mode.el")
(require 'forest-mode)

(global-set-key [f1] 'open-brainiac-src)
(global-set-key [f2] 'open-org-notes)
(global-set-key [f4] #'(lambda ()
                         (interactive)
                         (find-file (format "%s/%s-0001.tree"
                                            forest-tree-dir
                                            forest-mode-namespace-default))))
(global-set-key [f6] 'open-init)
(global-set-key [f7] 'recompile-init)
(global-set-key [f12] 'slime)

;; Since C-xo is "other-window +1", Cx-p moves back to previous window
(global-set-key "\C-xp" (lambda ()
                          (interactive)
                          (other-window -1)))
