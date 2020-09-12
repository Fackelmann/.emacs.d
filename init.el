(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq inhibit-startup-screen t)
(setq personal-host "Daniels-MacBook-Pro.local")
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Install missing packages.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;Creates new shell
(fset 'nshell
      "\C-u\370shell")

;;(setq make-backup-files nil) ; stop creating ~ files
;;Backup all files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;Remove toolbar
(tool-bar-mode -1)

;;Display clock
(display-time-mode 1)

;; To resync dirs when autocompletion doesn't work
(global-set-key (kbd "M-<up>") 'shell-resync-dirs)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Remove images from eww 02/22/18
;; (setq shr-max-image-proportion nil)
;;(setq shr-inhibit-images t)
;;(setq shr-width 78)



;; Enable company mode for all buffers 03/27/19
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)


;; Ace window. I love it! moves to other window by typying character window 03/27/19
;;(global-set-key (kbd "\C-xo") 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Kill non matching buffers to clean up 03/28/19
(defun kill-non-matching-buffers ()
  "Kill buffers that don't match \"shell\" or \"scratch\". Or any internal buffers for that matter."
  (interactive)
  ;; dont set a global variable
  ;; tempoary bind it with let
  (let ((list (buffer-list)))
    (while list
      ;; again bind buff locally
      (let ((buff (buffer-name (car list))))
        (setq list (cdr list))
        (when (and (not (string-match "shell" buff))
                   (not (string-match "scratch" buff))
                   ;; you probably don't want kill internal buffers
                   ;; which start with space or asterisk
                   (not (string-match "\\` " buff))
                   (not (string-match "\\`\\*" buff)))
          (kill-buffer buff))))))


(global-set-key [f5] 'kill-non-matching-buffers)


;; Display line numbers
(global-display-line-numbers-mode)
;; For older versions of emacs
;;(linum-mode)

;; Enable Elpy 06/01/2019
;;(elpy-enable)

;; set python shell interpreter version for elpy 06/01/2019
(setq python-shell-interpreter "/usr/local/bin/python3")



;; Add fill column indicator mode to python mode
(setq-default fill-column 80)
;;(add-hook 'python-mode-hook 'fci-mode)
;;(add-hook 'python-mode-hook 'flycheck-mode)
;;Show whitespaces in python 12/10/19
;;(add-hook 'python-mode-hook 'whitespace-mode)
;;Dunno 12/10/19
(put 'scroll-left 'disabled nil)

;; Org settings
(use-package org)
(use-package org-journal)
(setq org-default-notes-file (concat org-directory "/inbox.org"))


(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
(setq org-agenda-files (list "~/org"
			     "~/org/work"))
(add-to-list 'org-agenda-files org-journal-dir)

(setq org-log-done t)

;; Adding some new templates 05/21/20
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline org-default-notes-file  "Tasks")
;; 	 "* TODO %?\n  %i\n ")
;; 	("n" "Notes" entry (file+headline org-default-notes-file "Notes")
;; 	 "* %?\n %i\n")))

;; TODO keywords 08/06/20
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "white" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; TODO tags 08/06/20
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/inbox.org")
               "* TODO %?\n%U\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/org/inbox.org")
               "* %? :NOTE:\n%U\n\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/org/inbox.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/org/inbox.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	      ("s" "Slack" entry (file "~/org/inbox.org")
               "* SLACK %? :SLACK:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/inbox.org")
               "* NEXT %?\n%U\n\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; set up refile targets
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Allow refile to create parent tasks with confirmation
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Set up org habits 09/11/20
(add-to-list 'org-modules 'org-habit t)
(setq org-habit-show-habits-only-for-today nil)
(setq org-agenda-repeating-timestamp-show-all nil)

(setq org-startup-indented t)
(setq org-indent-indentation-level 1)
(setq org-adapt-indentation nil)
;; --- Magit
;; Bind magit C-x g
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
;; 12/13/19 Stop magit from asking to save modified files every damn time I try to do somethin
(setq magit-save-repository-buffers nil)

;; Enabling undo tree mode 01/02/20
(use-package undo-tree)
(global-undo-tree-mode)


;; Binding org journal scheduled entry 02/11/20
(global-set-key (kbd "C-c C-k") 'org-journal-new-scheduled-entry)

;; Closing org journal file after saving it 02/11/20
(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))
(define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)
;; Add clocktable at the beginning of header file 07/08/20
(setq org-journal-file-header "#+BEGIN: clocktable :scope file :maxlevel 9 :block today :scope agenda :fileskip0 t
#+END")


(global-set-key [f8] 'rename-buffer)


(when (string= (system-name) personal-host)
  ;; Helm bibtex 04/01
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  (setq bibtex-completion-bibliography
	'("/Users/dgonzalez/org-roam/zotero-library.bib"))
  (setq bibtex-completion-pdf-field "file")

  (setq bibtex-completion-format-citation-functions
	'((org-mode      . bibtex-completion-format-citation-default)
	  (latex-mode    . bibtex-completion-format-citation-cite)
	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	  (default       . bibtex-completion-format-citation-default)))

  ;;Org-roam 04/12/20
  (use-package org-roam
    :ensure t
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/org-roam")
    :bind (:map org-roam-mode-map
		(("C-c n l" . org-roam)
		 ("C-c n f" . org-roam-find-file)
		 ("C-c n g" . org-roam-graph-show))
		:map org-mode-map
		(("C-c n i" . org-roam-insert))
		(("C-c n I" . org-roam-insert-immediate))))

  (require 'org-roam-protocol)
  (setq org-roam-link-title-format "R:%s")

  ;; I like my filenames to be only given by timestamp, so removing title (which can change.
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam-capture--get-point)
	   "%?"
	   :file-name "%<%Y%m%d%H%M%S>"
	   :head "#+TITLE: ${title}\n"
	   :unnarrowed t)))
  (setq org-roam-graph-executable "/usr/local/bin/dot")
  (setq org-roam-graph-viewer "/Applications/Firefox Nightly.app/Contents/MacOS/firefox")

  (use-package deft
    :after org
    :bind
    ("C-c n d" . deft)
    :custom
    ;; We don't want recursion. The reason is that we have the /ref subdirectory for literature notes, and I don't want to clutter my deft results
    (deft-recursive nil)
    (deft-use-filter-string-for-filename t)
    (deft-default-extension "org")
    (deft-directory "~/org-roam"))




  (use-package org-ref)
  ;; see org-ref for use of these variables
  (setq reftex-default-bibliography '("/Users/dgonzalez/org-roam/zotero-library.bib"))
  (setq org-ref-default-bibliography '("/Users/dgonzalez/org-roam/zotero-library.bib"))
  (setq org-ref-bibliography-notes '("/Users/dgonzalez/org-roam/bibnotes.org"))

  ;; Literature notes in org-roam 08/22/20
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map
		(("C-c n a" . orb-note-actions))))

  (setq orb-templates
	'(("r" "ref" plain (function org-roam-capture--get-point) ""
           :file-name "refs/${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
           :unnarrowed t)))

  ;; Add mu4e mail client
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  (require 'mu4e)

  (setq
   mue4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"
   user-mail-address "dan@danielgplaza.com"

   mu4e-maildir       "~/Maildir/fastmail"   ;; top-level Maildir
   ;; note that these folders below must start with /
   ;; the paths are relative to maildir root
   mu4e-refile-folder "/Archive"
   mu4e-sent-folder   "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Trash")

  ;; this setting allows to re-sync and re-index mail
  ;; by pressing U
  (setq mu4e-get-mail-command  "mbsync -a")


  (setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server         "smtp.fastmail.com"
   smtpmail-stream-type 'starttls
   smtpmail-smtp-service 587
   smtp-local-domain "fastmail.com")

  (global-set-key (kbd "C-x m") 'mu4e)

  (fset 'my-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

  (setenv "PKG_CONFIG_PATH"
          (f-join
           (file-name-as-directory
            (nth 0
		 (split-string
                  (shell-command-to-string "brew --prefix"))))
           "Cellar" "libffi" "3.2.1" "lib" "pkgconfig"))
  (use-package pdf-tools
    :ensure t
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (setq pdf-annot-activate-created-annotations t)))



;; Enable ligature for FiraCode
;; https://github.com/tonsky/FiraCode
(if (fboundp 'mac-auto-operator-composition-mode) (mac-auto-operator-composition-mode))


;; Enable helm mode 04/19/20
;; Ido mode didn't allow me to add a space to a filename in org roam
;; In any case helm seems more popular nowadays

(use-package helm)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "/tmp/helm-cfg.el") (delete-file "/tmp/helm-cfg.el"))))
(global-set-key (kbd "C-c b") 'helm-bibtex)
;;
;; Some original Emacs commands are replaced by their ‘helm’ counterparts:


;; - ‘find-file’(C-x C-f)            =>‘helm-find-files’
;; - ‘occur’(M-s o)                  =>‘helm-occur’
;; - ‘list-buffers’(C-x C-b)         =>‘helm-buffers-list’
;; - ‘completion-at-point’(M-tab)    =>‘helm-lisp-completion-at-point’[1]
;; - ‘apropos-command’(C-h a)        =>‘helm-apropos’
;; - ‘dabbrev-expand’(M-/)           =>‘helm-dabbrev’
;; - ‘execute-extended-command’(M-x) =>‘helm-M-x’

;; Toggle truncate lines in all buffers 04/25/20
;;(setq-default truncate-lines t)
;;(setq truncate-partial-width-windows nil) ;; for vertically-split windows
;;(setq truncate-partial-width-windows nil)
(global-visual-line-mode t)

;; Toggle show paren mode. I don't know how I've lived without this all these years 05/03/20
(show-paren-mode 1)

;; Add hook to Pomidor to record completed pomodoros in org journal 05/07/20
(use-package pomidor)
(defun pomidor-insert-org-journal ()
  "Prompt the user to provide what was done during a pomodoro and add it to the journal file."
  ;; with a timestamp
	  (org-journal-new-entry nil)
	  (insert (concat (read-string "What did you do in this Pomodoro? ") " :POMODORO:"))
	  ;; And close org-journal window
	  (delete-window))

(defun pomidor-after-work-hook ()
  "Hook to execute after work.  Right when we enter the break state."
  (let ((state (pomidor--current-state)))
    (if (pomidor--break state)
	  (pomidor-insert-org-journal))))

(advice-add 'pomidor-break :after #'pomidor-after-work-hook)

;; Activate helm fuzzy matching 05/09/20
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
;;(setq helm-completion-style 'helm-fuzzy)
(setq helm-completion-style 'emacs)
(setq completion-styles (if (version<= emacs-version "27.0") '(helm-flex) '(flex)))

;; set up Scheme for SICP 05/09/20
(setq scheme-program-name "/usr/local/bin/scheme")
;; set up Sheme for literate programming org mode 09/11/20
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))
(setq org-confirm-babel-evaluate nil)

;; 05/21/20
(global-set-key [f9] 'toggle-truncate-lines)

;; 05/28/20 elmacro mode, convert macros to elisp
(use-package elmacro)
(elmacro-mode)

(use-package elfeed)
(use-package elfeed-org)
(elfeed-org)
(global-set-key (kbd "C-x w") 'elfeed)
(setq browse-url-browser-function 'eww-browse-url)

;; 05/28/20 gruvbox theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)))


;; (use-package modus-operandi-theme :ensure)
;; (load-theme 'modus-operandi t)

;; (use-package modus-vivendi-theme :ensure)
;; (load-theme 'modus-vivendi t)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Enable markdown export org mode
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;(use-package company-capf)
(use-package company-lsp)
;;(require 'company-lsp)
;;(push 'company-capf company-backends)
(push 'company-lsp company-backends)

;; Set up emacsclient for editing commands. See Bashrc too 08/07/20
(server-start)

;; Add Flycheck to python mode and remove flymake
;;(setq flycheck-python-pylint-executable "python3")
;;(add-hook 'python-mode-hook 'flycheck-mode)
;;(remove-hook 'elpy-modules 'elpy-module-flymake)

;;(add-hook 'python-mode-hook 'my-python-mode-hook)

;;(setq lsp-keymap-prefix "C-x l")

;;(add-hook 'python-mode-hook 'whitespace-mode)


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))



(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	   (python-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration)))))
     :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
     :commands lsp)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)

(use-package lsp-ui
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (python-mode . lsp-ui-mode)))

;; (use-package whitespace
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'whitespace-mode))
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;; Disable menu bar 07/15/20
(menu-bar-mode -1)

;; Add pass major mode 08/07/20
(use-package pass)



;; Nice! Show helmp completion in a posframe. "Floating" Window. Very neat, and doesn't mess up with the buffer 23/08/2020
;;(use-package helm-posframe)
;;(helm-posframe-enable)
;;Doesnt work very well, sometimes the floating windo get stuck

;;(require 'org-attach-git)

(use-package org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
;;(add-hook 'python-mode 'fci-mode)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
