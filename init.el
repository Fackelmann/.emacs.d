(defconst emacs-start-time (current-time))

;; I'm using straight.el for reproducibility and source control:
(setq straight-base-dir "~/.emacs.d/straight/")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Ensure use-package uses straight.el by default
(straight-use-package 'use-package)

;;And make it the default with use-package
(setq straight-use-package-by-default t)

;; Load org package right after that and before org-babel-load file to avoid issues with
;; org versions
(straight-use-package 'org)

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))


(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)

(put 'dired-find-alternate-file 'disabled nil)
