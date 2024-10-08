(defconst emacs-start-time (current-time))

(require 'org)

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
