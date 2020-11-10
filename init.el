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
<<<<<<< HEAD
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
=======
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
>>>>>>> cda7d53fcc42bbbb3b5a7593b1d90aac99e3f9aa
