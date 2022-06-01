;;; Disable package.el in favor of straight.
(setq package-enable-at-startup nil)

;;; Profile emacs startup
(setq me/default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 500 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
	    (setq gc-cons-threshold me/default-gc-cons-threshold)
            (message "Emacs loaded in %s with %d garbage collections"
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
