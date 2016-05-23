;;; init-platform --- Platform-specific settings
;;; Commentary:

;;; Code:
(require 'init-fonts)

;; This must run after window setup or it seems to have no effect.
(add-hook 'window-setup-hook
          (lambda ()
            (when (memq window-system '(mac ns))
              (add-to-list 'default-frame-alist '(font . "Hack"))
              (set-face-attribute 'default nil :font "Hack")
              (sanityinc/set-frame-font-size 14)
              (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)
              ;; The OS X visible bell is buggy as hell (according to
              ;; aaronbieber, not sure personally).  However, this
              ;; code is freaks out on windows (not sure about x).
              (defvar air-bell-ringing nil
                "Whether my visual bell is currently being rung.

This prevents simultaneously ringing two bells and falling into a race
condition where the bell visualization never clears.")
              (setq ring-bell-function (lambda ()
                                         (if (not air-bell-ringing)
                                             (let* ((bg (face-background 'default))
                                                    (fg (face-foreground 'default))
                                                    (reset `(lambda ()
                                                              (set-face-background 'default ,bg)
                                                              (set-face-foreground 'default ,fg)
                                                              (setq air-bell-ringing nil))))
                                               (set-face-background 'default "NavajoWhite4")
                                        ;(set-face-foreground 'default "black")
                                               (setq air-bell-ringing t)
                                               (run-with-timer 0.05 nil reset))))))

            (when (memq window-system '(x))
              (add-to-list 'default-frame-alist '(font . "Pragmata Pro"))
              (set-face-attribute 'default nil :font "Pragmata Pro")
              (sanityinc/set-frame-font-size 16)
              (setq visible-bell t))

            (when (memq window-system '(w32))
              (setq visible-bell t))

            (when (fboundp 'powerline-reset)
              (powerline-reset))))

;; Display emoji on Macs where the font is already there.
(when (memq window-system '(mac))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'init-platform)
;;; init-platform.el ends here
