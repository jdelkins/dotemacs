;;; init-term.el -- Wire in xterm-color for comint and eshell
;-*-Emacs-Lisp-*-

;;; Commentary:
;; See https://github.com/atomontage/xterm-color
;;; Code:

(require 'package)

(use-package xterm-color :ensure t)

(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(require 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))


(provide 'init-term)
;;; init-term.el ends here
