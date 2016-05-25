;;; init-powerline.el --- my setup for powerline

;;; Commentary:
;; Use the [selenized] colors, adapted to the solarized color name construct
;;
;; [selenized]: https://github.com/janek-warchol/selenized

;;; Code:

(setq my-pl-selenized
      '((:base03  . "#154053")
        (:base02  . "#245970")
        (:base01  . "#7c95a0")
        (:base00  . "#a8bcc3")
        (:base0   . "#a8bcc3")
        (:base1   . "#c4d8df")
        (:base2   . "#e4e4e4")
        (:base3   . "#ffffd7")
        (:yellow  . "#d8b033")
        (:orange  . "#d75f00")
        (:red     . "#fc5851")
        (:magenta . "#f16dc5")
        (:violet  . "#5f5faf")
        (:blue    . "#4e97f5")
        (:cyan    . "#41c7b9")
        (:green   . "#78b93e")))

(defface my-pl-segment1-active
  `((t (:foreground ,(assoc-default :base02 my-pl-selenized) :background ,(assoc-default :base0 my-pl-selenized))))
  "Powerline first segment active face.")
(defface my-pl-segment2-active
  `((t (:foreground ,(assoc-default :base2 my-pl-selenized) :background ,(assoc-default :base01 my-pl-selenized))))
  "Powerline second segment active face.")
(defface my-pl-segment3-active
  `((t (:foreground ,(assoc-default :base1 my-pl-selenized) :background ,(assoc-default :base02 my-pl-selenized))))
  "Powerline third segment active face.")

(defface my-pl-segment1-inactive
  `((t (:foreground ,(assoc-default :base0 my-pl-selenized) :background ,(assoc-default :base02 my-pl-selenized))))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-inactive
  `((t (:foreground ,(assoc-default :base0 my-pl-selenized) :background ,(assoc-default :base02 my-pl-selenized))))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-inactive
  `((t (:foreground ,(assoc-default :base0 my-pl-selenized) :background ,(assoc-default :base02 my-pl-selenized))))
  "Powerline third segment inactive face.")

(defun air--powerline-default-theme ()
  "Set up my custom Powerline with Evil indicators."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
                          (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
                          (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
                          (separator-left (if active (intern (format "powerline-%s-%s"
                                                                     (powerline-current-separator)
                                                                     (car powerline-default-separator-dir)))
                                            (lambda (seg _) (powerline-raw (char-to-string #xe0b1) seg))))
                          (separator-right (if active (intern (format "powerline-%s-%s"
                                                                      (powerline-current-separator)
                                                                      (cdr powerline-default-separator-dir)))
                                             (lambda (seg _) (powerline-raw (char-to-string #xe0b3) seg))))
                          (lhs (list (if active
                                         (let ((evil-face (powerline-evil-face)))
                                           (if evil-mode
                                               (powerline-raw (powerline-evil-tag) evil-face))))
                                     (if (and active evil-mode)
                                         (funcall separator-left (powerline-evil-face) seg1))

                                     ;;(when powerline-display-buffer-size
                                     ;;  (powerline-buffer-size nil 'l))
                                     ;;(when powerline-display-mule-info
                                     ;;  (powerline-raw mode-line-mule-info nil 'l))

                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object seg1 'l))
                                     (powerline-major-mode seg1 'l)
                                     (powerline-process seg1)
                                     (powerline-minor-modes seg1 'l)
                                     (powerline-narrow seg1 'l)
                                     (powerline-raw " " seg1)

                                     (funcall separator-left seg1 seg2)

                                     (powerline-buffer-id seg2 'l)
                                     (powerline-raw "[%*]" seg2 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format seg2 'l))
                                     (powerline-raw " " seg2)

                                     (funcall separator-left seg2 seg3)

                                     (powerline-vc seg3 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) seg3 'l))))

                          (rhs (list (powerline-raw global-mode-string seg3 'r)
                                     (funcall separator-right seg3 seg2)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) seg2 'l))
                                     (powerline-raw "%4l" seg2 'l)
                                     (powerline-raw ":" seg2 'l)
                                     (powerline-raw "%3c" seg2 'r)
                                     (funcall separator-right seg2 seg1)
                                     (powerline-raw " " seg1)
                                     (powerline-raw "%6p" seg1 'r)
                                     (when powerline-display-hud
                                       (powerline-hud seg1 seg3)))))
                     (concat (powerline-render lhs)
                             (powerline-fill seg3 (powerline-width rhs))
                             (powerline-render rhs)))))))
  
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator (if (display-graphic-p) 'arrow
                                      nil))
  (air--powerline-default-theme))

(use-package powerline-evil
  :ensure t)

(provide 'init-powerline)
;;; init-powerline.el ends here
