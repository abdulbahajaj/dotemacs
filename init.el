(setq gc-cons-threshold most-positive-fixnum)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" default))
 '(horizontal-scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load (concat user-emacs-directory "core/packages"))

(load (concat user-emacs-directory "core/core"))
(load (concat user-emacs-directory "core/keybindings"))
(e "~/org/timeline.org")
(setq gc-cons-threshold 16777216)
