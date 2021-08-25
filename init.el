;; -*- lexical-binding: t; -*-

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

(defun rubicon/load (path)
  (load (concat user-emacs-directory path)))

(dolist (fname '("core/packages" "core/core" "core/config" "core/keybindings"))
  (rubicon/load fname))


(if (file-exists-p (concat user-emacs-directory "local.el"))
    (rubicon/load  "local")
  (print "init.el was not found"))

(print (concat "emacs init time is " (emacs-init-time)))
