;; -*- lexical-binding: t; -*-

;;;###autoload
(defmacro ilm (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

;;;###autoload
(defun rubicon/escape ()
  (interactive)
  (evil-ex-nohighlight)
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 ;; quit the minibuffer if open.
	 (abort-recursive-edit))
	;; don't abort macros
	((or defining-kbd-macro executing-kbd-macro) nil)
	;; Back to the default
	((keyboard-quit))))

;; split creation and navigation
;;;###autoload
(defun rubicon/split-window (pos)
  (cond ((string= pos "right")
	 (split-window-horizontally)
	 (evil-window-right 1))
	((string= pos "left")
	 (split-window-horizontally))
	((string= pos "down")
	 (split-window-vertically)
	 (evil-window-down 1))
	((string= pos "up")
	 (split-window-vertically))))


;; Doom helper functions
;;;###autoload
(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
	 (this-buffer (current-buffer))
	 (that-window (windmove-find-other-window direction nil this-window))
	 (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
	      (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
	(funcall (pcase direction
		   ('left  #'evil-window-move-far-left)
		   ('right #'evil-window-move-far-right)
		   ('up    #'evil-window-move-very-top)
		   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
	(setq that-window
	      (split-window this-window nil
			    (pcase direction
			      ('up 'above)
			      ('down 'below)
			      (_ direction))))
	(setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
	(switch-to-buffer that-buffer))
      (with-selected-window that-window
	(switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))

;;;###autoload
(defun +evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))

;;;###autoload
(defun +evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))

;;;###autoload
(defun +evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))

;;;###autoload
(defun rubicon/gen-random-str ()
  (--reduce (format "%s%d" acc (random 10000)) (number-sequence 0 8)))

;;;###autoload
(defun rubicon/create-disposable-dir ()
  (interactive)
  (let ((path (format "~/scrap/%s" (rubicon/gen-random-str))))
    (dired-create-directory path)
    (e path)))

;;;###autoload
(defmacro multi-advice (name where fn-list args &rest body)
  `(progn
     (defun ,name ,args ,@body)
     (--map (advice-add it ,where (quote ,name)) ,fn-list)))

;;;###autoload
(defun rubicon/workspace-delete ()
  (interactive)
  (persp-kill (persp-current-name)))

;;;###autoload
(defun rubicon/workspace-get-marked-list ()
  (--map
   (if (string= (persp-current-name) it)
       (concat ">" it "<")
     (concat " " it " "))
   (persp-names)))

;;;###autoload
(defun rubicon/workspace-show-all ()
  (interactive)
  (message
   (string-join (rubicon/workspace-get-marked-list) " ")))

;;;###autoload
(defun rubicon/workspace-is-last-buffer? ()
  (interactive)
  (not (> (length (persp-current-buffer-names)) 1)))

;;;###autoload
(defun rubicon/workspace-kill-current-buffer ()
  (interactive)
  (if (not (rubicon/workspace-is-last-buffer?))
      (kill-current-buffer)
    (message "Can't delete last workspace buffer")))

;;;###autoload
(defun rubicon/workspace-quit-window ()
  (interactive)
  (if (not (rubicon/workspace-is-last-buffer?))
      (quit-window)
    (message "Can't delete last workspace buffer")))

;;;###autoload
(defun rubicon/visible-buffers ()
  "Returns a list of all currently visible buffers"
  (mapcar 'window-buffer (window-list)))

;;;###autoload
(defun rubicon/workspace-current-get-all-buffers ()
  "Returns a list of all buffers in current perspective"
  (persp-current-buffers))

;;;###autoload
(defun rubicon/workspace-current-get-invisible-buffers ()
  "returns a list of all invisible buffers in current perspective"
  (-difference (rubicon/workspace-current-get-all-buffers)
	       (rubicon/visible-buffers)))

;;;###autoload
(defun rubicon/kill-selected-buffers (selected-buffers)
  "Kills all buffers given to it"
  (--map (kill-buffer it) selected-buffers))


;;;###autoload
(defun rubicon/workspace-current-get-other-buffers ()
  "Get buffers other than the current buffers in the current perspective"
  (-difference
   (rubicon/workspace-current-get-all-buffers)
   (list (current-buffer))))

;;;###autoload
(defun rubicon/workspace-kill-invisible-buffers ()
  "Kills all invisible buffers in perspective"
  (interactive)
  (rubicon/kill-selected-buffers
   (rubicon/workspace-current-get-invisible-buffers)))

;;;###autoload
(defun rubicon/workspace-kill-other-buffers ()
  "Kills all buffers other than current one in perspective"
  (interactive)
  (delete-other-windows)
  (rubicon/kill-selected-buffers
   (rubicon/workspace-current-get-other-buffers)))

;;;###autoload
(defun rubicon/print-and-copy (val)
  (message val)
  (kill-new val))

;;;###autoload
(defun rubicon/copy-path-to-buffer-file ()
  (interactive)
  (rubicon/print-and-copy
   (or (buffer-file-name) default-directory)))

;; Key bindings functions

(defconst rubicon/nvm-states
  '(normal visual motion))

(defmacro rubicon/define-leader (prefix)
  (let ((rb-definer-name (intern (concat "rubicon/leader-" prefix))))
    `(general-create-definer
       ,rb-definer-name
       :prefix ,prefix
       :keymaps 'override
       :states rubicon/nvm-states)))

(rubicon/define-leader "SPC")
(rubicon/define-leader "<f13>")
(rubicon/define-leader "<f15>")

(defmacro rubicon/create-fs-map (shortcut file-path)
  `(let* ((fn-name-str (concat "open-" ,file-path))
	  (fn-name (intern fn-name-str)))
     (defalias fn-name (ilm (e ,file-path)))
     (rubicon/leader-<f15> ,shortcut fn-name)))


;;;###autoload
(defun +ivy/projectile-find-file ()
  "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME or /, `counsel-file-jump' if invoked
from a non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.
The point of this is to avoid Emacs locking up indexing massive file trees."
  (interactive)
  ;; Spoof the command so that ivy/counsel will display the (well fleshed-out)
  ;; actions list for `counsel-find-file' on C-o. The actions list for the other
  ;; commands aren't as well configured or are empty.
  (let ((this-command 'counsel-find-file))
    (call-interactively
     (cond ((or (file-equal-p default-directory "~")
		(file-equal-p default-directory "/"))
	    #'counsel-find-file)

	   ((projectile-project-p default-directory)
	    (let ((files (projectile-current-project-files)))
	      (if (<= (length files) ivy-sort-max-size)
		  #'counsel-projectile-find-file
		#'projectile-find-file)))
	   (#'counsel-file-jump)))))


;;;###autoload
(defun rubicon/gen-random-str ()
  (--reduce (format "%s%d" acc (random 10000)) (number-sequence 0 8)))

;;;###autoload
(defun rubicon/create-disposable-dir ()
  (interactive)
  (let ((path (format "~/scrap/%s" (rubicon/gen-random-str))))
    (dired-create-directory path)
    (e path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Vterm functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun vterm-send-escape()
  (interactive)
  (vterm-send-key "<escape>"))

(with-eval-after-load 'vterm
;;;###autoload
  (defun vterm (&optional buffer-name)
    "Create a new vterm."
    (interactive)
    (let ((buffer (generate-new-buffer (or buffer-name "vterm"))))
      (with-current-buffer buffer
        (vterm-mode))
      (switch-to-buffer buffer))))

;;;###autoload
(defun vterm-run-and-go-up ()
  (interactive)
  (vterm-send-return)
  (vterm-send-up))

;;;###autoload
(defun my-vterm-normal-mode ()
  (interactive)
  (evil-force-normal-state)
  (vterm-copy-mode))

;;;###autoload
(defun my-vterm-insert ()
  (interactive)
  (vterm-copy-mode -1)
  (evil-insert 1))

;;;###autoload
(defun rubicon/vterm-append ()
  (interactive)
  (vterm-copy-mode -1)
  (evil-append 1))

;;;###autoload
(defun rubicon/vterm-clear ()
  (interactive)
  (vterm-clear-scrollback)
  (vterm-clear))

;;;###autoload
(defun vt-exec (str)
  (vterm-send-string str)
  (vterm-send-return))

;;;###autoload
(defun vt-eq (key val)
  (vt-exec
   (format "%s=\"%s\"" key val)))

;;;###autoload
(defun vt-alias (key val)
  (vt-exec
   (format "alias %s=\"%s\"" key val)))

;;;###autoload
(defun vt-export (key val)
  (vt-exec
   (format "export %s=%s" key val)))

;;;###autoload
(defun vt-append-path (path)
  (vt-export
   "PATH"
   (format "%s:$PATH" path)))

;;;###autoload
(defun vt-source-zshrc ()
  (interactive)
  (vt-exec "source ~/.zshrc"))

;;;###autoload
(defun vt-cd-to (path)
  (interactive)
  (vt-exec
   (format "cd %s" path)))

;;;###autoload
(defun vt-pusdh (path)
  (vt-exec
   (format "pushd %s" path)))

;;;###autoload
(defun vt-popd ()
  (vt-exec
   (format "popd")))

;;;###autoload
(defun vt-insert-command (cmd)
  (vterm-send-string cmd)
  (evil-insert 1))

;;;###autoload
(defun vt-ls ()
  (vt-exec "ls -la"))

;;;###autoload
(defun vt-clear-current-command ()
  (vterm-send-escape)
  (vterm-send-string "dd")
  (vterm-send-string "i"))

;;;###autoload
(defun vt-insert-at-start (cmd) ;; requires vi mode
  (vterm-send-escape)
  (vterm-send-string "m")
  (vterm-send-string "p")
  (vterm-send-string "0i")
  (vterm-send-string cmd)
  (vterm-send-escape)
  (vterm-send-string "`p")
  (let ((cmd-size (length cmd))
        (cursor 0))
    (while (< cursor cmd-size)
      (vterm-send-string "l")
      (setq cursor (+ cursor 1))))
  (vterm-send-string "a"))

;;;###autoload
(defun vt-inset-at-point (cmd)
  (vterm-send-escape)
  (vterm-send-string "i")
  (vterm-send-string cmd))

;;;###autoload
(defun vt-add-sudo ()
  (interactive)
  (vt-insert-at-start "sudo "))

;;;###autoload
(defun vt-add-chmod ()
  (interactive)
  (vt-insert-at-start "chmod u+x "))

;;;###autoload
(defun vt-rc ()
  (interactive)
  (vt-append-path "~/bin/")
  (vt-exec "bindkey -v")
  (vt-eq "PROMPT" "%n %5~# ")
  (vt-alias "l" "ls")
  (vt-alias "c" "clear")
  (vt-alias "ktl" "kubectl")
  ;; (vt-alias "la" "ls -lAh")
  (vt-alias "la" "ls -lAh")
  (vt-alias "ll" "ls -lh")
  (vt-alias "pod" "popd")
  (vt-alias "pd" "pushd")
  (vt-alias "...." "cd ../../..")
  (vt-alias "..." "cd ../..")
  (vt-alias ".." "cd .."))

;;;###autoload
(defun rubicon/eshell-here ()
  (interactive)
  (let ((new-buffer (generate-new-buffer "*eshell*")))
    (switch-to-buffer new-buffer)
    (eshell-mode)))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
	     in (cl-remove-if-not #'listp org-todo-keywords)
	     for keywords =
	     (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
				     (match-string 1 x)
				   x))
		     keyword-spec)
	     if (eq type 'sequence)
	     if (member keyword keywords)
	     return keywords)))

(defun rubicon/get-org-path (org-file)
  (format "%s/%s" rubicon/org-dir-path org-file))

;;;###autoload
(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.
If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (let* ((context (org-element-context))
	 (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
	    type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
		   (member "TOC" (org-get-tags)))
	      (toc-org-insert-toc)
	      (message "Updating table of contents"))
	     ((string= "ARCHIVE" (car-safe (org-get-tags)))
	      (org-force-cycle-archived))
	     ((or (org-element-property :todo-type context)
		  (org-element-property :scheduled context))
	      (org-todo
	       (if (eq (org-element-property :todo-type context) 'done)
		   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
		       'todo)
		 'done)))
	     (t
	      (+org--refresh-inline-images-in-subtree)
	      (org-clear-latex-preview)
	      (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
	   (org-table-calc-current-TBLFM)
	 (ignore-errors
	   (save-excursion
	     (goto-char (org-element-property :contents-begin context))
	     (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate arg)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
		  (bound-and-true-p evil-local-mode))
	 (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies arg)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block arg))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview arg))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
	      (path (org-element-property :path lineage)))
	 (if (or (equal (org-element-property :type lineage) "img")
		 (and path (image-type-from-file-name path)))
	     (+org--refresh-inline-images-in-subtree)
	   (org-open-at-point arg))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
	 (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_
       (if (or (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil  t)
	       (org-in-regexp org-link-any-re nil t))
	   (call-interactively #'org-open-at-point)
	 (+org--refresh-inline-images-in-subtree))))))

(defun rubicon/create-org-writing ()
  (interactive)
  (e (format "%s/writings/%s.org" rubicon/org-dir-path (rubicon/gen-random-str))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Modeline 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun rubicon/enable-modeline ()
  (setq mode-line-format rubicon--modeline-format))

;;;###autoload
(defun rubicon/disable-modeline ()
  (setq mode-line-format nil))

;;;###autoload
(defun rubicon--modeline-face (inherits background-color)
  `((t :inherit ,inherits
       :background ,background-color
       :box (:line-width 1 :color ,background-color)
       :height 130)))

;;;###autoload
(defun rubicon/relative-default-dir ()
  (s-replace-regexp  rubicon--home-path-rg-starts-with
		     "~"
		     default-directory))

;;;###autoload
(defun rubicon/turn-fringes-off ()
  (setq-local left-fringe-width 0
	      right-fringe-width 0))

;;;###autoload
(defun rubicon/turn-fringes-on ()
  (let ((width 8))
    (setq-local left-fringe-width width
		right-fringe-width 0) 
    (set-window-fringes nil width nil)))


;;;###autoload
(defun rubicon/copy-git-link-at-point ()
  (interactive)
  (rubicon/print-and-copy (browse-at-remote-get-url)))


(defun rubicon/edit-last-kill ()
  (interactive)
  (e (car kill-ring)))


(provide 'core)
;;; core.el ends here
