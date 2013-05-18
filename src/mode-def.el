;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding minor mode

(defgroup zencoding nil
  "Customization group for zencoding-mode."
  :group 'convenience)

(defun zencoding-expr-on-line ()
  "Extract a zencoding expression and the corresponding bounds
   for the current line."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties start end))
         (expr (zencoding-regex "\\([ \t]*\\)\\([^\n]+\\)" line 2)))
    (if (first expr)
        (list (first expr) start end))))

(defcustom zencoding-indentation 4
  "Number of spaces used for indentation."
  :type '(number :tag "Spaces")
  :group 'zencoding)

(defun zencoding-prettify (markup indent)
  (let ((first-col (format (format "%%%ds" indent) ""))
        (tab       (format (format "%%%ds" zencoding-indentation) "")))
    (concat first-col
            (replace-regexp-in-string "\n" (concat "\n" first-col)
                                      (replace-regexp-in-string "    " tab markup)))))

(defun zencoding-transform (input)
  (if (eql major-mode 'css-mode)
      (zencoding-css-transform input)
    (zencoding-html-transform input)))

;;;###autoload
(defun zencoding-expand-line (arg)
  "Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'."
  (interactive "P")
  (let* ((here (point))
         (preview (if zencoding-preview-default (not arg) arg))
         (beg (if preview
                  (progn
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (point))
                (when mark-active (region-beginning))))
         (end (if preview
                  (progn
                    (end-of-line)
                    (skip-chars-backward " \t")
                    (point))
                (when mark-active (region-end)))))
    (if beg
        (progn
          (goto-char here)
          (zencoding-preview beg end))
      (let ((expr (zencoding-expr-on-line)))
        (if expr
            (let ((markup (zencoding-transform (first expr))))
              (when markup
                (let ((pretty (zencoding-prettify markup (current-indentation))))
                  (save-excursion
                    (delete-region (second expr) (third expr))
                    (zencoding-insert-and-flash pretty))))))))))

(defvar zencoding-mode-keymap nil
  "Keymap for zencode minor mode.")

(if zencoding-mode-keymap
    nil
  (progn
    (setq zencoding-mode-keymap (make-sparse-keymap))
    (define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
    (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-line)))

;;;###autoload
(define-minor-mode zencoding-mode
  "Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'."
  :lighter " Zen"
  :keymap zencoding-mode-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding yasnippet integration

(defun zencoding-transform-yas (input)
  (let* ((leaf-count 0)
         (zencoding-leaf-function
          (lambda ()
            (format "$%d" (incf leaf-count)))))
    (zencoding-transform input)))

;;;###autoload
(defun zencoding-expand-yas ()
  (interactive)
  (let ((expr (zencoding-expr-on-line)))
    (if expr
        (let* ((markup (zencoding-transform-yas (first expr)))
               (filled (replace-regexp-in-string "><" ">\n<" markup)))
          (delete-region (second expr) (third expr))
          (insert filled)
          (indent-region (second expr) (point))
          (yas/expand-snippet
           (buffer-substring (second expr) (point))
           (second expr) (point))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Real-time preview
;;

;;;;;;;;;;
;; Lennart's version

(defvar zencoding-preview-input nil)
(make-local-variable 'zencoding-preview-input)
(defvar zencoding-preview-output nil)
(make-local-variable 'zencoding-preview-output)
(defvar zencoding-old-show-paren nil)
(make-local-variable 'zencoding-old-show-paren)

(defface zencoding-preview-input
  '((default :box t :inherit secondary-selection))
  "Face for preview input field."
  :group 'zencoding)

(defface zencoding-preview-output
  '((default :inherit highlight))
  "Face for preview output field."
  :group 'zencoding)

(defvar zencoding-preview-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'zencoding-preview-accept)
    (define-key map (kbd "<return>") 'zencoding-preview-accept)
    (define-key map [(control ?g)] 'zencoding-preview-abort)
    map))

(defun zencoding-preview-accept ()
  (interactive)
  (let ((ovli zencoding-preview-input))
    (if (not (and (overlayp ovli)
                  (bufferp (overlay-buffer ovli))))
        (message "Preview is not active")
      (let* ((indent (current-indentation))
             (markup (zencoding-preview-transformed indent)))
        (when markup
          (delete-region (line-beginning-position) (overlay-end ovli))
          (zencoding-insert-and-flash markup)))))
  (zencoding-preview-abort))

(defvar zencoding-flash-ovl nil)
(make-variable-buffer-local 'zencoding-flash-ovl)

(defun zencoding-remove-flash-ovl (buf)
  (with-current-buffer buf
    (when (overlayp zencoding-flash-ovl)
      (delete-overlay zencoding-flash-ovl))
    (setq zencoding-flash-ovl nil)))

(defcustom zencoding-preview-default t
  "If non-nil then preview is the default action.
This determines how `zencoding-expand-line' works by default."
  :type 'boolean
  :group 'zencoding)

(defcustom zencoding-insert-flash-time 0.5
  "Time to flash insertion.
Set this to a negative number if you do not want flashing the
expansion after insertion."
  :type '(number :tag "Seconds")
  :group 'zencoding)

(defun zencoding-insert-and-flash (markup)
  (zencoding-remove-flash-ovl (current-buffer))
  (let ((here (point)))
    (insert markup)
    (setq zencoding-flash-ovl (make-overlay here (point)))
    (overlay-put zencoding-flash-ovl 'face 'zencoding-preview-output)
    (when (< 0 zencoding-insert-flash-time)
      (run-with-idle-timer zencoding-insert-flash-time
                           nil 'zencoding-remove-flash-ovl (current-buffer)))))

;;;###autoload
(defun zencoding-preview (beg end)
  "Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it."
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (zencoding-preview-abort)
  (if (not beg)
      (message "Region not active")
    (setq zencoding-old-show-paren show-paren-mode)
    (show-paren-mode -1)
    (let ((here (point)))
      (goto-char beg)
      (forward-line 1)
      (unless (= 0 (current-column))
        (insert "\n"))
      (let* ((opos (point))
             (ovli (make-overlay beg end nil nil t))
             (ovlo (make-overlay opos opos))
             (info (propertize " Zen preview. Choose with RET. Cancel by stepping out. \n"
                               'face 'tooltip)))
        (overlay-put ovli 'face 'zencoding-preview-input)
        (overlay-put ovli 'keymap zencoding-preview-keymap)
        (overlay-put ovlo 'face 'zencoding-preview-output)
        (overlay-put ovlo 'before-string info)
        (setq zencoding-preview-input  ovli)
        (setq zencoding-preview-output ovlo)
        (add-hook 'before-change-functions 'zencoding-preview-before-change t t)
        (goto-char here)
        (add-hook 'post-command-hook 'zencoding-preview-post-command t t)))))

(defvar zencoding-preview-pending-abort nil)
(make-variable-buffer-local 'zencoding-preview-pending-abort)

(defun zencoding-preview-before-change (beg end)
  (when
      (or (> beg (overlay-end zencoding-preview-input))
          (< beg (overlay-start zencoding-preview-input))
          (> end (overlay-end zencoding-preview-input))
          (< end (overlay-start zencoding-preview-input)))
    (setq zencoding-preview-pending-abort t)))

(defun zencoding-preview-abort ()
  "Abort zen code preview."
  (interactive)
  (setq zencoding-preview-pending-abort nil)
  (remove-hook 'before-change-functions 'zencoding-preview-before-change t)
  (when (overlayp zencoding-preview-input)
    (delete-overlay zencoding-preview-input))
  (setq zencoding-preview-input nil)
  (when (overlayp zencoding-preview-output)
    (delete-overlay zencoding-preview-output))
  (setq zencoding-preview-output nil)
  (remove-hook 'post-command-hook 'zencoding-preview-post-command t)
  (when zencoding-old-show-paren (show-paren-mode 1)))

(defun zencoding-preview-post-command ()
  (condition-case err
      (zencoding-preview-post-command-1)
    (error (message "zencoding-preview-post: %s" err))))

(defun zencoding-preview-post-command-1 ()
  (if (and (not zencoding-preview-pending-abort)
           (<= (point) (overlay-end zencoding-preview-input))
           (>= (point) (overlay-start zencoding-preview-input)))
      (zencoding-update-preview (current-indentation))
    (zencoding-preview-abort)))

(defun zencoding-preview-transformed (indent)
  (let* ((string (buffer-substring-no-properties
		  (overlay-start zencoding-preview-input)
		  (overlay-end zencoding-preview-input))))
    (let ((output (zencoding-transform string)))
      (when output
        (zencoding-prettify output indent)))))

(defun zencoding-update-preview (indent)
  (let* ((pretty (zencoding-preview-transformed indent))
         (show (when pretty
                 (propertize pretty 'face 'highlight))))
    (when show
      (overlay-put zencoding-preview-output 'after-string
                   (concat show "\n")))))

(provide 'zencoding-mode)

;;; zencoding-mode.el ends here
