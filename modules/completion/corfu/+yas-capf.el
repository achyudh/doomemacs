;;; completion/corfu/+yas-capf.el -*- lexical-binding: t; -*-

;;
;;; Capf for yasnippet. Inspired by @elken's yas-capf.
(defcustom yas-capf-lookup-by 'key
  "The method used to lookup candidates."
  :type '(choice
          (const :tag "Key" key)
          (const :tag "Name" name))
  :group 'yasnippet)

(defun yas-capf--exit (_ status)
  "Actually expand the snippet, if STATUS is \"finished\"."
  (when (string= "finished" status)
    (yas-expand)))

(defvar yas-capf--properties
  (list :annotation-function (lambda (_) "(Snippet)")
        :company-kind (lambda (_) 'snippet)
        :exit-function #'yas-capf--exit
        :exclusive 'no)
  "Return a list of extra properties for text at BEG through END.")

(defun yas-capf--maybe-candidate (special-req input template)
  "Take a  and use it to return a propertized string suitable for
completion or nil if it no longer matches INPUT."
  (let ((key (yas--template-key template))
        (name (yas--template-name template))
        (condition (yas--template-condition template)))
    (when (and (string-prefix-p input key)
               (yas--template-can-expand-p condition special-req))
      (let ((key-or-name (pcase yas-capf-lookup-by
                           ('key key)
                           ('name name))))
        (propertize key-or-name
                    'yas-template template)))))

(defun yas-capf--maybe-invalidate (old-completions input)
  "Use INPUT to filter OLD-COMPLETIONS and return a pair new-(invalidate-fun,
completions)."
  (let* ((non-nil-completions (seq-filter #'identity old-completions))
         (templates (mapcar (apply-partially #'get-text-property 0 'yas-template)
                            non-nil-completions))
         (valid-completions (mapcar
                             (apply-partially #'yas-capf--maybe-candidate (yas--require-template-specific-condition-p) input)
                             templates)))
    (cons (apply-partially #'yas-capf--maybe-invalidate valid-completions)
          valid-completions)))

(defun yas-capf--list (input)
  "Use INPUT to compute and filter the initial table."
  (when-let* ((templates (yas--all-templates (yas--get-snippet-tables major-mode)))
              (init-table (mapcar (apply-partially #'yas-capf--maybe-candidate (yas--require-template-specific-condition-p) input)
                                  templates)))
    (cons (apply-partially #'yas-capf--maybe-invalidate init-table)
          init-table)))

(defun yas-capf (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'yas-capf)
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        `(,beg ,end
          ,(cape--table-with-properties
            (cape--cached-table beg end #'yas-capf--list)
            :category 'yasnippet)
          ,@yas-capf--properties)))))
