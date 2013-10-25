(defvar f-sandbox-path
  (expand-file-name "sandbox" (file-name-directory load-file-name)))

(defmacro with-default-directory (&rest body)
  `(let ((default-directory "/default/directory")) ,@body))

(defmacro with-sandbox (&rest body)
  `(let ((default-directory f-sandbox-path))
     (mapc
      (lambda (file)
        (if (or (file-regular-p file) (file-symlink-p file))
            (delete-file file)
          (delete-directory file t)))
      (directory-files f-sandbox-path t "^[^\\.\\.?]"))
     ,@body))

(defun should-exist (filename &optional content)
  (let ((path (expand-file-name filename f-sandbox-path)))
    (should (file-exists-p path))
    (when content
      (with-temp-buffer
        (if (multibyte-string-p content)
            (insert-file-contents path)
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path))
        (should (string= (buffer-string) content))))))

(defun should-not-exist (filename)
  (let ((path (expand-file-name filename f-sandbox-path)))
    (should-not (file-exists-p path))))

(defun chmod (file mode)
  (let ((chmod (executable-find "chmod"))
        (args (list mode (f-expand file f-sandbox-path))))
    (apply 'call-process (append (list chmod nil nil nil) args))))
