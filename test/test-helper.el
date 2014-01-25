(defvar f-sandbox-path
  (expand-file-name "sandbox" (file-name-directory load-file-name)))

(defvar f-trash-path
  (expand-file-name "trash" (file-name-directory load-file-name)))

(defmacro with-default-directory (&rest body)
  `(let ((default-directory "/default/directory")) ,@body))

(defmacro with-sandbox (&rest body)
  `(let ((default-directory f-sandbox-path))
     (mapc
      (lambda (path)
        (when (file-exists-p path)
          (delete-directory path 'recursive))
        (make-directory path))
      (list f-sandbox-path f-trash-path))
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
