(require 'cl)
(require 'el-mock)

(defvar f-sandbox-path
  (expand-file-name "sandbox" (file-name-directory load-file-name)))

(defmacro f-flet (specs &rest body)
  (declare (indent 1) (debug t))
  (let ((flet (if (fboundp 'cl-flet) 'cl-flet 'flet)))
    `(,flet ,specs ,@body)))

(defmacro with-default-directory (&rest body)
  `(let ((default-directory "/default/directory")) ,@body))

(defmacro with-no-messages (&rest body)
  `(let ((messages))
     (f-flet ((message
             (format-string &rest args)
             (add-to-list 'messages (format format-string args) t)))
       ,@body
       (should-not messages))))


(defmacro with-sandbox (&rest body)
  `(let ((default-directory f-sandbox-path))
     (mapc
      (lambda (file)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file)))
      (directory-files f-sandbox-path t "^[^\\.\\.?]"))
     (with-no-messages ,@body)))

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
