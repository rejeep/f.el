(defvar f-sandbox-path
  (expand-file-name "sandbox" (file-name-directory load-file-name)))

(defun should-exist (filename &optional content)
  (let ((path (expand-file-name filename f-sandbox-path)))
    (should (file-exists-p path))
    (when content
      (with-temp-buffer
        (insert-file-contents-literally path)
        (should (equal (buffer-string) content))))))

(defmacro with-sandbox (&rest body)
  `(let ((default-directory f-sandbox-path))
     (mapc
      (lambda (file)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file nil)))
      (directory-files f-sandbox-path t "^[^\\.\\.?]"))
     ,@body))

(defmacro with-no-messages (&rest body)
  `(let ((messages))
     (flet ((message
             (format-string &rest args)
             (add-to-list 'messages (format format-string args) t)))
       ,@body
       (should-not messages))))

(ert-deftest f-destructive-test/write-no-content-relative-path ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt"))
   (should-exist "foo.txt")))

(ert-deftest f-destructive-test/write-no-content-absolute-path ()
  (with-sandbox
   (let* ((dirname (expand-file-name "bar" f-sandbox-path))
          (filename (expand-file-name "foo.txt" dirname)))
     (make-directory dirname)
     (with-no-messages
      (f-write filename))
     (should-exist "bar/foo.txt"))))

(ert-deftest f-destructive-test/write-with-content ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt" "FOO"))
   (should-exist "foo.txt" "FOO")))

(ert-deftest f-destructive-test/write-override ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt" "FOO")
    (f-write "foo.txt" "BAR"))
   (should-exist "foo.txt" "BAR")))
