(ert-deftest f-write-bytes-test/multibyte-string ()
  (with-sandbox
   (let ((err (should-error (f-write-bytes "☺ ☹" "foo.txt")
                            :type 'wrong-type-argument)))
     (should (equal (cdr err)
                    (list 'f-unibyte-string-p "☺ ☹"))))))

(ert-deftest f-write-bytes-test/unibyte-string ()
  (with-sandbox
   ;; Let's take some random bytes
   (let ((bytes (apply #'unibyte-string (-map #'random (-repeat 100 255)))))
     ;; Make a string of our bytes
     (f-write-bytes bytes "foo.txt")
     (should-exist "foo.txt" bytes))))

(ert-deftest f-write-text-test/unibyte-string ()
  (with-sandbox
   (f-write-text (unibyte-string 1 2 3 4 5) 'utf-8 "foo.txt")
   ;; Emacs only makes multibyte strings if actually required.
   (f-write-text "bar" 'utf-8 "bar.txt")
   (should-exist "bar.txt" "bar")))

(ert-deftest f-write-text-test/multibyte-string ()
  (with-sandbox
   (f-write-text "☺ ☹" 'utf-8 "foo.txt")
   (should-exist "foo.txt" (unibyte-string 226 152 186 32 226 152 185))
   (f-write-text "blök" 'iso-8859-1 "foo.txt")
   (should-exist "foo.txt" (unibyte-string 98 108 246 107))))

(ert-deftest f-read-bytes ()
  (with-sandbox
   (let ((bytes (apply #'unibyte-string (-map #'random (-repeat 100 255)))))
     (f-write-bytes bytes "foo.txt")
     (let ((content (f-read-bytes "foo.txt")))
       (should-not (multibyte-string-p content))
       (should (f-unibyte-string-p content))
       (should (string= content bytes))))))

(ert-deftest f-read-text ()
  (with-sandbox
   (f-write-bytes (unibyte-string 226 152 185 32 226 152 186) "foo.txt")
   (let ((text (f-read-text "foo.txt" 'utf-8)))
     (should (string= text "☹ ☺"))
     (should (multibyte-string-p text)))
   (f-write-bytes (unibyte-string 252 98 101 114) "foo.txt")
   (let ((text (f-read-text "foo.txt" 'iso-8859-1)))
     (should (string= text "über"))
     (should (multibyte-string-p text)))))

(ert-deftest f-read-text-no-coding-specified ()
  (with-sandbox
   (f-write-text "text" 'utf-8 "foo.txt")
   (should (equal (f-read-text "foo.txt") "text"))))

;;; Obsolete functions
(ert-deftest f-read-test/empty ()
  (with-sandbox
   (f-write "foo.txt")
   (should (equal (f-read "foo.txt") ""))))

(ert-deftest f-read-test/with-content ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (should (equal (f-read "foo.txt") "FOO"))))

(ert-deftest f-write-test/no-content-relative-path ()
  (with-sandbox
   (f-write "foo.txt")
   (should-exist "foo.txt")))

(ert-deftest f-write-test/no-content-absolute-path ()
  (with-sandbox
   (let* ((dirname (expand-file-name "bar" f-sandbox-path))
          (filename (expand-file-name "foo.txt" dirname)))
     (make-directory dirname)
     (f-write filename)
     (should-exist "bar/foo.txt"))))

(ert-deftest f-write-test/with-content ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (should-exist "foo.txt" "FOO")))

(ert-deftest f-write-test/override ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (f-write "foo.txt" "BAR")
   (should-exist "foo.txt" "BAR")))

(ert-deftest f-write-test/append ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (should-exist "foo.txt" "FOO")
   (f-write "foo.txt" "BAR" 'append)
   (should-exist "foo.txt" "FOOBAR")))
