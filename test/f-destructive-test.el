(ert-deftest f-write-test/no-content-relative-path ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt"))
   (should-exist "foo.txt")))

(ert-deftest f-write-test/no-content-absolute-path ()
  (with-sandbox
   (let* ((dirname (expand-file-name "bar" f-sandbox-path))
          (filename (expand-file-name "foo.txt" dirname)))
     (make-directory dirname)
     (with-no-messages
      (f-write filename))
     (should-exist "bar/foo.txt"))))

(ert-deftest f-write-test/with-content ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt" "FOO"))
   (should-exist "foo.txt" "FOO")))

(ert-deftest f-write-test/override ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt" "FOO")
    (f-write "foo.txt" "BAR"))
   (should-exist "foo.txt" "BAR")))

(ert-deftest f-mkdir-test/single-level ()
  (with-sandbox
   (with-no-messages
    (f-mkdir "foo"))
   (should-exist "foo")))

(ert-deftest f-mkdir-test/multiple-levels ()
  (with-sandbox
   (with-no-messages
    (f-mkdir "foo" "bar" "baz"))
   (should-exist "foo/bar/baz")))

(ert-deftest f-delete-test/file-in-directory ()
  (with-sandbox
   (with-no-messages
    (f-mkdir "foo")
    (f-write "foo/bar.txt")
    (should-exist "foo/bar.txt")
    (f-delete "foo/bar.txt")
    (should-not-exist "foo/bar.txt"))))

(ert-deftest f-delete-test/directory ()
  (with-sandbox
   (with-no-messages
    (f-mkdir "foo")
    (should-exist "foo")
    (f-delete "foo")
    (should-not-exist "foo"))))

(ert-deftest f-delete-test/directory-with-content ()
  (with-sandbox
   (with-no-messages
    (f-mkdir "foo")
    (f-write "foo/bar.txt")
    (should-exist "foo/bar.txt")
    (f-delete "foo/bar.txt" t)
    (should-not-exist "foo/bar.txt"))))

(ert-deftest f-symlink-test/make-link-to-file ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt")
    (f-symlink "foo.txt" "foo.link")
    (should (f-symlink? "foo.link")))))

(ert-deftest f-chmod-test/set-permissions ()
  (with-sandbox
   (with-no-messages
    (f-write "foo.txt")
    (f-chmod "foo.txt" #o123)
    (should (equal (format "%o" (file-modes "foo.txt")) "123")))))
