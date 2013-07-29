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

(ert-deftest f-mkdir-test/single-level ()
  (with-sandbox
   (f-mkdir "foo")
   (should-exist "foo")))

(ert-deftest f-mkdir-test/multiple-levels-same-call ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should-exist "foo/bar/baz")))

(ert-deftest f-mkdir-test/multiple-levels-different-calls ()
  (with-sandbox
   (f-mkdir "foo")
   (f-mkdir "foo" "bar")
   (f-mkdir "foo" "bar" "baz")
   (should-exist "foo/bar/baz")))

(ert-deftest f-delete-test/file-in-directory ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.txt")
   (should-exist "foo/bar.txt")
   (f-delete "foo/bar.txt")
   (should-not-exist "foo/bar.txt")))

(ert-deftest f-delete-test/directory ()
  (with-sandbox
   (f-mkdir "foo")
   (should-exist "foo")
   (f-delete "foo")
   (should-not-exist "foo")))

(ert-deftest f-delete-test/directory-with-content ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.txt")
   (should-exist "foo/bar.txt")
   (f-delete "foo/bar.txt" t)
   (should-not-exist "foo/bar.txt")))

(ert-deftest f-symlink-test/make-link-to-file ()
  (with-sandbox
   (f-write "foo.txt")
   (f-symlink "foo.txt" "foo.link")
   (should (f-symlink? "foo.link"))))

(ert-deftest f-move-test/move-relative-path ()
  (with-sandbox
   (f-write "foo.txt")
   (f-mkdir "bar")
   (f-move "foo.txt" "bar")
   (should-exist "bar/foo.txt")))

(ert-deftest f-move-test/move-absolute-path ()
  (with-sandbox
   (f-write "foo.txt")
   (f-mkdir "bar")
   (f-move
    (f-expand "foo.txt" f-sandbox-path)
    (f-expand "bar" f-sandbox-path))
   (should-exist "bar/foo.txt")))

(ert-deftest f-move-test/rename-relative-path ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (f-move "foo.txt" "bar.txt")
   (should-exist "bar.txt" "FOO")))

(ert-deftest f-move-test/rename-absolute-path ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (f-move
    (f-expand "foo.txt" f-sandbox-path)
    (f-expand "bar.txt" f-sandbox-path))
   (should-exist "bar.txt" "FOO")))

(ert-deftest f-copy-test/copy-relative-file ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (f-copy "foo.txt" "bar.txt")
   (should-exist "foo.txt" "FOO")
   (should-exist "bar.txt" "FOO")))

(ert-deftest f-copy-test/copy-absolute-file ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (f-copy
    (f-expand "foo.txt" f-sandbox-path)
    (f-expand "bar.txt" f-sandbox-path))
   (should-exist "foo.txt" "FOO")
   (should-exist "bar.txt" "FOO")))

(ert-deftest f-copy-test/copy-relative-dir ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/file.txt" "FILE")
   (f-copy "foo" "bar")
   (should-exist "foo/file.txt" "FILE")))

(ert-deftest f-copy-test/copy-absolute-dir ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/file.txt" "FILE")
   (f-copy
    (f-expand "foo" f-sandbox-path)
    (f-expand "bar" f-sandbox-path))
   (should-exist "foo/file.txt" "FILE")))
