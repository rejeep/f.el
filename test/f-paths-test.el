(ert-deftest f-join-test/single-path ()
  (should (equal (f-join "path") "path")))

(ert-deftest f-join-test/multiple-paths ()
  (should (equal (f-join "path" "to" "file") "path/to/file")))

(ert-deftest f-expand-test/no-dir ()
  (with-default-directory
   (should (equal (f-expand "foo") "/default/directory/foo"))))

(ert-deftest f-expand-test/with-dir ()
  (with-default-directory
   (should (equal (f-expand "foo" "/other") "/other/foo"))))

(ert-deftest f-filename-test/relative ()
  (should (equal (f-filename "path/to/file") "file")))

(ert-deftest f-filename-test/absolute ()
  (should (equal (f-filename "/path/to/file") "file")))

(ert-deftest f-filename-test/with-extension ()
  (should (equal (f-filename "/path/to/file.txt") "file.txt")))

(ert-deftest f-dirname-test/directory-relative ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should (equal (f-dirname "foo/bar/baz") "foo/bar/"))))

(ert-deftest f-dirname-test/file-relative ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (f-write "foo/bar/baz/qux.txt")
   (should (equal (f-dirname "foo/bar/baz/qux.txt") "foo/bar/baz/"))))

(ert-deftest f-dirname-test/directory-absolute ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should
    (equal
     (f-dirname (f-expand "foo/bar/baz" f-sandbox-path))
     (f-expand "foo/bar/" f-sandbox-path)))))

(ert-deftest f-dirname-test/file-absolute ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (f-write "foo/bar/baz/qux.txt")
   (should
    (equal
     (f-dirname (f-expand "foo/bar/baz/qux.txt" f-sandbox-path))
     (f-expand "foo/bar/baz/" f-sandbox-path)))))

(ert-deftest f-dirname-test/parent-alias ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should (equal (f-parent "foo/bar/baz") "foo/bar/"))))

(ert-deftest f-ext-test/no-extension ()
  (should (equal (f-ext "path/to/file") nil)))

(ert-deftest f-ext-test/single-extension ()
  (should (equal (f-ext "path/to/file.txt") "txt")))

(ert-deftest f-ext-test/multiple-extensions ()
  (should (equal (f-ext "path/to/file.txt.org") "org")))

(ert-deftest f-no-ext-test/no-extension ()
  (should (equal (f-no-ext "path/to/file") "path/to/file")))

(ert-deftest f-no-ext-test/single-extension ()
  (should (equal (f-no-ext "path/to/file.txt") "path/to/file")))

(ert-deftest f-no-ext-test/multiple-extensions ()
  (should (equal (f-no-ext "path/to/file.txt.org") "path/to/file.txt")))

(ert-deftest f-base-test/no-extension ()
  (should (equal (f-base "path/to/file") "file")))

(ert-deftest f-base-test/single-extension ()
  (should (equal (f-base "path/to/file.txt") "file")))

(ert-deftest f-base-test/multiple-extensions ()
  (should (equal (f-base "path/to/file.txt.org") "file.txt")))

(ert-deftest f-relative-test/with-path ()
  (should (equal (f-relative "/some/path/relative/to/my/file.txt" "/some/path/") "relative/to/my/file.txt")))

(ert-deftest f-relative-test/without-path ()
  (with-default-directory
   (should (equal (f-relative "/default/directory/my/file.txt") "my/file.txt"))))
