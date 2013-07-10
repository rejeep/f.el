(ert-deftest f-join-test/single-path ()
  (should (equal (f-join "path") "path")))

(ert-deftest f-join-test/multiple-paths ()
  (should (equal (f-join "path" "to" "file") "path/to/file")))

(ert-deftest f-expand-test/no-dirs ()
  (with-default-directory
   (should (equal (f-expand "foo") "/default/directory/foo"))))

(ert-deftest f-expand-test/single-dir ()
  (with-default-directory
   (should (equal (f-expand "foo" "/other") "/other/foo"))))

(ert-deftest f-expand-test/multiple-dirs-absolute ()
  (with-default-directory
   (should (equal (f-expand "foo" "/other" "directory") "/other/directory/foo"))))

(ert-deftest f-expand-test/multiple-dirs-relative ()
  (with-default-directory
   (should (equal (f-expand "foo" "path" "to") "/default/directory/path/to/foo"))))

(ert-deftest f-filename-test/relative ()
  (should (equal (f-filename "path/to/file") "file")))

(ert-deftest f-filename-test/absolute ()
  (should (equal (f-filename "/path/to/file") "file")))

(ert-deftest f-filename-test/with-extension ()
  (should (equal (f-filename "/path/to/file.txt") "file.txt")))

(ert-deftest f-dirname-test/relative ()
  (should (equal (f-dirname "path/to/directory") "path/to/")))

(ert-deftest f-dirname-test/absolute ()
  (should (equal (f-dirname "/path/to/directory") "/path/to/")))

(ert-deftest f-dirname-test/with-extension ()
  (should (equal (f-dirname "/path/to/file.txt") "/path/to/")))

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

(ert-deftest f-parent-test/directory ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should (equal (f-relative (f-parent "foo/bar/baz") f-sandbox-path) "foo/bar/"))))

(ert-deftest f-parent-test/file ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (f-write "foo/bar/baz/qux.txt")
   (should (equal (f-relative (f-parent "foo/bar/baz/qux.txt") f-sandbox-path) "foo/bar/baz/"))))
