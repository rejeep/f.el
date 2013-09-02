(ert-deftest f-join-test/single-path-relative ()
  (should (equal (f-join "path") "path")))

(ert-deftest f-join-test/single-path-absolute ()
  (should (equal (f-join "/path") "/path")))

(ert-deftest f-join-test/multiple-paths-relative ()
  (should (equal (f-join "path" "to" "file") "path/to/file")))

(ert-deftest f-join-test/multiple-paths-absolute ()
  (should (equal (f-join "/path" "to" "file") "/path/to/file")))

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

(ert-deftest f-filename-test/with-ending-slash ()
  (should (equal (f-filename "/path/to/dir/") "dir")))

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

(ert-deftest f-dirname-test/file-with-ending-slash ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should
    (equal
     (f-dirname "foo/bar/baz/") "foo/bar/"))))

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

(ert-deftest f-short-test/home ()
  (let ((home (getenv "HOME")))
    (should (equal (f-short (f-expand "Code/bar" home)) "~/Code/bar"))))

(ert-deftest f-short-test/other ()
  (should (equal (f-short "/path/to/Code/bar") "/path/to/Code/bar")))

(ert-deftest f-short-test/alias ()
  (let ((home (getenv "HOME")))
    (should (equal (f-abbrev (f-expand "Code/bar" home)) "~/Code/bar")))
  (should (equal (f-abbrev "/path/to/Code/bar") "/path/to/Code/bar")))

(ert-deftest f-long-test/home ()
  (let ((home (getenv "HOME")))
    (should (equal (f-long "~/Code/bar") (f-expand "Code/bar" home)))))

(ert-deftest f-long-test/other ()
  (should (equal (f-long "/path/to/Code/bar") "/path/to/Code/bar")))

(ert-deftest f-canonical-test/path ()
  (should (equal (f-canonical f-sandbox-path) f-sandbox-path)))

(ert-deftest f-canonical-test/symlink ()
  (with-sandbox
   (f-write "foo")
   (f-symlink "foo" "bar")
   (should
    (equal
     (f-expand "foo" f-sandbox-path)
     (f-canonical (f-expand "bar" f-sandbox-path))))))

(ert-deftest f-slash-test/absolute-no-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-slash foo) (concat foo "/"))))))

(ert-deftest f-slash-test/absolute-with-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-slash (concat foo "/")) (concat foo "/"))))))

(ert-deftest f-slash-test/relative-no-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-slash "foo") "foo/"))))

(ert-deftest f-slash-test/relative-with-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-slash "foo/") "foo/"))))

(ert-deftest f-slash-test/relative-file ()
  (with-sandbox
   (f-touch "foo")
   (should (equal (f-slash "foo") "foo"))))

(ert-deftest f-slash-test/absolute-file ()
  (with-sandbox
   (f-touch "foo")
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-slash foo) foo)))))

(ert-deftest f-slash-test/different-path-separator ()
  (with-mock
   (stub f-path-separator => "\\")
   (should (equal "path\\to\\file\\" (f-slash "path\\to\\file")))))

(ert-deftest f-full-test/relative-no-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-full "foo") (concat (f-expand "foo" f-sandbox-path) "/")))))

(ert-deftest f-full-test/relative-with-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-full "foo/") (concat (f-expand "foo" f-sandbox-path) "/")))))

(ert-deftest f-full-test/relative-tilde ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-full (f-short (f-expand "foo" f-sandbox-path)))
                  (concat (f-expand "foo" f-sandbox-path) "/")))))

(ert-deftest f-full-test/absolute-no-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-full foo) (concat foo "/"))))))

(ert-deftest f-full-test/absolute-with-slash ()
  (with-sandbox
   (f-mkdir "foo")
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-full (concat foo "/")) (concat foo "/"))))))

(ert-deftest f-full-test/absolute-tilde ()
  (with-sandbox
   (f-mkdir "foo")
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-full (f-short foo)) (concat foo "/"))))))

(ert-deftest f-full-test/file ()
  (with-sandbox
   (f-touch "foo")
   (should (equal (f-full "foo") (f-expand "foo" f-sandbox-path)))))

(ert-deftest f-full-test/file-tilde ()
  (with-sandbox
   (f-touch "foo")
   (should (equal (f-full (f-short "foo")) (f-expand "foo" f-sandbox-path)))))
