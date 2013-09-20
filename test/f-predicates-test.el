;;;; f-exists?

(ert-deftest f-exists?-test/directory-does-exist ()
  (with-sandbox
   (f-mkdir "foo")
   (should (f-exists? "foo"))))

(ert-deftest f-exists?-test/file-does-exist ()
  (with-sandbox
   (f-touch "foo.txt")
   (should (f-exists? "foo.txt"))))

(ert-deftest f-exists?-test/does-not-exists ()
  (with-sandbox
   (should-not (f-exists? "foo.txt"))))


;;;; f-directory?/f-dir?

(ert-deftest f-directory?-test/is-directory ()
  (with-sandbox
   (f-mkdir "foo")
   (should (f-directory? "foo"))))

(ert-deftest f-directory?-test/is-file ()
  (with-sandbox
   (f-touch "foo.txt")
   (should-not (f-directory? "foo.txt"))))

(ert-deftest f-dir?-test/alias ()
  (with-sandbox
   (f-mkdir "foo")
   (should (f-dir? "foo"))))


;;;; f-file?

(ert-deftest f-file?-test/is-file ()
  (with-sandbox
   (f-touch "foo.txt")
   (should (f-file? "foo.txt"))))

(ert-deftest f-file?-test/is-directory ()
  (with-sandbox
   (f-mkdir "foo")
   (should-not (f-file? "foo"))))


;;;; f-symlink?

(ert-deftest f-symlink?-test/is-symlink ()
  (with-sandbox
   (f-touch "foo.txt")
   (f-symlink "foo.txt" "foo.link")
   (should (f-symlink? "foo.link"))))

(ert-deftest f-symlink?-test/is-not-symlink ()
  (with-sandbox
   (f-touch "foo.txt")
   (should-not (f-symlink? "foo.txt"))))


;;;; f-readable?

(ert-deftest f-readable?-test/is-readable ()
  (with-sandbox
   (f-touch "foo.txt")
   (chmod "foo.txt" "400")
   (should (f-readable? "foo.txt"))))

(ert-deftest f-readable?-test/is-not-readable ()
  (with-sandbox
   (f-touch "foo.txt")
   (chmod "foo.txt" "000")
   (should-not (f-readable? "foo.txt"))))


;;;; f-writable?

(ert-deftest f-writeable?-test/is-readable ()
  (with-sandbox
   (f-touch "foo.txt")
   (chmod "foo.txt" "700")
   (should (f-writable? "foo.txt"))))

(ert-deftest f-writeable?-test/is-not-readable ()
  (with-sandbox
   (f-touch "foo.txt")
   (chmod "foo.txt" "400")
   (should-not (f-writable? "foo.txt"))))

(ert-deftest f-writeable?-test/is-readable ()
  (with-sandbox
   (f-touch "foo.txt")
   (chmod "foo.txt" "100")
   (should (f-executable? "foo.txt"))))

(ert-deftest f-writeable?-test/is-not-readable ()
  (with-sandbox
   (f-touch "foo.txt")
   (chmod "foo.txt" "200")
   (should-not (f-executable? "foo.txt"))))


;;;; f-absolute?

(ert-deftest f-absolute?-test/is-absolute ()
  (should (f-absolute? "/full/path/to/dir")))

(ert-deftest f-absolute?-test/is-relative ()
  (should-not (f-absolute? "path/to/dir")))


;;;; f-relative?

(ert-deftest f-relative?-test/is-relative ()
  (should (f-relative? "path/to/dir")))

(ert-deftest f-relative?-test/is-absolute ()
  (should-not (f-relative? "/full/path/to/dir")))


;;;; f-root?

(ert-deftest f-root?-test/is-root ()
  (should (f-root? "/")))

(ert-deftest f-root?-test/is-not-root ()
  (should-not (f-root? "/not/root")))

(ert-deftest f-root?-test/is-root-weird-syntax ()
  (should (f-root? "/bin/..")))


;;;; f-ext?

(ert-deftest f-ext?-test/ext-does-match ()
  (with-sandbox
   (f-touch "foo.el")
   (should (f-ext? "foo.el" "el"))))

(ert-deftest f-ext?-test/ext-does-not-match ()
  (with-sandbox
   (f-touch "foo.el")
   (should-not (f-ext? "foo.el" "txt"))))

(ert-deftest f-ext?-test/with-ext ()
  (with-sandbox
   (f-touch "foo.el")
   (should (f-ext? "foo.el"))))

(ert-deftest f-ext?-test/without-ext ()
  (with-sandbox
   (f-touch "foo")
   (should-not (f-ext? "foo"))))


;;;; f-same?/f-equal?

(ert-deftest f-same?/relative-equal ()
  (with-sandbox
   (should (f-same? "foo" "foo"))))

(ert-deftest f-same?/relative-not-equal ()
  (with-sandbox
   (should-not (f-same? "foo" "bar"))))

(ert-deftest f-same?/absolute-equal ()
  (with-sandbox
   (should (f-same? (f-expand "foo" f-sandbox-path)
                    (f-expand "foo" f-sandbox-path)))))

(ert-deftest f-same?/absolute-not-equal ()
  (with-sandbox
   (should-not (f-same? (f-expand "foo" f-sandbox-path)
                        (f-expand "bar" f-sandbox-path)))))

(ert-deftest f-same?/relative-and-absolute-equal ()
  (with-sandbox
   (should (f-same? "foo" (f-expand "foo" f-sandbox-path)))))

(ert-deftest f-same?/relative-and-absolute-not-equal ()
  (with-sandbox
   (should-not (f-same? "foo" (f-expand "bar" f-sandbox-path)))))

(ert-deftest f-same?/symlink ()
  (with-sandbox
   (f-touch "foo")
   (f-symlink "foo" "bar")
   (f-same? "foo" "bar")))

(ert-deftest f-equal?/alias ()
  (with-sandbox
   (should (f-equal? "foo" "foo"))))


;;;; f-parent-of?

(ert-deftest f-parent-of?-test/is-parent ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz" "qux")
   (should (equal t (f-parent-of? "foo" "foo/bar")))
   (should (equal t (f-parent-of? "foo/bar" "foo/bar/baz")))
   (should (equal t (f-parent-of? "foo/bar/baz" "foo/bar/baz/qux")))))

(ert-deftest f-parent-of?-test/is-not-parent ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-parent-of? "foo/bar" "foo"))
   (should-not (f-parent-of? "foo/bar/baz" "foo/bar"))
   (should-not (f-parent-of? "foo/bar/baz/qux" "foo/bar/baz"))))

(ert-deftest f-parent-of?-test/is-same ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-parent-of? "foo" "foo"))
   (should-not (f-parent-of? "foo/bar" "foo/bar"))
   (should-not (f-parent-of? "foo/bar/baz" "foo/bar/baz"))
   (should-not (f-parent-of? "foo/bar/baz/qux" "foo/bar/baz/qux"))))


;;;; f-child-of?

(ert-deftest f-child-of?-test/is-child ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz" "qux")
   (should (equal t (f-child-of? "foo/bar" "foo")))
   (should (equal t (f-child-of? "foo/bar/baz" "foo/bar")))
   (should (equal t (f-child-of? "foo/bar/baz/qux" "foo/bar/baz")))))

(ert-deftest f-child-of?-test/is-not-child ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-child-of? "foo" "foo/bar"))
   (should-not (f-child-of? "foo/bar" "foo/bar/baz"))
   (should-not (f-child-of? "foo/bar/baz" "foo/bar/baz/qux"))))

(ert-deftest f-child-of?-test/is-same ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-child-of? "foo" "foo"))
   (should-not (f-child-of? "foo/bar" "foo/bar"))
   (should-not (f-child-of? "foo/bar/baz" "foo/bar/baz"))
   (should-not (f-child-of? "foo/bar/baz/qux" "foo/bar/baz/qux"))))

