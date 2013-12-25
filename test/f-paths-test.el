;;;; f-join

(ert-deftest f-join-test/single-path-relative ()
  (should (equal (f-join "path") "path")))

(ert-deftest f-join-test/single-path-absolute ()
  (should (equal (f-join "/path") "/path")))

(ert-deftest f-join-test/multiple-paths-relative ()
  (should (equal (f-join "path" "to" "file") "path/to/file")))

(ert-deftest f-join-test/multiple-paths-absolute ()
  (should (equal (f-join "/path" "to" "file") "/path/to/file")))

(ert-deftest f-join-test/multiple-paths-absolute-2 ()
  (should (equal (f-join "/" "path" "to" "file") "/path/to/file")))


;;;; f-split

(ert-deftest f-split-test/single-path-relative ()
  (should (equal (f-split "path") '("path"))))

(ert-deftest f-split-test/single-path-absolute ()
  (should (equal (f-split "/path") '("/" "path"))))

(ert-deftest f-split-test/multiple-paths-relative ()
  (should (equal (f-split "path/to/file") '("path" "to" "file"))))

(ert-deftest f-split-test/multiple-paths-absolute ()
  (should (equal (f-split "/path/to/file") '("/" "path" "to" "file"))))

(ert-deftest f-split-test/inverse-of-join ()
  (should (equal (f-split (apply 'f-join (f-split "/path/to/file")))
                 '("/" "path" "to" "file"))))


;;;; f-expand

(ert-deftest f-expand-test/no-dir ()
  (with-default-directory
   (should (equal (f-expand "foo") "/default/directory/foo"))))

(ert-deftest f-expand-test/with-dir ()
  (with-default-directory
   (should (equal (f-expand "foo" "/other") "/other/foo"))))


;;;; f-filename

(ert-deftest f-filename-test/relative ()
  (should (equal (f-filename "path/to/file") "file")))

(ert-deftest f-filename-test/absolute ()
  (should (equal (f-filename "/path/to/file") "file")))

(ert-deftest f-filename-test/with-extension ()
  (should (equal (f-filename "/path/to/file.txt") "file.txt")))

(ert-deftest f-filename-test/with-ending-slash ()
  (should (equal (f-filename "/path/to/dir/") "dir")))


;;;; f-dirname

(ert-deftest f-dirname-test/directory-relative ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (should (equal (f-dirname "foo/bar/baz") "foo/bar/"))))

(ert-deftest f-dirname-test/file-relative ()
  (with-sandbox
   (f-mkdir "foo" "bar" "baz")
   (f-touch "foo/bar/baz/qux.txt")
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
   (f-touch "foo/bar/baz/qux.txt")
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

(ert-deftest f-dirname-test/root ()
  (with-sandbox
   (should-not (f-parent (f-root)))))


;;;; f-ext

(ert-deftest f-ext-test/no-extension ()
  (should (equal (f-ext "path/to/file") nil)))

(ert-deftest f-ext-test/single-extension ()
  (should (equal (f-ext "path/to/file.txt") "txt")))

(ert-deftest f-ext-test/multiple-extensions ()
  (should (equal (f-ext "path/to/file.txt.org") "org")))


;;;; f-no-ext

(ert-deftest f-no-ext-test/no-extension ()
  (should (equal (f-no-ext "path/to/file") "path/to/file")))

(ert-deftest f-no-ext-test/single-extension ()
  (should (equal (f-no-ext "path/to/file.txt") "path/to/file")))

(ert-deftest f-no-ext-test/multiple-extensions ()
  (should (equal (f-no-ext "path/to/file.txt.org") "path/to/file.txt")))


;;;; f-base

(ert-deftest f-base-test/no-extension ()
  (should (equal (f-base "path/to/file") "file")))

(ert-deftest f-base-test/single-extension ()
  (should (equal (f-base "path/to/file.txt") "file")))

(ert-deftest f-base-test/multiple-extensions ()
  (should (equal (f-base "path/to/file.txt.org") "file.txt")))


;;;; f-relative

(ert-deftest f-relative-test/with-path ()
  (should (equal (f-relative "/some/path/relative/to/my/file.txt" "/some/path/") "relative/to/my/file.txt")))

(ert-deftest f-relative-test/without-path ()
  (with-default-directory
   (should (equal (f-relative "/default/directory/my/file.txt") "my/file.txt"))))


;;;; f-short

(ert-deftest f-short-test/home ()
  (let ((home (getenv "HOME")))
    (should (equal (f-short (f-expand "Code/bar" home)) "~/Code/bar"))))

(ert-deftest f-short-test/other ()
  (should (equal (f-short "/path/to/Code/bar") "/path/to/Code/bar")))

(ert-deftest f-short-test/alias ()
  (let ((home (getenv "HOME")))
    (should (equal (f-abbrev (f-expand "Code/bar" home)) "~/Code/bar")))
  (should (equal (f-abbrev "/path/to/Code/bar") "/path/to/Code/bar")))


;;;; f-long

(ert-deftest f-long-test/home ()
  (let ((home (getenv "HOME")))
    (should (equal (f-long "~/Code/bar") (f-expand "Code/bar" home)))))

(ert-deftest f-long-test/other ()
  (should (equal (f-long "/path/to/Code/bar") "/path/to/Code/bar")))


;;;; f-cannonical

(ert-deftest f-canonical-test/path ()
  (should (equal (f-canonical f-sandbox-path) f-sandbox-path)))

(ert-deftest f-canonical-test/symlink ()
  (with-sandbox
   (f-touch "foo")
   (f-symlink "foo" "bar")
   (should
    (equal
     (f-expand "foo" f-sandbox-path)
     (f-canonical (f-expand "bar" f-sandbox-path))))))


;;;; f-slash

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

(ert-deftest f-slash-test/symlink-to-file ()
  (with-sandbox
   (f-touch "foo")
   (f-symlink "foo" "bar")
   (let ((bar (f-expand "bar" f-sandbox-path)))
     (should (equal (f-slash bar) bar)))))

(ert-deftest f-slash-test/symlink-to-directory ()
  (with-sandbox
   (f-mkdir "foo")
   (f-symlink "foo" "bar")
   (let ((bar (f-expand "bar" f-sandbox-path)))
     (should (equal (f-slash bar) (concat bar "/"))))))

(ert-deftest f-slash-test/non-existing-file-or-directory ()
  (with-sandbox
   (let ((foo (f-expand "foo" f-sandbox-path)))
     (should (equal (f-slash foo) foo)))))


;;;; f-full

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

;;;; f-uniquify

(ert-deftest f-uniquify/no-conflict ()
  (should (equal (f-uniquify '("/foo/bar" "/foo/baz" "/foo/quux")) '("bar" "baz" "quux"))))

(ert-deftest f-uniquify/single-conflict ()
  (should (equal (f-uniquify '("/foo/bar" "/www/bar" "/foo/quux")) '("foo/bar" "www/bar" "quux"))))

(ert-deftest f-uniquify/single-conflict-shared-subpath ()
  (should (equal (f-uniquify '("/foo/bar" "/www/bar" "/www/bar/quux")) '("foo/bar" "www/bar" "quux"))))

(ert-deftest f-uniquify/recursive-conflict ()
  (should (equal (f-uniquify '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz"))
                 '("foo/bar" "www/bar" "foo/baz" "home/www/baz" "foo/www/baz" "foo"))))

;;;; f-uniquify-alist

(ert-deftest f-uniquify/no-conflict ()
  (should (equal (f-uniquify-alist '("/foo/bar" "/foo/baz" "/foo/quux")) '(("/foo/bar" . "bar") ("/foo/baz" . "baz") ("/foo/quux" . "quux")) )))

(ert-deftest f-uniquify-alist/single-conflict ()
  (should (equal (f-uniquify-alist '("/foo/bar" "/www/bar" "/foo/quux")) '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/foo/quux" . "quux")) )))

(ert-deftest f-uniquify-alist/single-conflict-shared-subpath ()
  (should (equal (f-uniquify-alist '("/foo/bar" "/www/bar" "/www/bar/quux")) '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/www/bar/quux" . "quux")))))

(ert-deftest f-uniquify-alist/recursive-conflict ()
  (should (equal (f-uniquify-alist '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz"))
                 '(("/foo/bar" . "foo/bar") ("/home/www/bar" . "www/bar") ("/foo/baz" . "foo/baz") ("/home/www/baz" . "home/www/baz") ("/opt/foo/www/baz" . "foo/www/baz") ("/var/foo" . "foo")) )))
