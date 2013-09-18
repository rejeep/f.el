(ert-deftest f-glob-test/without-path ()
  (with-sandbox
   (f-write "foo.el")
   (f-write "baz.el")
   (f-mkdir "bar")
   (f-write "bar/qux.el")
   (should
    (equal
     (mapcar 'f-filename (f-glob "*.el")) '("baz.el" "foo.el")))))

(ert-deftest f-glob-test/with-path ()
  (with-sandbox
   (f-write "foo.el")
   (f-write "baz.el")
   (f-mkdir "bar")
   (f-write "bar/qux.el")
   (should
    (equal
     (mapcar 'f-filename (f-glob "*.el" "bar")) '("qux.el")))))

(ert-deftest f-entries-test/no-directories-or-files ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-entries "foo") nil))))

(ert-deftest f-entries-test/with-files-and-directories ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.txt")
   (f-write "foo/baz.txt")
   (f-mkdir "foo/qux")
   (should
    (equal
     (--map (f-relative it "foo") (f-entries "foo" nil t))
     '("qux" "baz.txt" "bar.txt")))))

(ert-deftest f-entries-test/with-callback-function ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.el")
   (f-write "foo/baz.el")
   (f-write "foo/qux.coffee")
   (let ((fn
          (lambda (entry)
            (equal (f-ext entry) "el"))))
     (should
      (equal
       (--map (f-relative it "foo") (f-entries "foo" fn t))
       '("baz.el" "bar.el"))))))

(ert-deftest f-entries-test/recursive ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.el")
   (f-mkdir "foo/bar")
   (f-write "foo/bar/baz.el")
   (f-mkdir "foo/bar/qux")
   (f-write "foo/bar/qux/hey.el")
   (should
    (equal
     (--map (f-relative it "foo") (f-entries "foo" nil t))
     '("bar.el" "bar" "bar/qux" "bar/baz.el" "bar/qux/hey.el")))))

(ert-deftest f-entries-test/anaphoric ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.el")
   (f-mkdir "foo/bar")
   (f-write "foo/baz.el")
   (f-write "foo/qux.coffee")
   (f-mkdir "foo/qux")
   (let* ((foo-path (f-expand "foo" f-sandbox-path))
          (el-files (-sort 'string< (--map (f-expand it foo-path) '("baz.el" "bar.el"))))
          (all-files (-sort 'string< (--map (f-expand it foo-path) '("baz.el" "bar.el" "qux.coffee" "bar" "qux")))))
     (should (equal (-sort 'string< (f--entries "foo" 'ignore)) all-files))
     (should (equal (-sort 'string< (f--entries "foo" (equal (f-ext it) "el") t)) el-files)))))

(ert-deftest f-directories-test/no-directories-or-files ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-directories "foo") nil))))

(ert-deftest f-directories-test/with-files-and-directories ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.txt")
   (f-write "foo/baz.txt")
   (f-mkdir "foo/qux")
   (should
    (equal
     (--map (f-relative it "foo") (f-directories "foo" nil t))
     '("qux")))))

(ert-deftest f-directories-test/with-callback-function ()
  (with-sandbox
   (f-mkdir "foo")
   (f-mkdir "foo/test")
   (f-mkdir "foo/baz")
   (f-mkdir "foo/baz/test")
   (f-write "foo/test/baz.el")
   (f-write "foo/baz/test/qux.el")
   (let ((fn
          (lambda (entry)
            (equal (f-filename entry) "test"))))
     (should
      (equal
       (--map (f-relative it "foo") (f-directories "foo" fn t))
       '("test" "baz/test"))))))

(ert-deftest f-directories-test/recursive ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.el")
   (f-mkdir "foo/bar")
   (f-write "foo/bar/baz.el")
   (f-mkdir "foo/bar/qux")
   (f-write "foo/bar/qux/hey.el")
   (should
    (equal
     (--map (f-relative it "foo") (f-directories "foo" nil t))
     '("bar" "bar/qux")))))

(ert-deftest f-directories-test/anaphoric ()
  (with-sandbox
   (f-mkdir "foo")
   (f-mkdir "foo/test")
   (f-mkdir "foo/baz")
   (f-mkdir "foo/baz/test")
   (f-write "foo/test/baz.el")
   (f-write "foo/baz/test/qux.el")
   (let* ((foo-path (f-expand "foo" f-sandbox-path))
          (test-dirs (-sort 'string< (--map (f-expand it foo-path) '("test" "baz/test"))))
          (all-dirs (-sort 'string< (--map (f-expand it foo-path) '("test" "baz" "baz/test")))))
     (should (equal (-sort 'string< (f--directories "foo" 'ignore :recursive)) all-dirs))
     (should (equal (-sort 'string< (f--directories "foo" (equal (f-filename it) "test") t)) test-dirs)))))

(ert-deftest f-files-test/no-files-or-files ()
  (with-sandbox
   (f-mkdir "foo")
   (should (equal (f-files "foo") nil))))

(ert-deftest f-files-test/with-files-and-files ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.txt")
   (f-write "foo/baz.txt")
   (f-mkdir "foo/qux")
   (should
    (equal
     (--map (f-relative it "foo") (f-files "foo" nil t))
     '("baz.txt" "bar.txt")))))

(ert-deftest f-files-test/with-callback-function ()
  (with-sandbox
   (f-mkdir "foo")
   (f-mkdir "foo/test")
   (f-mkdir "foo/baz")
   (f-mkdir "foo/baz/test")
   (f-write "foo/test/baz.el")
   (f-write "foo/baz/test/qux.el")
   (let ((fn
          (lambda (entry)
            (equal (f-ext entry) "el"))))
     (should
      (equal
       (--map (f-relative it "foo") (f-files "foo" fn t))
       '("baz/test/qux.el" "test/baz.el"))))))

(ert-deftest f-files-test/recursive ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.el")
   (f-mkdir "foo/bar")
   (f-write "foo/bar/baz.el")
   (f-mkdir "foo/bar/qux")
   (f-write "foo/bar/qux/hey.el")
   (should
    (equal
     (--map (f-relative it "foo") (f-files "foo" nil t))
     '("bar.el" "bar/baz.el" "bar/qux/hey.el")))))

(ert-deftest f-files-test/anaphoric ()
  (with-sandbox
   (f-mkdir "foo")
   (f-write "foo/bar.el")
   (f-write "foo/baz.el")
   (f-write "foo/qux.coffee")
   (let* ((foo-path (f-expand "foo" f-sandbox-path))
          (el-files (-sort 'string< (--map (f-expand it foo-path) '("baz.el" "bar.el"))))
          (all-files (-sort 'string< (--map (f-expand it foo-path) '("baz.el" "bar.el" "qux.coffee")))))
     (should (equal (-sort 'string< (f--entries "foo" 'ignore)) all-files))
     (should (equal (-sort 'string< (f--entries "foo" (equal (f-ext it) "el") t)) el-files)))))

(ert-deftest f-path-separator-test ()
  ;; was previously based on `default-directory', make sure we don't
  ;; need it.
  (let (default-directory)
    (should (equal (f-path-separator) "/"))))

(ert-deftest f-root-test ()
  (should (equal (f-root) "/")))

(ert-deftest f-up-test/false ()
  (with-sandbox
   (should (equal (f-root) (f-up (lambda (path) nil))))))

(ert-deftest f-up-test/true ()
  (with-sandbox
   (should (equal f-sandbox-path (f-up (lambda (path) t))))))

(ert-deftest f-up-test/traverse-up ()
  (with-sandbox
   (f-touch "foo")
   (f-mkdir "bar" "baz")
   (should
    (equal
     f-sandbox-path
     (f-up
      (lambda (path)
        (f-file? (f-expand "foo" path)))
      (f-join "bar" "baz"))))))

(ert-deftest f-up-test/non-existing-directory ()
  (with-sandbox
   (should-error
    (f-up 'ignore "err"))))

(ert-deftest f-up-test/anaphoric ()
  (with-sandbox
   (f-touch "foo")
   (f-mkdir "bar" "baz")
   (should
    (equal
     (f--up (equal (f-filename it) "bar") (f-join "bar" "baz"))
     (f-expand "bar" f-sandbox-path)))))
