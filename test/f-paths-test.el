(require 'cl)
(require 'el-mock)

(defmacro with-default-directory (&rest body)
  `(let ((default-directory "/default/directory")) ,@body))

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

(ert-deftest f-glob-test/without-path ()
  (with-mock
   (mock (file-expand-wildcards "/default/directory/*.el") :times 1)
   (with-default-directory
    (f-glob "*.el"))))

(ert-deftest f-glob-test/with-path ()
  (with-mock
   (mock (file-expand-wildcards "path/to/*.el") :times 1)
   (f-glob "*.el" "path/to")))

(ert-deftest f-relative-test/with-path ()
  (should (equal (f-relative "/some/path/relative/to/my/file.txt" "/some/path/") "relative/to/my/file.txt")))

(ert-deftest f-relative-test/without-path ()
  (with-default-directory
   (should (equal (f-relative "/default/directory/my/file.txt") "my/file.txt"))))
