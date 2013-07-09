(require 'cl)
(require 'el-mock)

(ert-deftest f-paths-test/join-single-path ()
  (should (equal (f-join "path") "path")))

(ert-deftest f-paths-test/join-multiple-paths ()
  (should (equal (f-join "path" "to" "file") "path/to/file")))

(ert-deftest f-paths-test/expand-no-dirs ()
  (let ((default-directory "/default/directory"))
    (should (equal (f-expand "foo") "/default/directory/foo"))))

(ert-deftest f-paths-test/expand-single-dir ()
  (let ((default-directory "/default/directory"))
    (should (equal (f-expand "foo" "/other") "/other/foo"))))

(ert-deftest f-paths-test/expand-multiple-dirs-absolute ()
  (let ((default-directory "/default/directory"))
    (should (equal (f-expand "foo" "/other" "directory") "/other/directory/foo"))))

(ert-deftest f-paths-test/expand-multiple-dirs-relative ()
  (let ((default-directory "/default/directory"))
    (should (equal (f-expand "foo" "path" "to") "/default/directory/path/to/foo"))))

(ert-deftest f-paths-test/filename-relative ()
  (should (equal (f-filename "path/to/file") "file")))

(ert-deftest f-paths-test/filename-absolute ()
  (should (equal (f-filename "/path/to/file") "file")))

(ert-deftest f-paths-test/filename-with-extension ()
  (should (equal (f-filename "/path/to/file.txt") "file.txt")))

(ert-deftest f-paths-test/dirname-relative ()
  (should (equal (f-dirname "path/to/directory") "path/to/")))

(ert-deftest f-paths-test/dirname-absolute ()
  (should (equal (f-dirname "/path/to/directory") "/path/to/")))

(ert-deftest f-paths-test/dirname-with-extension ()
  (should (equal (f-dirname "/path/to/file.txt") "/path/to/")))

(ert-deftest f-paths-test/ext-no-extension ()
  (should (equal (f-ext "path/to/file") nil)))

(ert-deftest f-paths-test/ext-single-extension ()
  (should (equal (f-ext "path/to/file.txt") "txt")))

(ert-deftest f-paths-test/ext-multiple-extensions ()
  (should (equal (f-ext "path/to/file.txt.org") "org")))

(ert-deftest f-paths-test/no-ext-no-extension ()
  (should (equal (f-no-ext "path/to/file") "path/to/file")))

(ert-deftest f-paths-test/no-ext-single-extension ()
  (should (equal (f-no-ext "path/to/file.txt") "path/to/file")))

(ert-deftest f-paths-test/no-ext-multiple-extensions ()
  (should (equal (f-no-ext "path/to/file.txt.org") "path/to/file.txt")))

(ert-deftest f-paths-test/base-no-extension ()
  (should (equal (f-base "path/to/file") "file")))

(ert-deftest f-paths-test/base-single-extension ()
  (should (equal (f-base "path/to/file.txt") "file")))

(ert-deftest f-paths-test/base-multiple-extensions ()
  (should (equal (f-base "path/to/file.txt.org") "file.txt")))

(ert-deftest f-paths-test/glob-without-path ()
  (with-mock
   (mock (file-expand-wildcards "/default/directory/*.el") :times 1)
   (let ((default-directory "/default/directory"))
     (f-glob "*.el"))))

(ert-deftest f-paths-test/glob-with-path ()
  (with-mock
   (mock (file-expand-wildcards "path/to/*.el") :times 1)
   (f-glob "*.el" "path/to")))
