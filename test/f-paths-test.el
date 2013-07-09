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
