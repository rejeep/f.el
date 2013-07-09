(ert-deftest f-paths-test/join-single-path ()
  (should (equal (f-join "path") "path")))

(ert-deftest f-paths-test/join-multiple-paths ()
  (should (equal (f-join "path" "to" "file") "path/to/file")))
