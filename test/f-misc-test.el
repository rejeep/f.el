(ert-deftest f-glob-test/without-path ()
  (with-mock
   (mock (file-expand-wildcards "/default/directory/*.el") :times 1)
   (with-default-directory
    (f-glob "*.el"))))

(ert-deftest f-glob-test/with-path ()
  (with-mock
   (mock (file-expand-wildcards "path/to/*.el") :times 1)
   (f-glob "*.el" "path/to")))
