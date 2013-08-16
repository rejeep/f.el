(ert-deftest f-size-test/empty-file ()
  (with-sandbox
   (f-write "foo.txt")
   (should (equal (f-size "foo.txt") 0))))

(ert-deftest f-size-test/file-with-content ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (should (equal (f-size "foo.txt") 3))))

(ert-deftest f-size-test/directory ()
  (with-sandbox
   (f-mkdir "bar")
   (f-write "bar/foo.txt" "FOO")
   (f-write "bar/baz.txt" "BAZ")
   (should (equal (f-size "bar") 6))))
