(ert-deftest f-size-test/empty-file ()
  (with-sandbox
   (f-touch "foo.txt")
   (should (equal (f-size "foo.txt") 0))))

(ert-deftest f-size-test/file-with-content ()
  (with-sandbox
   (f-write "FOO" 'utf-8 "foo.txt")
   (should (equal (f-size "foo.txt") 3))))

(ert-deftest f-size-test/directory ()
  (with-sandbox
   (f-mkdir "bar")
   (f-write "FOO" 'utf-8 "bar/foo.txt")
   (f-write "BAZ" 'utf-8 "bar/baz.txt")
   (should (equal (f-size "bar") 6))))
