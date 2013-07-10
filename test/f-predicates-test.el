(ert-deftest f-file?-test/is-file ()
  (with-sandbox
   (f-write "foo.txt")
   (should (f-file? "foo.txt"))))

(ert-deftest f-file?-test/is-directory ()
  (with-sandbox
   (f-mkdir "foo")
   (should-not (f-file? "foo"))))
