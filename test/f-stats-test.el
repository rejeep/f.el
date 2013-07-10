
(ert-deftest f-read-test/empty ()
  (with-sandbox
   (f-write "foo.txt")
   (should (equal (f-read "foo.txt") ""))))

(ert-deftest f-read-test/with-content ()
  (with-sandbox
   (f-write "foo.txt" "FOO")
   (should (equal (f-read "foo.txt") "FOO"))))
