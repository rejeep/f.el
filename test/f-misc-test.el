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
