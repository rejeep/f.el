;;;; f-write-bytes

(ert-deftest f-write-bytes-test/multibyte-string ()
  (with-sandbox
   (let ((err (should-error (f-write-bytes "☺ ☹" "foo.txt")
                            :type 'wrong-type-argument)))
     (should (equal (cdr err)
                    (list 'f-unibyte-string-p "☺ ☹"))))))

(ert-deftest f-write-bytes-test/unibyte-string ()
  (with-sandbox
   ;; Let's take some random bytes
   (let ((bytes (apply #'unibyte-string (-map #'random (-repeat 100 255)))))
     ;; Make a string of our bytes
     (f-write-bytes bytes "foo.txt")
     (should-exist "foo.txt" bytes))))


;;;; f-write/f-write-text

(ert-deftest f-write-text-test/unibyte-string ()
  (with-sandbox
   (f-write-text (unibyte-string 1 2 3 4 5) 'utf-8 "foo.txt")
   ;; Emacs only makes multibyte strings if actually required.
   (f-write-text "bar" 'utf-8 "bar.txt")
   (should-exist "bar.txt" "bar")))

(ert-deftest f-write-text-test/multibyte-string ()
  (with-sandbox
   (f-write-text "☺ ☹" 'utf-8 "foo.txt")
   (should-exist "foo.txt" (unibyte-string 226 152 186 32 226 152 185))
   (f-write-text "blök" 'iso-8859-1 "foo.txt")
   (should-exist "foo.txt" (unibyte-string 98 108 246 107))))

(ert-deftest f-write-test/alias ()
  (with-sandbox
   (f-write-text (unibyte-string 1 2 3 4 5) 'utf-8 "foo.txt")
   (f-write (unibyte-string 1 2 3 4 5) 'utf-8 "bar.txt")
   (should (equal (f-read "foo.txt") (f-read "bar.txt")))))


;;;; f-read-bytes

(ert-deftest f-read-bytes-test/ ()
  (with-sandbox
   (let ((bytes (apply #'unibyte-string (-map #'random (-repeat 100 255)))))
     (f-write-bytes bytes "foo.txt")
     (let ((content (f-read-bytes "foo.txt")))
       (should-not (multibyte-string-p content))
       (should (f-unibyte-string-p content))
       (should (string= content bytes))))))


;;;; f-read/f-read-text

(ert-deftest f-read-text-test/ ()
  (with-sandbox
   (f-write-bytes (unibyte-string 226 152 185 32 226 152 186) "foo.txt")
   (let ((text (f-read-text "foo.txt" 'utf-8)))
     (should (string= text "☹ ☺"))
     (should (multibyte-string-p text)))
   (f-write-bytes (unibyte-string 252 98 101 114) "foo.txt")
   (let ((text (f-read-text "foo.txt" 'iso-8859-1)))
     (should (string= text "über"))
     (should (multibyte-string-p text)))))

(ert-deftest f-read-text-test/no-coding-specified ()
  (with-sandbox
   (f-write-text "text" 'utf-8 "foo.txt")
   (should (equal (f-read-text "foo.txt") "text"))))

(ert-deftest f-read-test/alias ()
  (with-sandbox
   (f-write-bytes (unibyte-string 226 152 185 32 226 152 186) "foo.txt")
   (should (equal (f-read-text "foo.txt") (f-read "foo.txt")))))
