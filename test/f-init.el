(defvar f-test/test-path
  (directory-file-name (file-name-directory load-file-name)))

(defvar f-test/root-path
  (directory-file-name (file-name-directory f-test/test-path)))

(defvar f-test/vendor-path
  (expand-file-name "vendor" f-test/root-path))

(unload-feature 'f 'force)

(load (expand-file-name "f" f-test/root-path) :noerror :nomessage)

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" f-test/vendor-path)))
