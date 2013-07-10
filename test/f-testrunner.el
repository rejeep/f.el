(let ((current-directory (file-name-directory load-file-name)))
  (setq f-root-path (expand-file-name ".." current-directory))
  (setq f-test-path (expand-file-name "test" f-root-path))
  (setq f-vendor-path (expand-file-name "vendor" f-root-path)))

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" f-vendor-path)))
(require 'f (expand-file-name "f.el" f-root-path))

(load (expand-file-name "test-helper.el" f-test-path) nil t)
(let ((test-files
       (or
        (mapcar
         (lambda (test-file)
           (expand-file-name test-file f-root-path))
         (cdr argv))
        (directory-files (expand-file-name f-test-path) t "-test.el$"))))
  (mapc
   (lambda (test-file)
     (load test-file nil t))
   test-files))

(ert-run-tests-batch-and-exit t)
