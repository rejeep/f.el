(defvar f-test/test-path
  (directory-file-name (file-name-directory load-file-name)))

(defvar f-test/root-path
  (directory-file-name (file-name-directory f-test/test-path)))

(defvar f-test/vendor-path
  (expand-file-name "vendor" f-test/root-path))

(require 'cask)

;; This project uses ert-runner, which in turn uses f so to make sure
;; that those functions are not tested, this code unbinds all
;; currently bound f functions.
(let* ((elpa-f-elc
        (car (file-expand-wildcards (concat (cask-elpa-dir) "/f-*/f.elc") :full)))
       (f-history
        (--first (equal elpa-f-elc (car it)) load-history))
       (f-functions
        (--select (and (listp it) (eq (car it) 'defun)) f-history)))
  (-each (-map 'cdr f-functions) 'fmakunbound))

(load (expand-file-name "f" f-test/root-path) :noerror :nomessage)

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" f-test/vendor-path)))
