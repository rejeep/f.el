;;; f-predicates-test.el --- F: Predicate tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; URL: http://github.com/rejeep/f.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


;;;; f-exists-p

(ert-deftest f-exists-p-test/directory-does-exist ()
  (with-playground
   (f-mkdir "foo")
   (should (f-exists-p "foo"))))

(ert-deftest f-exists-p-test/file-does-exist ()
  (with-playground
   (f-touch "foo.txt")
   (should (f-exists-p "foo.txt"))))

(ert-deftest f-exists-p-test/does-not-exists ()
  (with-playground
   (should-not (f-exists-p "foo.txt"))))


;;;; f-directory-p/f-dir-p

(ert-deftest f-directory-p-test/is-directory ()
  (with-playground
   (f-mkdir "foo")
   (should (f-directory-p "foo"))))

(ert-deftest f-directory-p-test/is-file ()
  (with-playground
   (f-touch "foo.txt")
   (should-not (f-directory-p "foo.txt"))))

(ert-deftest f-dir-p-test/alias ()
  (with-playground
   (f-mkdir "foo")
   (should (f-dir-p "foo"))))


;;;; f-file-p

(ert-deftest f-file-p-test/is-file ()
  (with-playground
   (f-touch "foo.txt")
   (should (f-file-p "foo.txt"))))

(ert-deftest f-file-p-test/is-directory ()
  (with-playground
   (f-mkdir "foo")
   (should-not (f-file-p "foo"))))


;;;; f-symlink-p

(ert-deftest f-symlink-p-test/is-symlink ()
  (with-playground
   (f-touch "foo.txt")
   (f-symlink "foo.txt" "foo.link")
   (should (eq (f-symlink-p "foo.link") t))))

(ert-deftest f-symlink-p-test/is-not-symlink ()
  (with-playground
   (f-touch "foo.txt")
   (should (eq (f-symlink-p "foo.txt") nil))))


;;;; f-readable-p

(ert-deftest f-readable-p-test/is-readable ()
  (with-playground
   (f-touch "foo.txt")
   (chmod "foo.txt" "400")
   (should (f-readable-p "foo.txt"))))

(ert-deftest f-readable-p-test/is-not-readable ()
  (with-playground
   (f-touch "foo.txt")
   (chmod "foo.txt" "000")
   (should-not (f-readable-p "foo.txt"))))


;;;; f-writable-p

(ert-deftest f-writable-p-test/is-writable ()
  (with-playground
   (f-touch "foo.txt")
   (chmod "foo.txt" "700")
   (should (f-writable-p "foo.txt"))))

(ert-deftest f-writable-p-test/is-not-writable ()
  (with-playground
   (f-touch "foo.txt")
   (chmod "foo.txt" "400")
   (should-not (f-writable-p "foo.txt"))))

(ert-deftest f-executable-p-test/is-executable ()
  (with-playground
   (f-touch "foo.txt")
   (chmod "foo.txt" "100")
   (should (f-executable-p "foo.txt"))))

(ert-deftest f-executable-p-test/is-not-executable ()
  (with-playground
   (f-touch "foo.txt")
   (chmod "foo.txt" "200")
   (should-not (f-executable-p "foo.txt"))))


;;;; f-absolute-p

(ert-deftest f-absolute-p-test/is-absolute ()
  (should (f-absolute-p "/full/path/to/dir")))

(ert-deftest f-absolute-p-test/is-relative ()
  (should-not (f-absolute-p "path/to/dir")))


;;;; f-relative-p

(ert-deftest f-relative-p-test/is-relative ()
  (should (f-relative-p "path/to/dir")))

(ert-deftest f-relative-p-test/is-absolute ()
  (should-not (f-relative-p "/full/path/to/dir")))


;;;; f-root-p

(ert-deftest f-root-p-test/is-root ()
  (should (f-root-p "/")))

(ert-deftest f-root-p-test/is-not-root ()
  (should-not (f-root-p "/not/root")))

(ert-deftest f-root-p-test/is-root-weird-syntax ()
  (should (f-root-p "/bin/..")))


;;;; f-ext-p

(ert-deftest f-ext-p-test/ext-does-match ()
  (with-playground
   (f-touch "foo.el")
   (should (f-ext-p "foo.el" "el"))))

(ert-deftest f-ext-p-test/ext-does-not-match ()
  (with-playground
   (f-touch "foo.el")
   (should-not (f-ext-p "foo.el" "txt"))))

(ert-deftest f-ext-p-test/with-ext ()
  (with-playground
   (f-touch "foo.el")
   (should (f-ext-p "foo.el"))))

(ert-deftest f-ext-p-test/without-ext ()
  (with-playground
   (f-touch "foo")
   (should-not (f-ext-p "foo"))))


;;;; f-same-p/f-equal-p

(ert-deftest f-same-p/does-not-exist ()
  (with-playground
   (should (f-same-p "foo" "foo"))))

(ert-deftest f-same-p/relative-equal-file ()
  (with-playground
   (f-touch "foo")
   (should (f-same-p "foo" "foo"))))

(ert-deftest f-same-p/relative-equal-directory ()
  (with-playground
   (f-mkdir "foo")
   (should (f-same-p "foo" "foo"))))

(ert-deftest f-same-p/relative-not-equal-files ()
  (with-playground
   (f-touch "foo")
   (f-touch "bar")
   (should-not (f-same-p "foo" "bar"))))

(ert-deftest f-same-p/relative-not-equal-directories ()
  (with-playground
   (f-mkdir "foo")
   (f-mkdir "bar")
   (should-not (f-same-p "foo" "bar"))))

(ert-deftest f-same-p/absolute-equal-file ()
  (with-playground
   (f-touch (f-expand "foo" f-test/playground-path))
   (should (f-same-p (f-expand "foo" f-test/playground-path)
                    (f-expand "foo" f-test/playground-path)))))

(ert-deftest f-same-p/absolute-equal-directory ()
  (with-playground
   (f-mkdir (f-expand "foo" f-test/playground-path))
   (should (f-same-p (f-expand "foo" f-test/playground-path)
                    (f-expand "foo" f-test/playground-path)))))

(ert-deftest f-same-p/absolute-not-equal-files ()
  (with-playground
   (f-touch (f-expand "foo" f-test/playground-path))
   (f-touch (f-expand "bar" f-test/playground-path))
   (should-not (f-same-p (f-expand "foo" f-test/playground-path)
                        (f-expand "bar" f-test/playground-path)))))

(ert-deftest f-same-p/absolute-not-equal-directories ()
  (with-playground
   (f-mkdir (f-expand "foo" f-test/playground-path))
   (f-mkdir (f-expand "bar" f-test/playground-path))
   (should-not (f-same-p (f-expand "foo" f-test/playground-path)
                        (f-expand "bar" f-test/playground-path)))))

(ert-deftest f-same-p/relative-and-absolute-equal-file ()
  (with-playground
   (f-touch "foo")
   (should (f-same-p "foo" (f-expand "foo" f-test/playground-path)))))

(ert-deftest f-same-p/relative-and-absolute-equal-directory ()
  (with-playground
   (f-mkdir "foo")
   (should (f-same-p "foo" (f-expand "foo" f-test/playground-path)))))

(ert-deftest f-same-p/relative-and-absolute-not-equal-files ()
  (with-playground
   (f-touch "foo")
   (f-touch "bar")
   (should-not (f-same-p "foo" (f-expand "bar" f-test/playground-path)))))

(ert-deftest f-same-p/relative-and-absolute-not-equal ()
  (with-playground
   (f-mkdir "foo")
   (f-mkdir "bar")
   (should-not (f-same-p "foo" (f-expand "bar" f-test/playground-path)))))

(ert-deftest f-same-p/symlink ()
  (with-playground
   (f-touch "foo")
   (f-symlink "foo" "bar")
   (f-same-p "foo" "bar")))

(ert-deftest f-equal-p/alias ()
  (with-playground
   (f-touch "foo")
   (should (f-equal-p "foo" "foo"))))

(ert-deftest f-same-p/theoretical-paths ()
  (should (f-same-p "/a/b" "/a/b"))
  (should (f-same-p "/a/b/../c" "/a/c"))
  (should (f-same-p "a/b/../c" "a/c")))


;;;; f-parent-of-p

(ert-deftest f-parent-of-p-test/is-parent ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should (equal t (f-parent-of-p "foo" "foo/bar")))
   (should (equal t (f-parent-of-p "foo/bar" "foo/bar/baz")))
   (should (equal t (f-parent-of-p "foo/bar/baz" "foo/bar/baz/qux")))))

(ert-deftest f-parent-of-p-test/is-not-parent ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-parent-of-p "foo/bar" "foo"))
   (should-not (f-parent-of-p "foo/bar/baz" "foo/bar"))
   (should-not (f-parent-of-p "foo/bar/baz/qux" "foo/bar/baz"))))

(ert-deftest f-parent-of-p-test/is-same ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-parent-of-p "foo" "foo"))
   (should-not (f-parent-of-p "foo/bar" "foo/bar"))
   (should-not (f-parent-of-p "foo/bar/baz" "foo/bar/baz"))
   (should-not (f-parent-of-p "foo/bar/baz/qux" "foo/bar/baz/qux"))
   (should-not (f-parent-of-p (f-root) (f-root)))))


;;;; f-child-of-p

(ert-deftest f-child-of-p-test/is-child ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should (equal t (f-child-of-p "foo/bar" "foo")))
   (should (equal t (f-child-of-p "foo/bar/baz" "foo/bar")))
   (should (equal t (f-child-of-p "foo/bar/baz/qux" "foo/bar/baz")))))

(ert-deftest f-child-of-p-test/is-not-child ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-child-of-p "foo" "foo/bar"))
   (should-not (f-child-of-p "foo/bar" "foo/bar/baz"))
   (should-not (f-child-of-p "foo/bar/baz" "foo/bar/baz/qux"))))

(ert-deftest f-child-of-p-test/is-same ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-child-of-p "foo" "foo"))
   (should-not (f-child-of-p "foo/bar" "foo/bar"))
   (should-not (f-child-of-p "foo/bar/baz" "foo/bar/baz"))
   (should-not (f-child-of-p "foo/bar/baz/qux" "foo/bar/baz/qux"))
   (should-not (f-child-of-p (f-root) (f-root)))))


;;;; f-ancestor-of-p

(ert-deftest f-ancestor-of-p-test/is-ancestor ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should (equal t (f-ancestor-of-p "foo" "foo/bar")))
   (should (equal t (f-ancestor-of-p "foo" "foo/bar/baz")))
   (should (equal t (f-ancestor-of-p "foo" "foo/bar/baz/qux")))
   (should (equal t (f-ancestor-of-p "foo/bar" "foo/bar/baz")))
   (should (equal t (f-ancestor-of-p "foo/bar" "foo/bar/baz/qux")))
   (should (equal t (f-ancestor-of-p "foo/bar/baz" "foo/bar/baz/qux")))
   (should (equal t (f-ancestor-of-p (f-root) (f-expand (car (f-directories (f-root))) (f-root)))))))

(ert-deftest f-ancestor-of-p-test/is-not-ancestor ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-ancestor-of-p "foo/bar" "foo"))
   (should-not (f-ancestor-of-p "foo/bar/baz" "foo"))
   (should-not (f-ancestor-of-p "foo/bar/baz/qux" "foo"))
   (should-not (f-ancestor-of-p "foo/bar/baz" "foo/bar"))
   (should-not (f-ancestor-of-p "foo/bar/baz/qux" "foo/bar"))
   (should-not (f-ancestor-of-p "foo/bar/baz/qux" "foo/bar/baz"))
   (should-not (f-ancestor-of-p (f-expand (car (f-directories (f-root))) (f-root)) (f-root)))))

(ert-deftest f-ancestor-of-p-test/is-same ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-ancestor-of-p "foo" "foo"))
   (should-not (f-ancestor-of-p "foo/bar" "foo/bar"))
   (should-not (f-ancestor-of-p "foo/bar/baz" "foo/bar/baz"))
   (should-not (f-ancestor-of-p "foo/bar/baz/qux" "foo/bar/baz/qux"))
   (should-not (f-ancestor-of-p (f-root) (f-root)))))


;;;; f-descendant-of-p

(ert-deftest f-descendant-of-p-test/is-descendant ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should (equal t (f-descendant-of-p "foo/bar" "foo")))
   (should (equal t (f-descendant-of-p "foo/bar/baz" "foo")))
   (should (equal t (f-descendant-of-p "foo/bar/baz/qux" "foo")))
   (should (equal t (f-descendant-of-p "foo/bar/baz" "foo/bar")))
   (should (equal t (f-descendant-of-p "foo/bar/baz/qux" "foo/bar")))
   (should (equal t (f-descendant-of-p "foo/bar/baz/qux" "foo/bar/baz")))
   (should (equal t (f-descendant-of-p (f-expand (car (f-directories (f-root))) (f-root)) (f-root))))
   (should (equal t (f-descendant-of-p "/foo/bar/baz/qux" "/foo/bar/baz")))))

(ert-deftest f-descendant-of-p-test/is-not-descendant ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-descendant-of-p "foo" "foo/bar"))
   (should-not (f-descendant-of-p "foo" "foo/bar/baz"))
   (should-not (f-descendant-of-p "foo" "foo/bar/baz/qux"))
   (should-not (f-descendant-of-p "foo/bar" "foo/bar/baz"))
   (should-not (f-descendant-of-p "foo/bar" "foo/bar/baz/qux"))
   (should-not (f-descendant-of-p "foo/bar/baz" "foo/bar/baz/qux"))
   (should-not (f-descendant-of-p (f-root) (f-expand (car (f-directories (f-root))) (f-root))))
   (should-not (f-descendant-of-p "/foo/bar/baz" "/foo/bar/baz/qux"))))

(ert-deftest f-descendant-of-p-test/is-same ()
  (with-playground
   (f-mkdir "foo" "bar" "baz" "qux")
   (should-not (f-descendant-of-p "foo" "foo"))
   (should-not (f-descendant-of-p "foo/bar" "foo/bar"))
   (should-not (f-descendant-of-p "foo/bar/baz" "foo/bar/baz"))
   (should-not (f-descendant-of-p "foo/bar/baz/qux" "foo/bar/baz/qux"))
   (should-not (f-descendant-of-p (f-root) (f-root)))))


;;;; f-hidden-p

(ert-deftest f-hidden-p-test/is-hidden ()
  (with-playground
   (should (f-hidden-p ".foo"))
   (should (f-hidden-p ".bar"))
   (should (f-hidden-p ".baz"))
   (should (f-hidden-p ".foo" 'any))
   (should (f-hidden-p ".bar" 'any))
   (should (f-hidden-p ".baz" 'any))
   (should (f-hidden-p ".foo" 'last))
   (should (f-hidden-p ".bar" 'last))
   (should (f-hidden-p ".baz" 'last))))

(ert-deftest f-hidden-p-test/is-not-hidden ()
  (with-playground
   (should-not (f-hidden-p "foo"))
   (should-not (f-hidden-p "bar"))
   (should-not (f-hidden-p "baz"))
   (should-not (f-hidden-p "foo" 'any))
   (should-not (f-hidden-p "bar" 'any))
   (should-not (f-hidden-p "baz" 'any))
   (should-not (f-hidden-p "foo" 'last))
   (should-not (f-hidden-p "bar" 'last))
   (should-not (f-hidden-p "baz" 'last))))

(ert-deftest f-hidden-p-test/child-is-hidden ()
  (should     (f-hidden-p ".foo/bar"))
  (should     (f-hidden-p ".foo/bar" 'any))
  (should-not (f-hidden-p ".foo/bar" 'last))
  (should-not (f-hidden-p "foo/.bar"))
  (should     (f-hidden-p "foo/.bar" 'any))
  (should     (f-hidden-p "foo/.bar" 'last))
  (should-not (f-hidden-p "foo/.bar/baz"))
  (should     (f-hidden-p "foo/.bar/baz" 'any))
  (should-not (f-hidden-p "foo/.bar/baz" 'last))
  (should-not (f-hidden-p "foo/bar/.baz"))
  (should     (f-hidden-p "foo/bar/.baz" 'any))
  (should     (f-hidden-p "foo/bar/.baz" 'last)))

(ert-deftest f-hidden-p-test/child-is-not-hidden ()
  (should-not (f-hidden-p "foo/bar"))
  (should-not (f-hidden-p "foo/bar" 'any))
  (should-not (f-hidden-p "foo/bar" 'last))
  (should     (f-hidden-p ".foo/bar"))
  (should     (f-hidden-p ".foo/bar" 'any))
  (should-not (f-hidden-p ".foo/bar" 'last))
  (should-not (f-hidden-p "foo/.bar/baz"))
  (should     (f-hidden-p "foo/.bar/baz" 'any))
  (should-not (f-hidden-p "foo/.bar/baz" 'last))
  (should-not (f-hidden-p "./foo/.bar/baz"))
  (should     (f-hidden-p "./foo/.bar/baz" 'any))
  (should-not (f-hidden-p "./foo/.bar/baz" 'last))
  (should-not (f-hidden-p "../foo/bar/baz"))
  (should-not (f-hidden-p "../foo/bar/baz" 'any))
  (should-not (f-hidden-p "../foo/bar/baz" 'last))
  (should-not (f-hidden-p "../foo/.bar/baz"))
  (should     (f-hidden-p "../foo/.bar/baz" 'any))
  (should-not (f-hidden-p "../foo/.bar/baz" 'last)))

;;; f-empty-p

(ert-deftest f-empty-p-test/empty-file-is-empty ()
  (with-playground
   (f-write-text "" 'utf-8 "foo.txt")
   (should (f-empty-p "foo.txt"))))

(ert-deftest f-empty-p-test/file-containing-text-is--not-empty ()
  (with-playground
   (f-write-text "hello" 'utf-8 "derp.txt")
   (should-not (f-empty-p "derp.txt"))))

(ert-deftest f-empty-p-test/empty-directory-is-empty ()
  (with-playground
   (f-mkdir "bar")
   (should (f-empty-p "bar"))))

(ert-deftest f-empty-p-test/directory-containing-files-is--not-empty ()
  (with-playground
   (f-mkdir "bar")
   (f-write-text "hello" 'utf-8 "bar/derp.txt")
   (should-not (f-empty-p "bar"))))

(provide 'f-predicates-test)

;;; f-predicates-test.el ends here
