;;; f-stats-test.el --- F: Stats tests  -*- lexical-binding: t; -*-

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


;;;; f-size

(ert-deftest f-size-test/empty-file ()
  (with-playground
   (f-touch "foo.txt")
   (should (equal (f-size "foo.txt") 0))))

(ert-deftest f-size-test/file-with-content ()
  (with-playground
   (f-write "FOO" 'utf-8 "foo.txt")
   (should (equal (f-size "foo.txt") 3))))

(ert-deftest f-size-test/directory ()
  (with-playground
   (f-mkdir "bar")
   (f-write "FOO" 'utf-8 "bar/foo.txt")
   (f-write "BAZ" 'utf-8 "bar/baz.txt")
   (should (equal (f-size "bar") 6))))


;;;; f-target

(ert-deftest f-target-test/non-symlink ()
  (with-playground
   (f-touch "foo.txt")
   (should (equal (f-target "foo.txt") "foo.txt"))))

(ert-deftest f-target-test/recursive ()
  (with-playground
   (f-touch "foo.txt")
   (f-symlink "foo.txt" "bar")
   (f-symlink "bar" "baz")
   (should (equal (f-target "baz") "foo.txt"))))

(ert-deftest f-target-test/non-recursive ()
  (with-playground
   (f-touch "foo.txt")
   (f-symlink "foo.txt" "bar")
   (f-symlink "bar" "baz")
   (should (equal (f-target "baz" :non-recursive) "bar"))
   (should (equal (f-target "bar" :non-recursive) "foo.txt"))))

(provide 'f-stats-test)

;;; f-stats-test.el ends here
