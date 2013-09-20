;;; f.el --- Modern API for working with files and directories

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.11.0
;; Keywords: files, directories
;; URL: http://github.com/rejeep/f.el
;; Package-Requires: ((s "1.7.0") (dash "2.2.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 's)
(require 'dash)


;;;; Paths

(defun f-join (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (f-relative? (car args))))
    (-map
     (lambda (arg)
       (setq path (f-expand arg path)))
     args)
    (if relative (f-relative path) path)))

(defun f-expand (path &optional dir)
  "Expand PATH relative to DIR (or `default-directory')."
  (directory-file-name (expand-file-name path dir)))

(defun f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))

(defalias 'f-parent 'f-dirname)
(defun f-dirname (path)
  "Return the parent directory to PATH."
  (let ((parent (file-name-directory (f-expand path default-directory))))
    (if (f-relative? path)
        (f-relative parent)
      (directory-file-name parent))))

(defun f-ext (path)
  "Return the file extension of PATH."
  (file-name-extension path))

(defun f-no-ext (path)
  "Return everything but the file extension of PATH."
  (file-name-sans-extension path))

(defun f-base (path)
  "Return the name of PATH, excluding the extension if file."
  (f-no-ext (f-filename path)))

(defun f-relative (path &optional dir)
  "Return PATH relative to DIR."
  (file-relative-name path dir))

(defalias 'f-abbrev 'f-short)
(defun f-short (path)
  "Return abbrev of PATH. See `abbreviate-file-name'."
  (abbreviate-file-name path))

(defun f-long (path)
  "Return long version of PATH."
  (f-expand path))

(defun f-canonical (path)
  "Return the canonical name of PATH."
  (file-truename path))

(defun f-slash (path)
  "Append slash to PATH unless one already.

Some functions, such as `call-process' requires there to be an
ending slash."
  (if (or (f-file? path) (s-ends-with? (f-path-separator) path))
      path
    (s-concat path (f-path-separator))))

(defun f-full (path)
  "Return absolute path to PATH, with ending slash."
  (f-slash (f-long path)))


;;;; I/O

(defun f-read-bytes (path)
  "Read binary data from PATH.

Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defalias 'f-read 'f-read-text)
(defun f-read-text (path &optional coding)
  "Read text with PATH, using CODING.

CODING defaults to `utf-8'.

Return the decoded text as multibyte string."
  (decode-coding-string (f-read-bytes path) (or coding 'utf-8)))

(defalias 'f-write 'f-write-text)
(defun f-write-text (text coding path)
  "Write TEXT with CODING to PATH.

TEXT is a multibyte string.  CODING is a coding system to encode
TEXT with.  PATH is a file name to write to."
  (f-write-bytes (encode-coding-string text coding) path))

(defun f-unibyte-string-p (s)
  "Determine whether S is a unibyte string."
  (not (multibyte-string-p s)))

(defun f-write-bytes (data path)
  "Write binary DATA to PATH.

DATA is a unibyte string.  PATH is a file name to write to."
  (unless (f-unibyte-string-p data)
    (signal 'wrong-type-argument (list 'f-unibyte-string-p data)))
  (let ((file-coding-system-alist nil)
        (coding-system-for-write 'binary))
    (with-temp-file path
      (setq buffer-file-coding-system 'binary)
      (set-buffer-multibyte nil)
      (insert data))))


;;;; Destructive

(defun f-mkdir (&rest dirs)
  "Create directories DIRS."
  (let (path)
    (-each
     dirs
     (lambda (dir)
       (setq path (f-expand dir path))
       (unless (f-directory? path)
         (make-directory path))))))

(defun f-delete (path &optional force)
  "Delete PATH, which can be file or directory.

If FORCE is t, a directory will be deleted recursively."
  (if (f-file? path)
      (delete-file path)
    (delete-directory path force)))

(defun f-symlink (source path)
  "Create a symlink to `source` from `path`."
  (make-symbolic-link source path))

(defun f-move (from to)
  "Move or rename FROM to TO."
  (rename-file from to t))

(defun f-copy (from to)
  "Copy file or directory."
  (if (f-file? from)
      (copy-file from to)
    (copy-directory from to)))

(defun f-touch (path)
  "Update PATH last modification date or create if it does not exist."
  (if (f-file? path)
      (set-file-times path)
    (f-write-bytes "" path)))


;;;; Predicates

(defun f-exists? (path)
  "Return t if PATH exists, false otherwise."
  (file-exists-p path))

(defalias 'f-dir? 'f-directory?)
(defun f-directory? (path)
  "Return t if PATH is directory, false otherwise."
  (file-directory-p path))

(defun f-file? (path)
  "Return t if PATH is file, false otherwise."
  (file-regular-p path))

(defun f-symlink? (path)
  "Return t if PATH is symlink, false otherwise."
  (file-symlink-p path))

(defun f-readable? (path)
  "Return t if PATH is readable, false otherwise."
  (file-readable-p path))

(defun f-writable? (path)
  "Return t if PATH is writable, false otherwise."
  (file-writable-p path))

(defun f-executable? (path)
  "Return t if PATH is executable, false otherwise."
  (file-executable-p path))

(defun f-absolute? (path)
  "Return t if PATH is absolute, false otherwise."
  (file-name-absolute-p path))

(defun f-relative? (path)
  "Return t if PATH is relative, false otherwise."
  (not (f-absolute? path)))

(defun f-root? (path)
  "Return t if PATH is root directory, false otherwise."
  (f-same? path (f-parent path)))

(defun f-ext? (path &optional ext)
  "Return t if extension of PATH is EXT, false otherwise.

If EXT is nil or omitted, return t if PATH has any extension,
false otherwise."
  (if ext
      (string= (f-ext path) ext)
    (not (eq (f-ext path) nil))))

(defalias 'f-equal? 'f-same?)
(defun f-same? (path-a path-b)
  "Return t if PATH-A and PATH-b are references to same file."
  (equal
   (f-canonical (f-expand path-a))
   (f-canonical (f-expand path-b))))

(defun f-parent-of? (path-a path-b)
  "Return t if PATH-A is parent of PATH-B."
  (unless (f-same? path-a path-b)
    (f-same? path-a (f-parent path-b))))

(defun f-child-of? (path-a path-b)
  "Return t if PATH-A is child of PATH-B."
  (unless (f-same? path-a path-b)
    (f-same? (f-parent path-a) path-b)))

(defun f-ancestor-of? (path-a path-b)
  "Return t if PATH-A is ancestor of PATH-B."
  (unless (f-same? path-a path-b)
    (f-same?
     path-a
     (f-up
      (lambda (path)
        (f-same? path path-a))
      path-b))))

(defun f-descendant-of? (path-a path-b)
  "Return t if PATH-A is desendant of PATH-B."
  (unless (f-same? path-a path-b)
    (f-same?
     path-b
     (f-up
      (lambda (path)
        (f-same? path path-b))
      path-a))))


;;;; Stats

(defun f-size (path)
  "Return size of PATH.

If PATH is a file, return size of that file. If PATH is
directory, return sum of all files in PATH."
  (if (f-directory? path)
      (-sum (-map 'f-size (f-files path nil t)))
    (nth 7 (file-attributes path))))


;;;; Misc

(defun f-this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(defun f-path-separator ()
  "Return path separator."
  (let ((x (f-expand "x" "y")))
    (substring x (- (length x) 2) (- (length x) 1))))

(defun f-glob (pattern &optional path)
  "Find PATTERN in PATH."
  (file-expand-wildcards
   (f-join (or path default-directory) pattern)))

(defun f--collect-entries (path recursive)
  (let (result
        (entries
         (-reject
          (lambda (file)
            (or
             (equal (f-filename file) ".")
             (equal (f-filename file) "..")))
          (directory-files path t))))
    (cond (recursive
           (-map
            (lambda (entry)
              (if (f-file? entry)
                  (setq result (cons entry result))
                (when (f-directory? entry)
                  (setq result (cons entry result))
                  (setq result (append result (f--collect-entries entry recursive))))))
            entries))
          (t (setq result entries)))
    result))

(defmacro f--entries (path body &optional recursive)
  "Anaphoric version of `f-entries'."
  `(f-entries
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-entries (path &optional fn recursive)
  "Find all files and directories in PATH.

FN - called for each found file and directory. If FN returns a thruthy
value, file or directory will be included.
RECURSIVE - Search for files and directories recursive."
  (let ((entries (f--collect-entries path recursive)))
    (if fn (-select fn entries) entries)))

(defmacro f--directories (path body &optional recursive)
  "Anaphoric version of `f-directories'."
  `(f-directories
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-directories (path &optional fn recursive)
  "Find all directories in PATH. See `f-entries`."
  (let ((directories (-select 'f-directory? (f--collect-entries path recursive))))
    (if fn (-select fn directories) directories)))

(defmacro f--files (path body &optional recursive)
  "Anaphoric version of `f-files'."
  `(f-files
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-files (path &optional fn recursive)
  "Find all files in PATH. See `f-entries`."
  (let ((files (-select 'f-file? (f--collect-entries path recursive))))
    (if fn (-select fn files) files)))

(defun f-root ()
  "Return absolute root."
  (let ((dir default-directory))
    (while (not (f-root? dir))
      (setq dir (f-parent dir)))
    dir))

(defmacro f--up (body &optional dir)
  "Anaphoric version of `f-up'."
  `(f-up
    (lambda (path)
      (let ((it path))
        ,body))
    ,dir))

(defun f-up (fn &optional dir)
  "Traverse up as long as FN returns nil, starting at DIR."
  (unless dir
    (setq dir default-directory))
  (when (f-relative? dir)
    (setq dir (f-expand dir)))
  (unless (f-exists? dir)
    (error "File %s does not exist" dir))
  (let ((parent (f-parent dir)))
    (if (f-root? parent)
        parent
      (if (funcall fn dir)
          dir
        (f-up fn parent)))))

(provide 'f)

;;; f.el ends here
