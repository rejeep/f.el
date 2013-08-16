;;; f.el --- Modern API for working with files and directories

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.6.0
;; Keywords: files, directories
;; URL: http://github.com/rejeep/f.el
;; Package-Requires: ((s "1.6.1") (dash "1.5.0"))

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

(defalias 'f-short 'f-abbrev)
(defun f-abbrev (path)
  "Abbrev PATH. See `abbreviate-file-name'."
  (abbreviate-file-name path))

(defun f-canonical (path)
  "Return the canonical name of PATH."
  (file-truename path))

(defun f-this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(defun f-write (path &optional content append)
  "Write CONTENT or nothing to PATH. If no content, just create file."
  (with-temp-file path
    (when append
      (insert-file-contents-literally path)
      (goto-char (point-max)))
    (if content (insert content))))

(make-obsolete
 'f-write "Use `f-write-text' instead for proper coding conversion" "0.6")

(defun f-write-text (text coding path)
  "Write TEXT with CODING to PATH.

TEXT is a multibyte string.  CODING is a coding system to encode
TEXT with.  PATH is a file name to write to."
  (unless (multibyte-string-p text)
    (signal 'wrong-type-argument (list 'multibyte-string-p text)))
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

(defun f-mkdir (&rest dirs)
  "Create directories DIRS."
  (let (path)
    (-map
     (lambda (dir)
       (setq path (f-expand dir path))
       (unless (f-directory? path)
         (make-directory path)))
     dirs)))

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
  (equal path (f-parent path)))

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

(defun f-size (path)
  "Return size of PATH.

If PATH is a file, return size of that file. If PATH is
directory, return sum of all files in PATH."
  (if (f-directory? path)
      (-sum (-map 'f-size (f-files path nil t)))
    (nth 7 (file-attributes path))))

(defun f-read (path)
  "Return content of PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(make-obsolete
 'f-read "Use `f-read-text' instead for proper coding conversion" "0.6")

(defun f-read-bytes (path)
  "Read binary data from PATH.

Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun f-read-text (path &optional coding)
  "Read text with PATH, using CODING.

CODING defaults to `prefer-utf-8'.

Return the decoded text as multibyte string."
  (decode-coding-string (f-read-bytes path) (or coding 'prefer-utf-8)))

(defun f-glob (pattern &optional path)
  "Find PATTERN in PATH."
  (file-expand-wildcards
   (f-join (or path default-directory) pattern)))

(defun f--entries (path recursive)
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
                  (setq result (append result (f--entries entry recursive))))))
            entries))
          (t (setq result entries)))
    result))

(defun f-entries (path &optional fn recursive)
  "Find all files and directories in PATH.

FN - called for each found file and directory. If FN returns a thruthy
value, file or directory will be included.
RECURSIVE - Search for files and directories recursive."
  (let ((entries (f--entries path recursive)))
    (if fn (-select fn entries) entries)))

(defun f-directories (path &optional fn recursive)
  "Find all directories in PATH. See `f-entries`."
  (let ((directories (-select 'f-directory? (f--entries path recursive))))
    (if fn (-select fn directories) directories)))

(defun f-files (path &optional fn recursive)
  "Find all files in PATH. See `f-entries`."
  (let ((files (-select 'f-file? (f--entries path recursive))))
    (if fn (-select fn files) files)))

(provide 'f)

;;; f.el ends here
