;;; f.el --- TODO

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: files, directories
;; URL: http://github.com/rejeep/f.el
;; Package-Requires: ((s "1.6.0") (dash "1.4.0"))

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

(defvar f-path-separator "/")

(defun f-join (&rest args)
  (s-join f-path-separator args))

(defun f-expand (file &rest dirs)
  (expand-file-name file (apply 'f-join dirs)))

(defun f-filename (path)
  (file-name-nondirectory path))

(defun f-dirname (path)
  (file-name-directory path))

(defun f-ext (path)
  (file-name-extension path))

(defun f-no-ext (path)
  (file-name-sans-extension path))

(defun f-base (path)
  (f-no-ext (f-filename path)))

(defun f-relative (file &optional path)
  (file-relative-name file path))

(defun f-write (path &optional content)
  (with-temp-file path
    (if content (insert content))))

(defun f-mkdir (&rest dirs)
  (let (dir)
    (while dirs
      (setq dir (f-expand (car dirs) dir))
      (make-directory dir)
      (setq dirs (cdr dirs)))))

(defun f-delete (path &optional force)
  (if (f-file? path)
      (delete-file path nil)
    (delete-directory path force nil)))

(defun f-symlink (source path)
  (make-symbolic-link source path))

(defun f-move (from to)
  (rename-file from to t))

(defun f-chmod (path mode)
  (set-file-modes path mode))

(defun f-chown (path user group &optional recursive)
  ""
  )

(defun f-exists? (path)
  (file-exists-p path))

(defun f-directory? (path)
  (file-directory-p path))

(defun f-file? (path)
  (file-regular-p path))

(defun f-symlink? (path)
  (file-symlink-p path))

(defun f-readable? (path)
  (file-readable-p path))

(defun f-writable? (path)
  (file-writable-p path))

(defun f-executable? (path)
  (file-executable-p path))

(defun f-size (path)
  (if (f-directory? path)
      (apply '+ (-map 'f-size (f-files path nil t)))
    (nth 7 (file-attributes path))))

(defun f-last-change (path)
  ""
  )

(defun f-last-access (path)
  ""
  )

(defun f-read (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun f-glob (pattern &optional path)
  (file-expand-wildcards
   (f-join (or path default-directory) pattern)))

(defun f--entries (path)
  (-reject
   (lambda (file)
     (or
      (equal (f-filename file) ".")
      (equal (f-filename file) "..")))
   (directory-files path t)))

(defun f-entries (path &optional fn recursive)
  (let (result (entries (f--entries path)))
    (cond (recursive
           (-map
            (lambda (entry)
              (if (f-file? entry)
                  (setq result (cons entry result))
                (when (f-directory? entry)
                  (setq result (cons entry result))
                  (setq result (append result (f-entries entry fn recursive))))))
            entries))
          (t (setq result entries)))
    (if fn (-filter fn result) result)))

(defun f-directories (path &optional fn recursive)
  (let ((directories (f-entries path 'f-directory? recursive)))
    (if fn (-filter fn directories) directories)))

(defun f-files (path &optional fn recursive)
  (let ((files (f-entries path 'f-file? recursive)))
    (if fn (-filter fn files) files)))

(provide 'f)

;;; f.el ends here
