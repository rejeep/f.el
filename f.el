;;; f.el --- TODO

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: files, directories
;; URL: http://github.com/rejeep/f.el

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

(defvar f-path-separator "/")

(defun f-join (&rest args)
  (mapconcat 'identity args f-path-separator))

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

(defun f-glob (pattern &optional path)
  (unless path
    (setq path default-directory))
  (file-expand-wildcards (f-join path pattern)))

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
  ""
  )

(defun f-symlink (source path)
  ""
  )

(defun f-move (from to)
  ""
  )

(defun f-chmod (path mode &optional recursive)
  ""
  )

(defun f-chown (path user group &optional recursive)
  ""
  )

(defun f-exists? (path)
  ""
  )

(defun f-directory? (path)
  ""
  )

(defun f-file? (path)
  ""
  )

(defun f-symlink? (path)
  ""
  )

(defun f-readable? (path)
  ""
  )

(defun f-writable? (path)
  ""
  )

(defun f-executable? (path)
  ""
  )

(defun f-size (path)
  ""
  )

(defun f-last-change (path)
  ""
  )

(defun f-last-access (path)
  ""
  )

(defun f-read (path)
  ""
  )

(defun f-entries (path &optional pattern recursive)
  ""
  )

(defun f-directories (path &optional pattern recursive)
  ""
  )

(defun f-files (path &optional pattern recursive)
  ""
  )

(provide 'f)

;;; f.el ends here
