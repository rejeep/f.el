# f.el [![Build Status](https://api.travis-ci.org/rejeep/f.el.png?branch=master)](http://travis-ci.org/rejeep/f.el)

Much inspired by [@magnars](https://github.com/magnars)'s excellent
[s.el](https://github.com/magnars/s.el) and
[dash.el](https://github.com/magnars/dash.el),
[f.el](https://github.com/rejeep/f.el) is a modern API for working
with files and directories in Emacs.

## Installation

It's available on [Melpa](http://melpa.milkbox.net/):

    M-x package-install f

Or you can just dump `f.el` in your load path somewhere.

## API

### Paths

* [f-join](#f-join-rest-args) `(&rest args)`
* [f-expand](#f-expand-path-optional-dir) `(path &optional dir)`
* [f-filename](#f-filename-path) `(path)`
* [f-dirname](#f-dirname-path) `(path)`
* [f-ext](#f-ext-path) `(path)`
* [f-no-ext](#f-no-ext-path) `(path)`
* [f-base](#f-base-path) `(path)`
* [f-relative](#f-relative-path-optional-file) `(path &optional file)`
* [f-short](#f-short-path) `(path)`
* [f-long](#f-long-path) `(path)`
* [f-canonical](#f-canonical-path) `(path)`
* [f-slash](#f-slash-path) `(path)`
* [f-full](#f-full-path) `(path)`

### I/O

* [f-read-bytes](#f-read-bytes-path) `(path)`
* [f-write-bytes](#f-write-bytes-path) `(path)`
* [f-read-text](#f-read-text-path-optional-coding) `(path &optional coding)`
* [f-write-text](#f-write-text-text-coding-path)`(text coding path)`

### Destructive

* [f-mkdir](#f-mkdir-rest-dirs) `(&rest dirs)`
* [f-delete](#f-delete-path-optional-force) `(path &optional force)`
* [f-symlink](#f-symlink-source-path) `(source path)`
* [f-move](#f-move-from-to) `(from to)`
* [f-copy](#f-copy-from-to) `(from to)`
* [f-touch](#f-touch-path) `(path)`

### Predicates

* [f-exists?](#f-exists-path) `(path)`
* [f-directory?](#f-directory-path) `(path)`
* [f-file?](#f-file-path) `(path)`
* [f-symlink?](#f-symlink-path) `(path)`
* [f-readable?](#f-readable-path) `(path)`
* [f-writable?](#f-writable-path) `(path)`
* [f-executable?](#f-executable-path) `(path)`
* [f-absolute?](#f-absolute-path) `(path)`
* [f-relative?](#f-relative-path) `(path)`
* [f-root?](#f-root-path) `(path)`
* [f-ext?](#f-ext-path-ext) `(path ext)`
* [f-same?](#f-same-path-a-path-b) `(path-a path-b)`

### Stats

* [f-size](#f-size-path) `(path)`

### Misc

* [f-this-file](#f-this-file-) `()`
* [f-path-separator](#f-path-separator-) `()`
* [f-glob](#f-glob-pattern-optional-path) `(pattern &optional path)`
* [f-entries](#f-entries-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-directories](#f-directories-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-files](#f-files-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-root](#f-root-) `()`

## Documentation and examples

### f-join `(&rest args)`

{{f-join}}

```lisp
(f-join "path") ;; => "path"
(f-join "path" "to") ;; => "path/to"
(f-join "path" "to" "heaven") ;; => "path/to/heaven"
```

### f-expand `(path &optional dir)`

{{f-expand}}

```lisp
(f-expand "name") ;; => "/default/directory/name"
(f-expand "name" "other/directory") ;; => "other/directory/name"
```

### f-filename `(path)`

{{f-filename}}

```lisp
(f-filename "path/to/file.ext") ;; => "file.ext"
(f-filename "path/to/directory") ;; => "directory"
```

### f-dirname `(path)`

{{f-dirname}}

Alias: `f-parent`

```lisp
(f-dirname "path/to/file.ext") ;; => "path/to"
(f-dirname "path/to/directory") ;; => "path/to"
```

### f-ext `(path)`

{{f-ext}}

```lisp
(f-ext "path/to/file.ext") ;; => "ext"
(f-ext "path/to/directory") ;; => nil
```

### f-no-ext `(path)`

{{f-no-ext}}

```lisp
(f-no-ext "path/to/file.ext") ;; => "path/to/file"
(f-no-ext "path/to/directory") ;; => "path/to/directory"
```

### f-base `(path)`

{{f-base}}

```lisp
(f-base "path/to/file.ext") ;; => "file"
(f-base "path/to/directory") ;; => nil
```

### f-relative `(path &optional dir)`

{{f-relative}}

```lisp
(f-relative "/some/path/relative/to/my/file.txt" "/some/path/") ;; => relative/to/my/file.txt
(f-relative "/default/directory/my/file.txt") ;; => my/file.txt
```

### f-short `(path)`

{{f-short}}

Alias: `f-abbrev`

```lisp
(f-short "/Users/foo/Code/bar") ;; => ~/Code/bar
(f-short "/path/to/Code/bar") ;; => /path/to/Code/bar
```

### f-long `(path)`

{{f-long}}

```lisp
(f-long "~/Code/bar") ;; => /Users/foo/Code/bar
(f-long "/path/to/Code/bar") ;; => /path/to/Code/bar
```

### f-canonical `(path)`

{{f-canonical}}

```lisp
(f-canonical "/path/to/real/file") ;; => /path/to/real/file
(f-canonical "/link/to/file") ;; => /path/to/real/file
```

### f-slash `(path)`

{{f-slash}}

```lisp
(f-slash "/path/to/file") ;; => /path/to/file
(f-slash "/path/to/dir") ;; => /path/to/dir/
(f-slash "/path/to/dir/") ;; => /path/to/dir/
```

### f-full `(path)`

{{f-full}}

```lisp
(f-slash "~/path/to/file") ;; => /home/path/to/file
(f-slash "~/path/to/dir") ;; => /home/path/to/dir/
(f-slash "~/path/to/dir/") ;; => /home/path/to/dir/
```

### f-read-bytes `(path)`

{{f-write-bytes}}

```lisp
(f-read-bytes "path/to/binary/data")
```

### f-write-bytes `(path)`

{{{f-write-bytes}}}

```lisp
(f-write-bytes "path/to/binary/data" (unibyte-string 72 101 108 108 111 32 119 111 114 108 100))
```

### f-read-text `(path &optional coding)`

{{f-read-text}}

```lisp
(f-read-text "path/to/file.txt" 'utf-8)
```

### f-write-text `(text coding path)`

{{f-write-text}}

```lisp
(f-write-text "Hello world" 'utf-8 "path/to/file.txt")
```

### f-mkdir `(&rest dirs)`

{{f-mkdir}}

```lisp
(f-mkdir "dir") ;; => /default/directory/dir
(f-mkdir "other" "dir") ;; => /default/directory/other/dir
```

### f-delete `(path &optional force)`

{{f-delete}}

```lisp
(f-delete "dir")
(f-delete "other/dir" t)
(f-delete "path/to/file.txt")
```

### f-symlink `(source path)`

{{f-symlink}}

```lisp
(f-symlink "path/to/source" "path/to/link")
```

### f-move `(from to)`

{{f-move}}

```lisp
(f-move "path/to/file.txt" "new-file.txt")
(f-move "path/to/file.txt" "other/path")
```

### f-copy `(from to)`

{{f-copy}}

```lisp
(f-copy "path/to/file.txt" "new-file.txt")
(f-copy "path/to/dir" "other/dir")
```

### f-touch `(path)`

{{f-touch}}

```lisp
(f-touch "path/to/existing/file.txt")
(f-touch "path/to/non/existing/file.txt")
```

### f-exists? `(path)`

{{f-exists?}}

```lisp
(f-exists? "path/to/file.txt")
(f-exists? "path/to/dir")
```

### f-directory? `(path)`

{{f-directory?}}

Alias: `f-dir?`

```lisp
(f-directory? "path/to/file.txt") ;; => nil
(f-directory? "path/to/dir") ;; => t
```

### f-file? `(path)`

{{f-file?}}

```lisp
(f-directory? "path/to/file.txt") ;; => t
(f-directory? "path/to/dir") ;; => nil
```

### f-symlink? `(path)`

{{f-symlink?}}

```lisp
(f-symlink? "path/to/file.txt") ;; => nil
(f-symlink? "path/to/dir") ;; => nil
(f-symlink? "path/to/link") ;; => t
```

### f-readable? `(path)`

{{f-readable?}}

```lisp
(f-readable? "path/to/file.txt")
(f-readable? "path/to/dir")
```

### f-writable? `(path)`

{{f-writable?}}

```lisp
(f-writable? "path/to/file.txt")
(f-writable? "path/to/dir")
```

### f-executable? `(path)`

{{f-executable?}}

```lisp
(f-executable? "path/to/file.txt")
(f-executable? "path/to/dir")
```

### f-absolute? `(path)`

{{f-absolute?}}

```lisp
(f-absolute? "path/to/dir") ;; => nil
(f-absolute? "/full/path/to/dir") ;; => t
```

### f-relative? `(path)`

{{f-relative?}}

```lisp
(f-relative? "path/to/dir") ;; => t
(f-relative? "/full/path/to/dir") ;; => nil
```

### f-root? `(path)`

{{f-root?}}

```lisp
(f-root? "/") ;; => t
(f-root? "/not/root") ;; => nil
```

### f-ext? `(path ext)`

{{f-ext?}}

```lisp
(f-ext? "path/to/file.el" "el") ;; => t
(f-ext? "path/to/file.el" "txt") ;; => nil
(f-ext? "path/to/file.el") ;; => t
(f-ext? "path/to/file") ;; => nil
```

### f-same? `(path-a path-b)`

{{f-same?}}

Alias: `f-equal?`

```lisp
(f-same? "foo.txt" "foo.txt") ;; => t
(f-same? "/path/to/foo.txt" "/path/to/bar.txt") ;; => nil
```

### f-size `(path)`

{{f-size}}

```lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
```

### f-this-file `()`

{{f-this-file}}

```lisp
(f-this-file) ;; => /path/to/this/file
```

### f-path-separator `()`

{{f-path-separator}}

```lisp
(f-path-separator) ;; => /
```

### f-glob `(pattern &optional path)`

{{f-glob}}

See: `file-expand-wildcards`

```lisp
(f-glob "path/to/*.el")
(f-glob "*.el" "path/to")
```

### f-entries `(path &optional fn recursive)`

{{f-entries}}

```lisp
(f-entries "path/to/dir")
(f-entries "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-entries "path/to/dir" nil t)
```

### f-directories `(path &optional fn recursive)`

{{f-directories}}

```lisp
(f-directories "path/to/dir")
(f-directories "path/to/dir" (lambda (dir) ((f-filename dir) "test")))
(f-directories "path/to/dir" nil t)
```

### f-files `(path &optional fn recursive)`

{{f-files}}

```lisp
(f-files "path/to/dir")
(f-files "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-files "path/to/dir" nil t)
```

### f-root `()`

{{f-root}}

```lisp
(f-root) ;; => "/"
```

## Changelog

### v0.10.0

* Add `f-root`.
* Fix `f-root?` bug for weird syntax

### v0.9.0

* Make `s-long`
* Make `s-short` default and `f-abbrev` the alias
* Add `f-full`
* Do not append path separator if file in `f-slash`
* Fixed bug in `f-path-separator`

### v0.8.0

* Moved `f-this-file` to misc section
* Add `f-slash`
* Add `f-path-separator`

### v0.7.1

* Fix coding bug in `f-read-text`

### v0.7.0

* Add `f-touch`

### v0.6.1

* Fix `f-write-text` for unibyte strings

### v0.6.0

* Add `f-write-text` and `f-write-bytes` and deprecate `f-write`
* Add `f-read-text` and `f-read-bytes` and deprecate `f-read`
* Add `f-this-file`
* Add `f-canonical`
* Fix `f-same?` for symlinks

### v0.5.0

* Add `f-same?` (alias `f-equal?`)

### v0.4.1

* Bump `s` and `dash` versions

### v0.4.0

* Add `f-copy`

### v0.3.0

* Add `f-ext?`

### v0.2.1

* Fix `f-filename` when ending with slash

### v0.2.0

* Add `f-root?`
* Fix `f-dirname` when ending with slash

### v0.1.0

* Add `f-abbrev` (alias `f-short`)

### v0.0.2

* `f-join` platform independent

### v0.0.1

## Example

Here's an example of a function that finds the Git project root.

### Using standard Emacs builtin functions

```lisp
(defun find-git-root (&optional dir)
  (unless dir (setq dir (expand-file-name (file-name-directory (buffer-file-name)))))
  (let ((parent (expand-file-name ".." dir)))
    (unless (equal parent dir)
      (if (file-exists-p (expand-file-name ".git" dir))
          dir
        (find-git-root parent)))))
```

### Using `f.el`

```lisp
(defun find-git-root (&optional dir)
  (interactive)
  (unless dir (setq dir (f-dirname (buffer-file-name))))
  (let ((parent (f-parent dir)))
    (unless (f-root? parent)
      (if (f-exists? (f-expand ".git" dir))
          dir
        (find-git-root parent)))))
```

## Contribution

Be sure to!

Install [Cask](https://github.com/rejeep/cask.el) if you haven't
already.

Run the unit tests with:

    $ make test

Do not change `README.md` directly. If you want to change the README
or if you change any function comments, update the README with:

    $ make docs
