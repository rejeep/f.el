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
* [f-split](#f-split-path) `(path)`
* [f-expand](#f-expand-path-optional-dir) `(path &optional dir)`
* [f-filename](#f-filename-path) `(path)`
* [f-dirname](#f-dirname-path) `(path)`
* [f-common-parent](#f-common-parent-paths) `(paths)`
* [f-ext](#f-ext-path) `(path)`
* [f-no-ext](#f-no-ext-path) `(path)`
* [f-base](#f-base-path) `(path)`
* [f-relative](#f-relative-path-optional-file) `(path &optional file)`
* [f-short](#f-short-path) `(path)`
* [f-long](#f-long-path) `(path)`
* [f-canonical](#f-canonical-path) `(path)`
* [f-slash](#f-slash-path) `(path)`
* [f-full](#f-full-path) `(path)`
* [f-uniquify](#f-uniquify-paths) `(paths)`
* [f-uniquify-alist](#f-uniquify-alist-paths) `(paths)`

### I/O

* [f-read-bytes](#f-read-bytes-path) `(path)`
* [f-write-bytes](#f-write-bytes-data-path) `(data path)`
* [f-read-text](#f-read-text-path-optional-coding) `(path &optional coding)`
* [f-write-text](#f-write-text-text-coding-path)`(text coding path)`
* [f-append-text](#f-append-text-text-coding-path)`(text coding path)`
* [f-append-bytes](#f-append-data-path)`(text coding path)`

### Destructive

* [f-mkdir](#f-mkdir-rest-dirs) `(&rest dirs)`
* [f-delete](#f-delete-path-optional-force) `(path &optional force)`
* [f-symlink](#f-symlink-source-path) `(source path)`
* [f-move](#f-move-from-to) `(from to)`
* [f-copy](#f-copy-from-to) `(from to)`
* [f-copy-contenst](#f-copy-contents-from-to) `(from to)`
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
* [f-parent-of?](#f-parent-of-path-a-path-b) `(path-a path-b)`
* [f-child-of?](#f-child-of-path-a-path-b) `(path-a path-b)`
* [f-ancestor-of?](#f-ancestor-of-path-a-path-b) `(path-a path-b)`
* [f-descendant-of?](#f-descendant-of-path-a-path-b) `(path-a path-b)`
* [f-hidden?](#f-hidden-path) `(path)`

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
* [f-up](#f-up-fn-optional-dir) `(fn &optional dir)`
* [f-traverse-upwards](#f-traverse-upwards-fn-optional-path) `(fn &optional path)`
* [f-with-sandbox](#f-with-sandbox-path-or-paths-rest-body) `(path-or-paths &rest body)`

## Documentation and examples

### f-join `(&rest args)`

Join ARGS to a single path.

```lisp
(f-join "path") ;; => "path"
(f-join "path" "to") ;; => "path/to"
(f-join "/" "path" "to" "heaven") ;; => "/path/to/heaven"
```

### f-split `(path)`

Split PATH and return list containing parts.

```lisp
(f-split "path") ;; => '("path")
(f-split "path/to") ;; => '("path" "to")
(f-split "/path/to/heaven") ;; => '("/" "path" "to" "heaven")
```

### f-expand `(path &optional dir)`

Expand PATH relative to DIR (or `default-directory').

```lisp
(f-expand "name") ;; => "/default/directory/name"
(f-expand "name" "other/directory") ;; => "other/directory/name"
```

### f-filename `(path)`

Return the name of PATH.

```lisp
(f-filename "path/to/file.ext") ;; => "file.ext"
(f-filename "path/to/directory") ;; => "directory"
```

### f-dirname `(path)`

Return the parent directory to PATH.

Alias: `f-parent`

```lisp
(f-dirname "path/to/file.ext") ;; => "path/to"
(f-dirname "path/to/directory") ;; => "path/to"
(f-dirname "/") ;; => nil
```

### f-common-parent `(paths)`

Return the deepest common parent directory of PATHS.

```lisp
(f-common-parent '("foo/bar/baz" "foo/bar/qux" "foo/bar/mux")) ;; => "foo/bar/"
(f-common-parent '("/foo/bar/baz" "/foo/bar/qux" "/foo/bax/mux")) ;; => "/foo/"
(f-common-parent '("foo/bar/baz" "quack/bar/qux" "lack/bar/mux")) ;; => ""
```

### f-ext `(path)`

Return the file extension of PATH.

```lisp
(f-ext "path/to/file.ext") ;; => "ext"
(f-ext "path/to/directory") ;; => nil
```

### f-no-ext `(path)`

Return everything but the file extension of PATH.

```lisp
(f-no-ext "path/to/file.ext") ;; => "path/to/file"
(f-no-ext "path/to/directory") ;; => "path/to/directory"
```

### f-base `(path)`

Return the name of PATH, excluding the extension if file.

```lisp
(f-base "path/to/file.ext") ;; => "file"
(f-base "path/to/directory") ;; => nil
```

### f-relative `(path &optional dir)`

Return PATH relative to DIR.

```lisp
(f-relative "/some/path/relative/to/my/file.txt" "/some/path/") ;; => relative/to/my/file.txt
(f-relative "/default/directory/my/file.txt") ;; => my/file.txt
```

### f-short `(path)`

Return abbrev of PATH. See `abbreviate-file-name'.

Alias: `f-abbrev`

```lisp
(f-short "/Users/foo/Code/bar") ;; => ~/Code/bar
(f-short "/path/to/Code/bar") ;; => /path/to/Code/bar
```

### f-long `(path)`

Return long version of PATH.

```lisp
(f-long "~/Code/bar") ;; => /Users/foo/Code/bar
(f-long "/path/to/Code/bar") ;; => /path/to/Code/bar
```

### f-canonical `(path)`

Return the canonical name of PATH.

```lisp
(f-canonical "/path/to/real/file") ;; => /path/to/real/file
(f-canonical "/link/to/file") ;; => /path/to/real/file
```

### f-slash `(path)`

Append slash to PATH unless one already.

Some functions, such as `call-process' requires there to be an
ending slash.

```lisp
(f-slash "/path/to/file") ;; => /path/to/file
(f-slash "/path/to/dir") ;; => /path/to/dir/
(f-slash "/path/to/dir/") ;; => /path/to/dir/
```

### f-full `(path)`

Return absolute path to PATH, with ending slash.

```lisp
(f-full "~/path/to/file") ;; => /home/path/to/file
(f-full "~/path/to/dir") ;; => /home/path/to/dir/
(f-full "~/path/to/dir/") ;; => /home/path/to/dir/
```

### f-uniquify `(paths)`

Return unique suffixes of PATHS.

This function expects no duplicate paths.

```lisp
(f-uniquify '("/foo/bar" "/foo/baz" "/foo/quux")) ;; => '("bar" "baz" "quux")
(f-uniquify '("/foo/bar" "/www/bar" "/foo/quux")) ;; => '("foo/bar" "www/bar" "quux")
(f-uniquify '("/foo/bar" "/www/bar" "/www/bar/quux")) ;; => '("foo/bar" "www/bar" "quux")
(f-uniquify '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")) ;; => '("foo/bar" "www/bar" "foo/baz" "home/www/baz" "foo/www/baz" "foo")
```

### f-uniquify-alist `(paths)`

Return alist mapping PATHS to unique suffixes of PATHS.

This function expects no duplicate paths.

```lisp
(f-uniquify-alist '("/foo/bar" "/foo/baz" "/foo/quux")) ;; => '(("/foo/bar" . "bar") ("/foo/baz" . "baz") ("/foo/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/www/bar" "/foo/quux")) ;; => '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/foo/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/www/bar" "/www/bar/quux")) ;; => '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/www/bar/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")) ;; => '(("/foo/bar" . "foo/bar") ("/home/www/bar" . "www/bar") ("/foo/baz" . "foo/baz") ("/home/www/baz" . "home/www/baz") ("/opt/foo/www/baz" . "foo/www/baz") ("/var/foo" . "foo"))
```

### f-read-bytes `(path)`

Read binary data from PATH.

Return the binary data as unibyte string.

```lisp
(f-read-bytes "path/to/binary/data")
```

### f-write-bytes `(data path)`

Write binary DATA to PATH.

DATA is a unibyte string.  PATH is a file name to write to.

```lisp
(f-write-bytes (unibyte-string 72 101 108 108 111 32 119 111 114 108 100) "path/to/binary/data")
```

### f-read-text `(path &optional coding)`

Read text with PATH, using CODING.

CODING defaults to `utf-8'.

Return the decoded text as multibyte string.

Alias: `f-read`

```lisp
(f-read-text "path/to/file.txt" 'utf-8)
(f-read "path/to/file.txt" 'utf-8)
```

### f-write-text `(text coding path)`

Write TEXT with CODING to PATH.

TEXT is a multibyte string.  CODING is a coding system to encode
TEXT with.  PATH is a file name to write to.

Alias: `f-write`

```lisp
(f-write-text "Hello world" 'utf-8 "path/to/file.txt")
(f-write "Hello world" 'utf-8 "path/to/file.txt")
```

### f-append-text `(text coding path)`

Append TEXT with CODING to PATH.

If PATH does not exist, it is created.

Alias: `f-append`

```lisp
(f-append-text "Hello world" 'utf-8 "path/to/file.txt")
(f-append "Hello world" 'utf-8 "path/to/file.txt")
```

### f-append-bytes `(data path)`

Append binary DATA to PATH.

If PATH does not exist, it is created.

```lisp
(f-append-bytes "path/to/file" (unibyte-string 72 101 108 108 111 32 119 111 114 108 100))
```

### f-mkdir `(&rest dirs)`

Create directories DIRS.

```lisp
(f-mkdir "dir") ;; => /default/directory/dir
(f-mkdir "other" "dir") ;; => /default/directory/other/dir
```

### f-delete `(path &optional force)`

Delete PATH, which can be file or directory.

If FORCE is t, a directory will be deleted recursively.

```lisp
(f-delete "dir")
(f-delete "other/dir" t)
(f-delete "path/to/file.txt")
```

### f-symlink `(source path)`

Create a symlink to `source` from `path`.

```lisp
(f-symlink "path/to/source" "path/to/link")
```

### f-move `(from to)`

Move or rename FROM to TO.

```lisp
(f-move "path/to/file.txt" "new-file.txt")
(f-move "path/to/file.txt" "other/path")
```

### f-copy `(from to)`

Copy file or directory FROM to TO.

```lisp
(f-copy "path/to/file.txt" "new-file.txt")
(f-copy "path/to/dir" "other/dir")
```

### f-copy-contents `(from to)`

Copy contents in directory FROM, to directory TO.

```lisp
(f-copy-contents "path/to/dir" "path/to/other/dir")
```

### f-touch `(path)`

Update PATH last modification date or create if it does not exist.

```lisp
(f-touch "path/to/existing/file.txt")
(f-touch "path/to/non/existing/file.txt")
```

### f-exists? `(path)`

Return t if PATH exists, false otherwise.

```lisp
(f-exists? "path/to/file.txt")
(f-exists? "path/to/dir")
```

### f-directory? `(path)`

Return t if PATH is directory, false otherwise.

Alias: `f-dir?`

```lisp
(f-directory? "path/to/file.txt") ;; => nil
(f-directory? "path/to/dir") ;; => t
```

### f-file? `(path)`

Return t if PATH is file, false otherwise.

```lisp
(f-directory? "path/to/file.txt") ;; => t
(f-directory? "path/to/dir") ;; => nil
```

### f-symlink? `(path)`

Return t if PATH is symlink, false otherwise.

```lisp
(f-symlink? "path/to/file.txt") ;; => nil
(f-symlink? "path/to/dir") ;; => nil
(f-symlink? "path/to/link") ;; => t
```

### f-readable? `(path)`

Return t if PATH is readable, false otherwise.

```lisp
(f-readable? "path/to/file.txt")
(f-readable? "path/to/dir")
```

### f-writable? `(path)`

Return t if PATH is writable, false otherwise.

```lisp
(f-writable? "path/to/file.txt")
(f-writable? "path/to/dir")
```

### f-executable? `(path)`

Return t if PATH is executable, false otherwise.

```lisp
(f-executable? "path/to/file.txt")
(f-executable? "path/to/dir")
```

### f-absolute? `(path)`

Return t if PATH is absolute, false otherwise.

```lisp
(f-absolute? "path/to/dir") ;; => nil
(f-absolute? "/full/path/to/dir") ;; => t
```

### f-relative? `(path)`

Return t if PATH is relative, false otherwise.

```lisp
(f-relative? "path/to/dir") ;; => t
(f-relative? "/full/path/to/dir") ;; => nil
```

### f-root? `(path)`

Return t if PATH is root directory, false otherwise.

```lisp
(f-root? "/") ;; => t
(f-root? "/not/root") ;; => nil
```

### f-ext? `(path ext)`

Return t if extension of PATH is EXT, false otherwise.

If EXT is nil or omitted, return t if PATH has any extension,
false otherwise.

```lisp
(f-ext? "path/to/file.el" "el") ;; => t
(f-ext? "path/to/file.el" "txt") ;; => nil
(f-ext? "path/to/file.el") ;; => t
(f-ext? "path/to/file") ;; => nil
```

### f-same? `(path-a path-b)`

Return t if PATH-A and PATH-b are references to same file.

Alias: `f-equal?`

```lisp
(f-same? "foo.txt" "foo.txt") ;; => t
(f-same? "/path/to/foo.txt" "/path/to/bar.txt") ;; => nil
```

### f-parent-of? `(path-a path-b)`

Return t if PATH-A is parent of PATH-B.

```lisp
(f-parent-of? "/path/to" "/path/to/dir") ;; => t
(f-parent-of? "/path/to/dir" "/path/to") ;; => nil
(f-parent-of? "/path/to" "/path/to") ;; => nil
```

### f-child-of? `(path-a path-b)`

Return t if PATH-A is child of PATH-B.

```lisp
(f-child-of? "/path/to" "/path/to/dir") ;; => nil
(f-child-of? "/path/to/dir" "/path/to") ;; => t
(f-child-of? "/path/to" "/path/to") ;; => nil
```

### f-ancestor-of? `(path-a path-b)`

Return t if PATH-A is ancestor of PATH-B.

```lisp
(f-ancestor-of? "/path/to" "/path/to/dir") ;; => t
(f-ancestor-of? "/path" "/path/to/dir") ;; => t
(f-ancestor-of? "/path/to/dir" "/path/to") ;; => nil
(f-ancestor-of? "/path/to" "/path/to") ;; => nil
```

### f-descendant-of? `(path-a path-b)`

Return t if PATH-A is desendant of PATH-B.

```lisp
(f-descendant-of? "/path/to/dir" "/path/to") ;; => t
(f-descendant-of? "/path/to/dir" "/path") ;; => t
(f-descendant-of? "/path/to" "/path/to/dir") ;; => nil
(f-descendant-of? "/path/to" "/path/to") ;; => nil
```

### f-hidden? `(path)`

Return t if PATH is hidden, nil otherwise.

```lisp
(f-hidden? "/path/to/foo") ;; => nil
(f-hidden? "/path/to/.foo") ;; => t
```

### f-size `(path)`

Return size of PATH.

If PATH is a file, return size of that file. If PATH is
directory, return sum of all files in PATH.

```lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
```

### f-this-file `()`

Return path to this file.

```lisp
(f-this-file) ;; => /path/to/this/file
```

### f-path-separator `()`

Return path separator.

```lisp
(f-path-separator) ;; => /
```

### f-glob `(pattern &optional path)`

Find PATTERN in PATH.

See: `file-expand-wildcards`

```lisp
(f-glob "path/to/*.el")
(f-glob "*.el" "path/to")
```

### f-entries `(path &optional fn recursive)`

Find all files and directories in PATH.

FN - called for each found file and directory. If FN returns a thruthy
value, file or directory will be included.
RECURSIVE - Search for files and directories recursive.

```lisp
(f-entries "path/to/dir")
(f-entries "path/to/dir" (lambda (file) (s-matches? "test" file)))
(f-entries "path/to/dir" nil t)
(f--entries "path/to/dir" (s-matches? "test" it))
```

### f-directories `(path &optional fn recursive)`

Find all directories in PATH. See `f-entries`.

```lisp
(f-directories "path/to/dir")
(f-directories "path/to/dir" (lambda (dir) (equal (f-filename dir) "test")))
(f-directories "path/to/dir" nil t)
(f--directories "path/to/dir" (equal (f-filename it) "test"))
```

### f-files `(path &optional fn recursive)`

Find all files in PATH. See `f-entries`.

```lisp
(f-files "path/to/dir")
(f-files "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-files "path/to/dir" nil t)
(f--files "path/to/dir" (equal (f-ext it) "el"))
```

### f-root `()`

Return absolute root.

```lisp
(f-root) ;; => "/"
```

### f-up `(fn &optional dir)`

Traverse up as long as FN returns nil, starting at DIR.

Deprecated in favor of: [f-traverse-upwards](#f-traverse-upwards-fn-optional-path)

```lisp
(f-up
 (lambda (path)
   (f-exists? ".git" path))
 start-dir)
(f--up (f-exists? ".git" it) start-dir) ;; same as above
```

### f-traverse-upwards `(fn &optional path)`

Traverse up as long as FN returns nil, starting at PATH.

If FN returns a non-nil value, the path sent as argument to FN is
returned. If no function callback return a non-nil value, nil is
returned.

```lisp
(f-traverse-upwards
 (lambda (path)
   (f-exists? ".git" path))
 start-path)
(f--traverse-upwards (f-exists? ".git" it) start-path) ;; same as above
```

### f-with-sandbox `(path-or-paths &rest body)`

Only allow PATH-OR-PATHS and decendants to be modified in BODY.

```lisp
(f-with-sandbox foo-path
  (f-touch (f-expand "foo" foo-path)))
(f-with-sandbox (list foo-path bar-path)
  (f-touch (f-expand "foo" foo-path))
  (f-touch (f-expand "bar" bar-path)))
(f-with-sandbox foo-path
  (f-touch (f-expand "bar" bar-path))) ;; "Destructive operation outside sandbox"
```

## Changelog

### v0.17.0

* Add `f-common-parent` (by @Fuco1)

### v0.16.0

* Add `f-with-sandbox`

### v0.15.0

* Add `f-split`

### v0.14.0

* Add `f-traverse-upwards` and its anaphoric version `f--traverse-upwards`
* Deprecate `f-up` and its anaphoric version `f--up`

### v0.13.0

* Add `f-uniquify` and `f-uniquify-alist` (by @Fuco1)

### v0.12.0

* `f-parent` returns nil if argument is root

### v0.11.0

* Add `f-descendant-of?`
* Add `f-ancestor-of?`
* Add `f-parent-of?`
* Add `f-child-of?`
* Remove deprecation for `f-read` and `f-write` and make them aliases
  to `f-read-text` and `f-write-text` respectively
* Add anaphoric function `f--entries` for `f-entries`
* Add anaphoric function `f--files` for `f-files`
* Add anaphoric function `f--directories` of `f-directories`
* Add `f-up` and anaphoric version `f--up`

### v0.10.0

* Add `f-root`
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

Now, try writing it even simpler yourself. Hint, check out `f-traverse-upwards`.

## Contribution

Be sure to!

Install [Cask](https://github.com/rejeep/cask.el) if you haven't
already.

Run the unit tests with:

    $ make test

Do not change `README.md` directly. If you want to change the README
or if you change any function comments, update the README with:

    $ make docs
