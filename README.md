# f.el [![Build Status](https://api.travis-ci.org/rejeep/f.el.png?branch=master)](http://travis-ci.org/rejeep/f.el)

Much inspired by [@magnars](https://github.com/magnars)'s excellent
[s.el](https://github.com/magnars/s.el) and
[dash.el](https://github.com/magnars/dash.el),
[f.el](https://github.com/rejeep/f.el) is a modern API for working
with files and directories in Emacs.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](http://melpa.milkbox.net/):

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
* [f-abbrev](#f-abbrev-path) `(path)`

### Destructive

* [f-write](#f-write-path-optional-content) `(path &optional content)`
* [f-mkdir](#f-mkdir-rest-dirs) `(&rest dirs)`
* [f-delete](#f-delete-path-optional-force) `(path &optional force)`
* [f-symlink](#f-symlink-source-path) `(source path)`
* [f-move](#f-move-from-to) `(from to)`

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

### Stats

* [f-size](#f-size-path) `(path)`

### Misc

* [f-read](#f-read-path) `(path)`
* [f-glob](#f-glob-pattern-optional-path) `(pattern &optional path)`
* [f-entries](#f-entries-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-directories](#f-directories-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-files](#f-files-path-optional-fn-recursive) `(path &optional fn recursive)`

## Documentation and examples

### f-join `(&rest args)`

Join ARGS to a single path.

```lisp
(f-join "path") ;; => "path"
(f-join "path" "to") ;; => "path/to"
(f-join "path" "to" "heaven") ;; => "path/to/heaven"
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

### f-abbrev `(path)`

Abbrev PATH. See `abbreviate-file-name'.

Alias: `f-short`

```lisp
(f-abbrev "/Users/foo/Code/bar") ;; => ~/Code/bar
(f-abbrev "/path/to/Code/bar") ;; => /path/to/Code/bar
```

### f-write `(path &optional content)`

Write CONTENT or nothing to PATH. If no content, just create file.

```lisp
(f-write "path/to/file.txt")
(f-write "path/to/file.txt" "some-content")
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

### f-size `(path)`

Return size of PATH.

If PATH is a file, return size of that file. If PATH is
directory, return sum of all files in PATH.

```lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
```

### f-read `(path)`

Return content of PATH.

```lisp
(f-read "path/to/file.txt")
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
(f-entries "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-entries "path/to/dir" nil t)
```

### f-directories `(path &optional fn recursive)`

Find all directories in PATH. See `f-entries`.

```lisp
(f-directories "path/to/dir")
(f-directories "path/to/dir" (lambda (dir) ((f-filename dir) "test")))
(f-directories "path/to/dir" nil t)
```

### f-files `(path &optional fn recursive)`

Find all files in PATH. See `f-entries`.

```lisp
(f-files "path/to/dir")
(f-files "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-files "path/to/dir" nil t)
```

## Contribution

Be sure to!

Install [Carton](https://github.com/rejeep/carton) if you haven't
already.

Run the unit tests with:

    $ make test

Do not change `README.md` directly. If you want to change the README
or if you change any function comments, update the README with:

    $ make docs
