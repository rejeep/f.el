# f.el [![Build Status](https://api.travis-ci.org/rejeep/f.el.png?branch=master)](http://travis-ci.org/rejeep/f.el)

Much inspired by [@magnars](https://github.com/magnars)'s excellent
[s.el](https://github.com/magnars/s.el) and
[dash.el](https://github.com/magnars/dash.el),
[f.el](https://github.com/rejeep/f.el) is a library for working with
files and directories.

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
* [f-parent](#f-parent-path) `(path)`

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

### f-write `(path &optional content)`

{{f-write}}

```lisp
(f-write "path/to/file.txt")
(f-write "path/to/file.txt" "some-content")
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

### f-size `(path)`

{{f-size}}

```lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
```

### f-read `(path)`

{{f-read}}

```lisp
(f-read "path/to/file.txt")
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

## Contribution

Be sure to!

Install [Carton](https://github.com/rejeep/carton) if you haven't
already.

Run the unit tests with:

    $ make test

Do not change `README.md` directly. If you want to change the README
or if you change any function comments, update the README with:

    $ make docs
