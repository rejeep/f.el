# f.el [![Build Status](https://api.travis-ci.org/rejeep/f.el.png?branch=master)](http://travis-ci.org/rejeep/f.el)

## API

### Paths

* [f-join](#f-join-rest-args) `(&rest args)`
* [f-expand](#f-expand-file-rest-dirs) `(file &rest dirs)`
* [f-filename](#f-filename-path) `(path)`
* [f-dirname](#f-dirname-path) `(path)`
* [f-ext](#f-ext-path) `(path)`
* [f-no-ext](#f-no-ext-path) `(path)`
* [f-base](#f-base-path) `(path)`
* [f-relative](#f-relative-file-optional-path) `(file &optional path)`

### Destructive

* [f-write](#f-write-path-optional-content) `(path &optional content)`
* [f-mkdir](#f-mkdir-rest-dirs) `(&rest dirs)`
* [f-delete](#f-delete-path-optional-force) `(path &optional force)`
* [f-symlink](#f-symlink-source-path) `(source path)`
* [f-move](#f-move-from-to) `(from to)`
* [f-chmod](#f-chmod-path-mode) `(path mode)`
* [f-chown](#f-chown-path-user-group-optional-recursive) `(path user group &optional recursive)`

### Predicates

* [f-exists?](#f-exists-path) `(path)`
* [f-directory?](#f-directory-path) `(path)`
* [f-file?](#f-file-path) `(path)`
* [f-symlink?](#f-symlink-path) `(path)`
* [f-readable?](#f-readable-path) `(path)`
* [f-writable?](#f-writable-path) `(path)`
* [f-executable?](#f-executable-path) `(path)`

### Stats

* [f-size](#f-size-path) `(path)`
* [f-last-change](#f-last-change-path) `(path)`
* [f-last-access](#f-last-access-path) `(path)`
* f-uid
* f-user
* f-gid
* f-group
* f-inode

### Misc

* [f-read](#f-read-path) `(path)`
* [f-glob](#f-glob-pattern-optional-path) `(pattern &optional path)`
* [f-entries](#f-entries-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-directories](#f-directories-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-files](#f-files-path-optional-fn-recursive) `(path &optional fn recursive)`

## Documentation and examples

### f-join `(&rest args)`

Join list of files and directories to a single path.

```lisp
(f-join "path") ;; => "path"
(f-join "path" "to") ;; => "path/to"
(f-join "path" "to" "heaven") ;; => "path/to/heaven"
```

### f-expand `(file &rest dirs)`

Expands `file` relative to `dirs`.

```lisp
(f-expand "name") ;; => "default/directory/name"
(f-expand "name" "other/directory") ;; => "other/directory/name"
(f-expand "name" "other" "directory") ;; => "other/directory/name"
```

### f-filename `(path)`

Returns the name of `path`, which can be a file or directory.

```lisp
(f-filename "path/to/file.ext") ;; => "file.ext"
(f-filename "path/to/directory") ;; => "directory"
```

### f-dirname `(path)`

Returns the parent directory to `path`.

```lisp
(f-dirname "path/to/file.ext") ;; => "path/to"
(f-dirname "path/to/directory") ;; => "path/to"
```

### f-ext `(path)`

Returns the file extension of `path`.

```lisp
(f-ext "path/to/file.ext") ;; => "ext"
(f-ext "path/to/directory") ;; => nil
```

### f-no-ext `(path)`

Returns everything but the file extension of `path`.

```lisp
(f-no-ext "path/to/file.ext") ;; => "path/to/file"
(f-no-ext "path/to/directory") ;; => "path/to/directory"
```

### f-base `(path)`

Returns the name of the `path`, excluding the extension if file.

```lisp
(f-base "path/to/file.ext") ;; => "file"
(f-base "path/to/directory") ;; => nil
```

### f-relative `(file &optional path)`

Returns the path to `file` relative to `path`.

```lisp
(f-relative "/some/path/relative/to/my/file.txt" "/some/path/") ;; => relative/to/my/file.txt
(f-relative "/default/directory/my/file.txt") ;; => my/file.txt
```

### f-write `(path &optional content)`

Writes the `content` or nothing to `path`.

```lisp
(f-write "path/to/file.txt")
(f-write "path/to/file.txt" "some-content")
```

### f-mkdir `(&rest dirs)`

Creates directory.

```lisp
(f-mkdir "dir") ;; => /default/directory/dir
(f-mkdir "other" "dir") ;; => /default/directory/other/dir
```

### f-delete `(path &optional force)`

Deletes directory or file `path`. Use `force` to delete a directory
recursive.

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

Move or rename `from` to `to`.

```lisp
(f-move "path/to/file.txt" "new-file.txt")
(f-move "path/to/file.txt" "other/path")
```

### f-chmod `(path mode)`

Change mode permissions of `path`. Use `recursive` to apply to
directory recursively.

```lisp
(f-chmod "path/to/file.txt" #o644)
(f-chmod "path/to/dir" #o755)
```

### f-chown `(path user group &optional recursive)`

Change user or/and group of `path`. Use `recursive` to apply to
directory recursively.

```lisp
(f-chown "path/to/file.txt" "user" "group")
(f-chown "path/to/file.txt" nil "group")
(f-chown "path/to/dir" "user" "group" t)
```

### f-exists? `(path)`

Returns true if `path` exists, false otherwise.

```lisp
(f-exists? "path/to/file.txt")
(f-exists? "path/to/dir")
```

### f-directory? `(path)`

Alias: `f-dir?`

Returns true if `path` is a directory, false otherwise.

```lisp
(f-directory? "path/to/file.txt") ;; => nil
(f-directory? "path/to/dir") ;; => t
```

### f-file? `(path)`

Returns true if `path` is a regular file, false otherwise.

```lisp
(f-directory? "path/to/file.txt") ;; => t
(f-directory? "path/to/dir") ;; => nil
```

### f-symlink? `(path)`

Returns true if `path` is a symlink, false otherwise.

```lisp
(f-symlink? "path/to/file.txt") ;; => nil
(f-symlink? "path/to/dir") ;; => nil
(f-symlink? "path/to/link") ;; => t
```

### f-readable? `(path)`

Returns true if `path` is readable, false otherwise.

```lisp
(f-readable? "path/to/file.txt")
(f-readable? "path/to/dir")
```

### f-writable? `(path)`

Returns true if `path` is writable, false otherwise.

```lisp
(f-writable? "path/to/file.txt")
(f-writable? "path/to/dir")
```

### f-executable? `(path)`

Returns true if `path` is executable, false otherwise.

```lisp
(f-executable? "path/to/file.txt")
(f-executable? "path/to/dir")
```

### f-size `(path)`

Returns the size of `path`. If file, return its size. If directory,
return total size of all files in it.

```lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
```

### f-last-change `(path)`

Return timestamp of when `path` was last changed.

```lisp
(f-last-change "path/to/file.txt")
(f-last-change "path/to/dir")
```

### f-last-access `(path)`

Return timestamp of when `path` was last accessed.

```lisp
(f-last-access "path/to/file.txt")
(f-last-access "path/to/dir")
```

### f-read `(path)`

Reads the content of `path`.

```lisp
(f-read "path/to/file.txt")
```

### f-glob `(pattern &optional path)`

See: `file-expand-wildcards`

```lisp
(f-glob "path/to/*.el")
(f-glob "*.el" "path/to")
```

### f-entries `(path &optional fn recursive)`

Return all files and directories in `path`. If `fn` is specified,
return only those who returns a true value. If `recursive`, return all
recursively.

```lisp
(f-entries "path/to/dir")
(f-entries "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-entries "path/to/dir" nil t)
```

### f-directories `(path &optional fn recursive)`

Return all directories in `path`. If `fn` is specified, return only
those who returns a true value. If `recursive`, return all directories
recursively.

```lisp
(f-directories "path/to/dir")
(f-directories "path/to/dir" (lambda (dir) ((f-filename dir) "test")))
(f-directories "path/to/dir" nil t)
```

### f-files `(path &optional fn recursive)`

Return all files in `path`. If `fn` is specified, return only those
who returns a true value. If `recursive`, return all files
recursively.

```lisp
(f-files "path/to/dir")
(f-files "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-files "path/to/dir" nil t)
```
