# f.el
[![CI](https://github.com/rejeep/f.el/actions/workflows/workflow.yml/badge.svg)](https://github.com/rejeep/f.el/actions/workflows/workflow.yml)
[![Coverage Status](https://img.shields.io/coveralls/rejeep/f.el.svg)](https://coveralls.io/r/rejeep/f.el)

Much inspired by [@magnars](https://github.com/magnars)'s excellent
[s.el](https://github.com/magnars/s.el) and
[dash.el](https://github.com/magnars/dash.el),
[f.el](https://github.com/rejeep/f.el) is a modern API for working
with files and directories in Emacs.

## Installation

It's available on [Melpa](https://melpa.org/):

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
* [f-swap-ext](#f-swap-ext) `(path ext)`
* [f-base](#f-base-path) `(path)`
* [f-relative](#f-relative-path-optional-dir) `(path &optional dir)`
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
* [f-copy-contents](#f-copy-contents-from-to) `(from to)`
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
* [f-empty?](#f-empty-path) `(path)`

### Stats

* [f-size](#f-size-path) `(path)`
* [f-depth](#f-depth-path) `(path)`

### Misc

* [f-this-file](#f-this-file-) `()`
* [f-path-separator](#f-path-separator-) `()`
* [f-glob](#f-glob-pattern-optional-path) `(pattern &optional path)`
* [f-entries](#f-entries-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-directories](#f-directories-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-files](#f-files-path-optional-fn-recursive) `(path &optional fn recursive)`
* [f-root](#f-root-) `()`
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

Expand PATH relative to DIR (or ‘default-directory’).
PATH and DIR can be either a directory names or directory file
names.  Return a directory name if PATH is a directory name, and
a directory file name otherwise.  File name handlers are
ignored.

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

Return FILENAME’s final "extension".
The extension, in a file name, is the part that begins with the last ‘.’,
excluding version numbers and backup suffixes, except that a leading ‘.’
of the file name, if there is one, doesn’t count.
Return nil for extensionless file names such as ‘foo’.
Return the empty string for file names such as ‘foo.’.

By default, the returned value excludes the period that starts the
extension, but if the optional argument PERIOD is non-nil, the period
is included in the value, and in that case, if FILENAME has no
extension, the value is "".

(fn FILENAME &optional PERIOD)

```lisp
(f-ext "path/to/file.ext") ;; => "ext"
(f-ext "path/to/directory") ;; => nil
```

### f-no-ext `(path)`

Return FILENAME sans final "extension".
The extension, in a file name, is the part that begins with the last ‘.’,
except that a leading ‘.’ of the file name, if there is one, doesn’t count.

(fn FILENAME)

```lisp
(f-no-ext "path/to/file.ext") ;; => "path/to/file"
(f-no-ext "path/to/directory") ;; => "path/to/directory"
```

### f-swap-ext `(path ext)`

Return PATH but with EXT as the new extension.
EXT must not be nil or empty.

```lisp
(f-swap-ext "path/to/file.ext" "org") ;; => "path/to/file.org"
(f-swap-ext "path/to/file.ext" "") ;; => error
```

### f-base `(path)`

Return the name of PATH, excluding the extension of file.

```lisp
(f-base "path/to/file.ext") ;; => "file"
(f-base "path/to/directory") ;; => nil
```

### f-relative `(path &optional dir)`

Convert FILENAME to be relative to DIRECTORY (default: ‘default-directory’).
This function returns a relative file name that is equivalent to FILENAME
when used with that default directory as the default.
If FILENAME is a relative file name, it will be interpreted as existing in
‘default-directory’.
If FILENAME and DIRECTORY lie on different machines or on different drives
on a DOS/Windows machine, it returns FILENAME in expanded form.

(fn FILENAME &optional DIRECTORY)

```lisp
(f-relative "/some/path/relative/to/my/file.txt" "/some/path/") ;; => relative/to/my/file.txt
(f-relative "/default/directory/my/file.txt") ;; => my/file.txt
```

### f-short `(path)`

Return a version of FILENAME shortened using ‘directory-abbrev-alist’.
This also substitutes "~" for the user’s home directory (unless the
home directory is a root directory) and removes automounter prefixes
(see the variable ‘automount-dir-prefix’).

When this function is first called, it caches the user’s home
directory as a regexp in ‘abbreviated-home-dir’, and reuses it
afterwards (so long as the home directory does not change;
if you want to permanently change your home directory after having
started Emacs, set ‘abbreviated-home-dir’ to nil so it will be recalculated).

(fn FILENAME)

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

Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against ‘default-directory’.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

(fn FILENAME)

```lisp
(f-canonical "/path/to/real/file") ;; => /path/to/real/file
(f-canonical "/link/to/file") ;; => /path/to/real/file
```

### f-slash `(path)`

Append slash to PATH unless one already.

Some functions, such as ‘call-process’ requires there to be an
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

Return unique suffixes of FILES.

This function expects no duplicate paths.

```lisp
(f-uniquify '("/foo/bar" "/foo/baz" "/foo/quux")) ;; => '("bar" "baz" "quux")
(f-uniquify '("/foo/bar" "/www/bar" "/foo/quux")) ;; => '("foo/bar" "www/bar" "quux")
(f-uniquify '("/foo/bar" "/www/bar" "/www/bar/quux")) ;; => '("foo/bar" "www/bar" "quux")
(f-uniquify '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")) ;; => '("foo/bar" "www/bar" "foo/baz" "home/www/baz" "foo/www/baz" "foo")
```

### f-uniquify-alist `(paths)`

Return alist mapping FILES to unique suffixes of FILES.

This function expects no duplicate paths.

```lisp
(f-uniquify-alist '("/foo/bar" "/foo/baz" "/foo/quux")) ;; => '(("/foo/bar" . "bar") ("/foo/baz" . "baz") ("/foo/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/www/bar" "/foo/quux")) ;; => '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/foo/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/www/bar" "/www/bar/quux")) ;; => '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/www/bar/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")) ;; => '(("/foo/bar" . "foo/bar") ("/home/www/bar" . "www/bar") ("/foo/baz" . "foo/baz") ("/home/www/baz" . "home/www/baz") ("/opt/foo/www/baz" . "foo/www/baz") ("/var/foo" . "foo"))
```

### f-read-bytes `(path)`

Read binary data from PATH.

Return the binary data as unibyte string. The optional second and
third arguments BEG and END specify what portion of the file to
read.

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

CODING defaults to ‘utf-8’.

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

The function can accept fully formed paths as its sole argument,
but only if this creates only one directory. It cannot create
subdirectories recursively from a fully formed path yet.

```lisp
(f-mkdir "dir") ;; creates /default/directory/dir
(f-mkdir "other" "dir") ;; creates /default/directory/other/dir
(f-mkdir "/" "some" "path") ;; creates /some/path
(f-mkdir "~" "yet" "another" "dir") ;; creates ~/yet/another/dir
(f-mkdir "~/some/dir") ;; creates "dir" if "~/some" already exists
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

Create a symlink to SOURCE from PATH.

```lisp
(f-symlink "path/to/source" "path/to/link")
```

### f-move `(from to)`

Move or rename FROM to TO.
If TO is a directory name, move FROM into TO.

```lisp
(f-move "path/to/file.txt" "new-file.txt")
(f-move "path/to/file.txt" "other/path")
```

### f-copy `(from to)`

Copy file or directory FROM to TO.
If FROM names a directory and TO is a directory name, copy FROM
into TO as a subdirectory.

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

Return t if file FILENAME exists (whether or not you can read it).
Return nil if FILENAME does not exist, or if there was trouble
determining whether the file exists.
See also ‘file-readable-p’ and ‘file-attributes’.
This returns nil for a symlink to a nonexistent file.
Use ‘file-symlink-p’ to test for such links.

(fn FILENAME)

```lisp
(f-exists? "path/to/file.txt")
(f-exists? "path/to/dir")
```

### f-directory? `(path)`

Return t if FILENAME names an existing directory.
Return nil if FILENAME does not name a directory, or if there
was trouble determining whether FILENAME is a directory.

As a special case, this function will also return t if FILENAME is the
empty string ("").  This quirk is due to Emacs interpreting the
empty string (in some cases) as the current directory.

Symbolic links to directories count as directories.
See ‘file-symlink-p’ to distinguish symlinks.

(fn FILENAME)

Aliases: `f-directory-p f-dir? f-dir-p`

```lisp
(f-directory? "path/to/file.txt") ;; => nil
(f-directory? "path/to/dir") ;; => t
```

### f-file? `(path)`

Return t if FILENAME names a regular file.
This is the sort of file that holds an ordinary stream of data bytes.
Return nil if FILENAME does not exist or is not a regular file,
or there was trouble determining whether FILENAME is a regular file.
Symbolic links to regular files count as regular files.
See ‘file-symlink-p’ to distinguish symlinks.

(fn FILENAME)

Alias: `f-file-p`

```lisp
(f-file? "path/to/file.txt") ;; => t
(f-file? "path/to/dir") ;; => nil
```

### f-symlink? `(path)`

Return t if PATH is symlink, false otherwise.

Alias: `f-symlink-p`

```lisp
(f-symlink? "path/to/file.txt") ;; => nil
(f-symlink? "path/to/dir") ;; => nil
(f-symlink? "path/to/link") ;; => t
```

### f-readable? `(path)`

Return t if file FILENAME exists and you can read it.
See also ‘file-exists-p’ and ‘file-attributes’.

(fn FILENAME)

Alias: `f-readable-p`

```lisp
(f-readable? "path/to/file.txt")
(f-readable? "path/to/dir")
```

### f-writable? `(path)`

Return t if file FILENAME can be written or created by you.

(fn FILENAME)

Alias: `f-writable-p`

```lisp
(f-writable? "path/to/file.txt")
(f-writable? "path/to/dir")
```

### f-executable? `(path)`

Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory.
(It is generally better to use ‘file-accessible-directory-p’ for that
purpose, though.)

(fn FILENAME)

Alias: `f-executable-p`

```lisp
(f-executable? "path/to/file.txt")
(f-executable? "path/to/dir")
```

### f-absolute? `(path)`

Return t if FILENAME is an absolute file name.
On Unix, absolute file names start with ‘/’.  In Emacs, an absolute
file name can also start with an initial ‘~’ or ‘~USER’ component,
where USER is a valid login name.

(fn FILENAME)

Alias: `f-absolute-p`

```lisp
(f-absolute? "path/to/dir") ;; => nil
(f-absolute? "/full/path/to/dir") ;; => t
```

### f-relative? `(path)`

Return t if PATH is relative, false otherwise.

Alias: `f-relative-p`

```lisp
(f-relative? "path/to/dir") ;; => t
(f-relative? "/full/path/to/dir") ;; => nil
```

### f-root? `(path)`

Return t if PATH is root directory, false otherwise.

Alias: `f-root-p`

```lisp
(f-root? "/") ;; => t
(f-root? "/not/root") ;; => nil
```

### f-ext? `(path ext)`

Return t if extension of PATH is EXT, false otherwise.

If EXT is nil or omitted, return t if PATH has any extension,
false otherwise.

The extension, in a file name, is the part that follows the last
’.’, excluding version numbers and backup suffixes.

Alias: `f-ext-p`

```lisp
(f-ext? "path/to/file.el" "el") ;; => t
(f-ext? "path/to/file.el" "txt") ;; => nil
(f-ext? "path/to/file.el") ;; => t
(f-ext? "path/to/file") ;; => nil
```

### f-same? `(path-a path-b)`

Return t if PATH-A and PATH-B are references to same file.

Aliases: `f-same-p f-equal? f-equal-p`

```lisp
(f-same? "foo.txt" "foo.txt") ;; => t
(f-same? "/path/to/foo.txt" "/path/to/bar.txt") ;; => nil
```

### f-parent-of? `(path-a path-b)`

Return t if PATH-A is parent of PATH-B.

Alias: `f-parent-of-p`

```lisp
(f-parent-of? "/path/to" "/path/to/dir") ;; => t
(f-parent-of? "/path/to/dir" "/path/to") ;; => nil
(f-parent-of? "/path/to" "/path/to") ;; => nil
```

### f-child-of? `(path-a path-b)`

Return t if PATH-A is child of PATH-B.

Alias: `f-child-of-p`

```lisp
(f-child-of? "/path/to" "/path/to/dir") ;; => nil
(f-child-of? "/path/to/dir" "/path/to") ;; => t
(f-child-of? "/path/to" "/path/to") ;; => nil
```

### f-ancestor-of? `(path-a path-b)`

Return t if PATH-A is ancestor of PATH-B.

Alias: `f-ancestor-of-p`

```lisp
(f-ancestor-of? "/path/to" "/path/to/dir") ;; => t
(f-ancestor-of? "/path" "/path/to/dir") ;; => t
(f-ancestor-of? "/path/to/dir" "/path/to") ;; => nil
(f-ancestor-of? "/path/to" "/path/to") ;; => nil
```

### f-descendant-of? `(path-a path-b)`

Return t if PATH-A is desendant of PATH-B.

Alias: `f-descendant-of-p`

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

Alias: `f-hidden-p`

### f-empty? `(path)`

If PATH is a file, return t if the file in PATH is empty, nil otherwise.
If PATH is directory, return t if directory has no files, nil otherwise.

```lisp
(f-empty? "/path/to/empty-file") ;; => t
(f-empty? "/path/to/file-with-contents") ;; => nil
(f-empty? "/path/to/empty-dir/") ;; => t
(f-empty? "/path/to/dir-with-contents/") ;; => nil
```

Alias: `f-empty-p`

### f-size `(path)`

Return size of PATH.

If PATH is a file, return size of that file.  If PATH is
directory, return sum of all files in PATH.

```lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
```

### f-depth `(path)`

Return the depth of PATH.

At first, PATH is expanded with ‘f-expand’.  Then the full path is used to
detect the depth.
’/’ will be zero depth,  ’/usr’ will be one depth.  And so on.

```lisp
(f-depth "/") ;; 0
(f-depth "/var/") ;; 1
(f-depth "/usr/local/bin") ;; 3
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

FN - called for each found file and directory.  If FN returns a thruthy
value, file or directory will be included.
RECURSIVE - Search for files and directories recursive.

```lisp
(f-entries "path/to/dir")
(f-entries "path/to/dir" (lambda (file) (s-matches? "test" file)))
(f-entries "path/to/dir" nil t)
(f--entries "path/to/dir" (s-matches? "test" it))
```

### f-directories `(path &optional fn recursive)`

Find all directories in PATH.  See ‘f-entries’.

```lisp
(f-directories "path/to/dir")
(f-directories "path/to/dir" (lambda (dir) (equal (f-filename dir) "test")))
(f-directories "path/to/dir" nil t)
(f--directories "path/to/dir" (equal (f-filename it) "test"))
```

### f-files `(path &optional fn recursive)`

Find all files in PATH.  See ‘f-entries’.

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

### f-traverse-upwards `(fn &optional path)`

Traverse up as long as FN return nil, starting at PATH.

If FN returns a non-nil value, the path sent as argument to FN is
returned.  If no function callback return a non-nil value, nil is
returned.

```lisp
(f-traverse-upwards
 (lambda (path)
   (f-exists? (f-expand ".git" path)))
 start-path)

(f--traverse-upwards (f-exists? (f-expand ".git" it)) start-path) ;; same as above
```

### f-with-sandbox `(path-or-paths &rest body)`

Only allow PATH-OR-PATHS and descendants to be modified in BODY.

```lisp
(f-with-sandbox foo-path
  (f-touch (f-expand "foo" foo-path)))
(f-with-sandbox (list foo-path bar-path)
  (f-touch (f-expand "foo" foo-path))
  (f-touch (f-expand "bar" bar-path)))
(f-with-sandbox foo-path
  (f-touch (f-expand "bar" bar-path))) ;; "Destructive operation outside sandbox"
```

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
