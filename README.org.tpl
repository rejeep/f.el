# -*- mode: org -*-
#+title: f.el

[[https://github.com/rejeep/f.el/actions/workflows/workflow.yml][file:https://github.com/rejeep/f.el/actions/workflows/workflow.yml/badge.svg]]
[[https://coveralls.io/r/rejeep/f.el][file:https://img.shields.io/coveralls/rejeep/f.el.svg]]
[[https://melpa.org/#/f][file:https://melpa.org/packages/f-badge.svg]]
[[https://stable.melpa.org/#/f][file:https://stable.melpa.org/packages/f-badge.svg]]

Much inspired by [[https://github.com/magnars][@magnars]]s excellent [[https://github.com/magnars/s.el][s.el]] and [[https://github.com/magnars/dash.el][dash.el]], [[https://github.com/rejeep/f.el][f.el]] is a
modern API for working with files and directories in Emacs.

* Installation                                                     :noexport:
It’s available on [[https://melpa.org/#/f][Melpa]] and [[https://stable.melpa.org/#/f][Melpa Stable]].
#+begin_src text
M-x package-install f
#+end_src

Or you can just dump ~f.el~ in your load path somewhere.

* Table of Contents                                                :TOC_3_gh:
- [[#contributing][Contributing]]
- [[#documentation-and-examples][Documentation and examples]]
  - [[#paths][Paths]]
    - [[#f-join][f-join]]
    - [[#f-split][f-split]]
    - [[#f-expand][f-expand]]
    - [[#f-filename][f-filename]]
    - [[#f-dirname][f-dirname]]
    - [[#f-common-parent][f-common-parent]]
    - [[#f-ext][f-ext]]
    - [[#f-no-ext][f-no-ext]]
    - [[#f-swap-ext][f-swap-ext]]
    - [[#f-base][f-base]]
    - [[#f-relative][f-relative]]
    - [[#f-short][f-short]]
    - [[#f-long][f-long]]
    - [[#f-canonical][f-canonical]]
    - [[#f-slash][f-slash]]
    - [[#f-full][f-full]]
    - [[#f-uniquify][f-uniquify]]
    - [[#f-uniquify-alist][f-uniquify-alist]]
  - [[#io][I/O]]
    - [[#f-read-bytes][f-read-bytes]]
    - [[#f-write-bytes][f-write-bytes]]
    - [[#f-append-bytes][f-append-bytes]]
    - [[#f-read-text][f-read-text]]
    - [[#f-write-text][f-write-text]]
    - [[#f-append-text][f-append-text]]
  - [[#destructive][Destructive]]
    - [[#f-mkdir][f-mkdir]]
    - [[#f-mkdir-full-path][f-mkdir-full-path]]
    - [[#f-delete][f-delete]]
    - [[#f-symlink][f-symlink]]
    - [[#f-move][f-move]]
    - [[#f-copy][f-copy]]
    - [[#f-copy-contents][f-copy-contents]]
    - [[#f-touch][f-touch]]
  - [[#predicates][Predicates]]
    - [[#f-exists-p][f-exists-p]]
    - [[#f-directory-p][f-directory-p]]
    - [[#f-file-p][f-file-p]]
    - [[#f-symlink-p][f-symlink-p]]
    - [[#f-readable-p][f-readable-p]]
    - [[#f-writable-p][f-writable-p]]
    - [[#f-executable-p][f-executable-p]]
    - [[#f-absolute-p][f-absolute-p]]
    - [[#f-relative-p][f-relative-p]]
    - [[#f-root-p][f-root-p]]
    - [[#f-ext-p][f-ext-p]]
    - [[#f-same-p][f-same-p]]
    - [[#f-parent-of-p][f-parent-of-p]]
    - [[#f-child-of-p][f-child-of-p]]
    - [[#f-ancestor-of-p][f-ancestor-of-p]]
    - [[#f-descendant-of-p][f-descendant-of-p]]
    - [[#f-hidden-p][f-hidden-p]]
    - [[#f-empty-p][f-empty-p]]
  - [[#stats][Stats]]
    - [[#f-size][f-size]]
    - [[#f-depth][f-depth]]
    - [[#f-change-time][f-change-time]]
    - [[#f-modification-time][f-modification-time]]
    - [[#f-access-time][f-access-time]]
  - [[#misc][Misc]]
    - [[#f-this-file][f-this-file]]
    - [[#f-path-separator][f-path-separator]]
    - [[#f-glob][f-glob]]
    - [[#f-entries][f-entries]]
    - [[#f-directories][f-directories]]
    - [[#f-files][f-files]]
    - [[#f-root][f-root]]
    - [[#f-traverse-upwards][f-traverse-upwards]]
    - [[#f-with-sandbox][f-with-sandbox]]
- [[#example][Example]]
  - [[#using-standard-emacs-builtin-functions][Using standard Emacs builtin functions]]
  - [[#using-fel][Using f.el]]

* Contributing
Check [[file:./CONTRIBUTING.org]]

* Documentation and examples
** Paths
*** f-join
#+begin_example
(f-join &rest args)

{{f-join}}
#+end_example

#+begin_src emacs-lisp
(f-join "path") ;; => "path"
(f-join "path" "to") ;; => "path/to"
(f-join "/" "path" "to" "heaven") ;; => "/path/to/heaven"
(f-join "path" "/to" "file") ;; => "/to/file"
#+end_src

*** f-split
#+begin_example
(f-split path)

{{f-split}}
#+end_example

#+begin_src emacs-lisp
(f-split "path") ;; => '("path")
(f-split "path/to") ;; => '("path" "to")
(f-split "/path/to/heaven") ;; => '("/" "path" "to" "heaven")
(f-split "~/back/to/earth") ;; => '("~" "back" "to" "earth")
#+end_src

*** f-expand
#+begin_example
(f-expand path &optional dir)

{{f-expand}}
#+end_example

#+begin_src emacs-lisp
(f-expand "name") ;; => "/default/directory/name"
(f-expand "name" "other/directory") ;; => "other/directory/name"
#+end_src

*** f-filename
#+begin_example
(f-filename path)

{{f-filename}}
#+end_example

#+begin_src emacs-lisp
(f-filename "path/to/file.ext") ;; => "file.ext"
(f-filename "path/to/directory") ;; => "directory"
#+end_src

*** f-dirname
#+begin_example
(f-dirname path)

{{f-dirname}}
#+end_example

Alias: ~f-parent~

#+begin_src emacs-lisp
(f-dirname "path/to/file.ext") ;; => "path/to"
(f-dirname "path/to/directory") ;; => "path/to"
(f-dirname "/") ;; => nil
#+end_src

*** f-common-parent
#+begin_example
(f-common-parent paths)

{{f-common-parent}}
#+end_example

#+begin_src emacs-lisp
(f-common-parent '("foo/bar/baz" "foo/bar/qux" "foo/bar/mux")) ;; => "foo/bar/"
(f-common-parent '("/foo/bar/baz" "/foo/bar/qux" "/foo/bax/mux")) ;; => "/foo/"
(f-common-parent '("foo/bar/baz" "quack/bar/qux" "lack/bar/mux")) ;; => ""
#+end_src

*** f-ext
#+begin_example
(f-ext path)
#+end_example

Alias of ~file-name-extension~

#+begin_src emacs-lisp
(f-ext "path/to/file") ;; => nil
(f-ext "path/to/file.txt") ;; => txt
(f-ext "path/to/file.txt.org") ;; => org
#+end_src

*** f-no-ext
#+begin_example
(f-no-ext path)
#+end_example

Alias of ~file-name-sans-extension~

#+begin_src emacs-lisp
(f-no-ext "path/to/file") ;; => path/to/file
(f-no-ext "path/to/file.txt") ;; => path/to/file
(f-no-ext "path/to/file.txt.org") ;; => path/to/file.txt
#+end_src

*** f-swap-ext
#+begin_example
(f-swap-ext path ext)

{{f-swap-ext}}
#+end_example

#+begin_src emacs-lisp
(f-swap-ext "path/to/file.ext" "org") ;; => "path/to/file.org"
(f-swap-ext "path/to/file.ext" "") ;; => error
#+end_src

*** f-base
#+begin_example
(f-base path)

{{f-base}}
#+end_example

#+begin_src emacs-lisp
(f-base "path/to/file.ext") ;; => "file"
(f-base "path/to/directory") ;; => nil
#+end_src

*** f-relative
#+begin_example
(f-relative path &optional dir)

{{f-relative}}
#+end_example

#+begin_src emacs-lisp
(f-relative "/some/path/relative/to/my/file.txt" "/some/path/") ;; => relative/to/my/file.txt
(f-relative "/default/directory/my/file.txt") ;; => my/file.txt
#+end_src

*** f-short
#+begin_example
(f-short path)
#+end_example

Alias of ~abbreviate-file-name~

Alias: ~f-abbrev~

#+begin_src emacs-lisp
(f-short "/Users/foo/Code/bar") ;; => ~/Code/bar
(f-short "/path/to/Code/bar") ;; => /path/to/Code/bar
#+end_src

*** f-long
#+begin_example
(f-long path)

{{f-long}}
#+end_example

#+begin_src emacs-lisp
(f-long "~/Code/bar") ;; => /Users/foo/Code/bar
(f-long "/path/to/Code/bar") ;; => /path/to/Code/bar
#+end_src

*** f-canonical
#+begin_example
(f-canonical path)
#+end_example

Alias of ~file-truename~

#+begin_src emacs-lisp
(f-canonical "/path/to/real/file") ;; => /path/to/real/file
(f-canonical "/link/to/file") ;; => /path/to/real/file
#+end_src

*** f-slash
#+begin_example
(f-slash path)

{{f-slash}}
#+end_example

#+begin_src emacs-lisp
(f-slash "/path/to/file") ;; => /path/to/file
(f-slash "/path/to/dir") ;; => /path/to/dir/
(f-slash "/path/to/dir/") ;; => /path/to/dir/
#+end_src

*** f-full
#+begin_example
(f-full path)

{{f-full}}
#+end_example

#+begin_src emacs-lisp
(f-full "~/path/to/file") ;; => /home/foo/path/to/file
(f-full "~/path/to/dir") ;; => /home/foo/path/to/dir/
(f-full "~/path/to/dir/") ;; => /home/foo/path/to/dir/
#+end_src

*** f-uniquify
#+begin_example
(f-uniquify paths)

{{f-uniquify}}
#+end_example

#+begin_src emacs-lisp
(f-uniquify '("/foo/bar" "/foo/baz" "/foo/quux")) ;; => '("bar" "baz" "quux")
(f-uniquify '("/foo/bar" "/www/bar" "/foo/quux")) ;; => '("foo/bar" "www/bar" "quux")
(f-uniquify '("/foo/bar" "/www/bar" "/www/bar/quux")) ;; => '("foo/bar" "www/bar" "quux")
(f-uniquify '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")) ;; => '("foo/bar" "www/bar" "foo/baz" "home/www/baz" "foo/www/baz" "foo")
#+end_src

*** f-uniquify-alist
#+begin_example
(f-uniquify-alist paths)

{{f-uniquify-alist}}
#+end_example

#+begin_src emacs-lisp
(f-uniquify-alist '("/foo/bar" "/foo/baz" "/foo/quux")) ;; => '(("/foo/bar" . "bar") ("/foo/baz" . "baz") ("/foo/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/www/bar" "/foo/quux")) ;; => '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/foo/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/www/bar" "/www/bar/quux")) ;; => '(("/foo/bar" . "foo/bar") ("/www/bar" . "www/bar") ("/www/bar/quux" . "quux"))
(f-uniquify-alist '("/foo/bar" "/foo/baz" "/home/www/bar" "/home/www/baz" "/var/foo" "/opt/foo/www/baz")) ;; => '(("/foo/bar" . "foo/bar") ("/home/www/bar" . "www/bar") ("/foo/baz" . "foo/baz") ("/home/www/baz" . "home/www/baz") ("/opt/foo/www/baz" . "foo/www/baz") ("/var/foo" . "foo"))
#+end_src

** I/O
*** f-read-bytes
#+begin_example
(f-read-bytes path)

{{f-read-bytes}}
#+end_example

#+begin_src emacs-lisp
(f-read-bytes "path/to/binary/data")
#+end_src

*** f-write-bytes
#+begin_example
(f-write-bytes data path)

{{f-write-bytes}}
#+end_example

#+begin_src emacs-lisp
(f-write-bytes (unibyte-string 72 101 108 108 111 32 119 111 114 108 100) "path/to/binary/data")
#+end_src

*** f-append-bytes
#+begin_example
(f-append-bytes text coding path)

{{f-append-bytes}}
#+end_example

#+begin_src emacs-lisp
(f-append-bytes "path/to/file" (unibyte-string 72 101 108 108 111 32 119 111 114 108 100))
#+end_src

*** f-read-text
#+begin_example
(f-read-text path &optional coding)

{{f-read-text}}
#+end_example

Alias: ~f-read~

#+begin_src emacs-lisp
(f-read-text "path/to/file.txt" 'utf-8)
(f-read "path/to/file.txt" 'utf-8)
#+end_src

*** f-write-text
#+begin_example
(f-write-text text coding path)

{{f-write-text}}
#+end_example

Alias: ~f-write~

#+begin_src emacs-lisp
(f-write-text "Hello world" 'utf-8 "path/to/file.txt")
(f-write "Hello world" 'utf-8 "path/to/file.txt")
#+end_src

*** f-append-text
#+begin_example
(f-append-text text coding path)

{{f-append-text}}
#+end_example

Alias: ~f-append~

#+begin_src emacs-lisp
(f-append-text "Hello world" 'utf-8 "path/to/file.txt")
(f-append "Hello world" 'utf-8 "path/to/file.txt")
#+end_src

** Destructive
*** f-mkdir
#+begin_example
(f-mkdir &rest dirs)

{{f-mkdir}}
#+end_example

#+begin_src emacs-lisp
(f-mkdir "dir") ;; creates /default/directory/dir
(f-mkdir "other" "dir") ;; creates /default/directory/other/dir
(f-mkdir "/" "some" "path") ;; creates /some/path
(f-mkdir "~" "yet" "another" "dir") ;; creates ~/yet/another/dir
#+end_src

*** f-mkdir-full-path
#+begin_example
(f-mkdir-full-path dir)

{{f-mkdir-full-path}}
#+end_example

#+begin_src emacs-lisp
(f-mkdir-full-path "dir") ;; creates /default/directory/dir
(f-mkdir-full-path "other/dir") ;; creates /default/directory/other/dir
(f-mkdir-full-path "/some/path") ;; creates /some/path
(f-mkdir-full-path "~/yet/another/dir") ;; creates ~/yet/another/dir
#+end_src

*** f-delete
#+begin_example
(f-delete path &optional force)

{{f-delete}}
#+end_example

#+begin_src emacs-lisp
(f-delete "dir")
(f-delete "other/dir" t)
(f-delete "path/to/file.txt")
#+end_src

*** f-symlink
#+begin_example
(f-symlink source path)

{{f-symlink}}
#+end_example

#+begin_src emacs-lisp
(f-symlink "path/to/source" "path/to/link")
#+end_src

*** f-move
#+begin_example
(f-move from to)

{{f-move}}
#+end_example

#+begin_src emacs-lisp
(f-move "path/to/file.txt" "new-file.txt")
(f-move "path/to/file.txt" "other/path")
#+end_src

*** f-copy
#+begin_example
(f-copy from to)

{{f-copy}}
#+end_example

#+begin_src emacs-lisp
(f-copy "path/to/file.txt" "new-file.txt")
(f-copy "path/to/dir" "other/dir")
#+end_src

*** f-copy-contents
#+begin_example
(f-copy-contents from to)

{{f-copy-contents}}
#+end_example

#+begin_src emacs-lisp
(f-copy-contents "path/to/dir" "path/to/other/dir")
#+end_src

*** f-touch
#+begin_example
(f-touch path)

{{f-touch}}
#+end_example

#+begin_src emacs-lisp
(f-touch "path/to/existing/file.txt")
(f-touch "path/to/non/existing/file.txt")
#+end_src

** Predicates
*** f-exists-p
#+begin_example
(f-exists-p path)
#+end_example

Alias of ~file-exists-p~

Alias: ~f-exists?~

#+begin_src emacs-lisp
(f-exists-p "path/to/file.txt")
(f-exists-p "path/to/dir")
#+end_src

*** f-directory-p
#+begin_example
(f-directory-p path)
#+end_example

Alias of ~file-directory-p~

Aliases:
- ~f-directory?~
- ~f-dir-p~
- ~f-dir?~

#+begin_src emacs-lisp
(f-directory-p "path/to/file.txt") ;; => nil
(f-directory-p "path/to/dir") ;; => t
#+end_src

*** f-file-p
#+begin_example
(f-file-p path)
#+end_example

Alias of ~file-regular-p~

Alias: ~f-file?~

#+begin_src emacs-lisp
(f-file-p "path/to/file.txt") ;; => t
(f-file-p "path/to/dir") ;; => nil
#+end_src

*** f-symlink-p
#+begin_example
(f-symlink-p path)

{{f-symlink-p}}
#+end_example

Alias: ~f-symlink?~

#+begin_src emacs-lisp
(f-symlink-p "path/to/file.txt") ;; => nil
(f-symlink-p "path/to/dir") ;; => nil
(f-symlink-p "path/to/link") ;; => t
#+end_src

*** f-readable-p
#+begin_example
(f-readable-p path)
#+end_example

Alias of ~file-readable-p~

Alias: ~f-readable?~

#+begin_src emacs-lisp
(f-readable-p "path/to/file.txt")
(f-readable-p "path/to/dir")
#+end_src

*** f-writable-p
#+begin_example
(f-writable-p path)
#+end_example

Alias of ~file-writable-p~

Alias: ~f-writable?~

#+begin_src emacs-lisp
(f-writable-p "path/to/file.txt")
(f-writable-p "path/to/dir")
#+end_src

*** f-executable-p
#+begin_example
(f-executable-p path)
#+end_example

Alias of ~file-executable-p~

Alias: ~f-executable?~

#+begin_src emacs-lisp
(f-executable-p "path/to/file.txt")
(f-executable-p "path/to/dir")
#+end_src

*** f-absolute-p
#+begin_example
(f-absolute-p path)
#+end_example

Alias of ~file-name-absolute-p~

Alias: ~f-absolute?~

#+begin_src emacs-lisp
(f-absolute-p "path/to/dir") ;; => nil
(f-absolute-p "/full/path/to/dir") ;; => t
#+end_src

*** f-relative-p
#+begin_example
(f-relative-p path)

{{f-relative-p}}
#+end_example

Alias: ~f-relative?~

#+begin_src emacs-lisp
(f-relative-p "path/to/dir") ;; => t
(f-relative-p "/full/path/to/dir") ;; => nil
#+end_src

*** f-root-p
#+begin_example
(f-root-p path)

{{f-root-p}}
#+end_example

Alias: ~f-root?~

#+begin_src emacs-lisp
(f-root-p "/") ;; => t
(f-root-p "/not/root") ;; => nil
#+end_src

*** f-ext-p
#+begin_example
(f-ext-p path ext)

{{f-ext-p}}
#+end_example

Alias: ~f-ext?~

#+begin_src emacs-lisp
(f-ext-p "path/to/file.el" "el") ;; => t
(f-ext-p "path/to/file.el" "txt") ;; => nil
(f-ext-p "path/to/file.el") ;; => t
(f-ext-p "path/to/file") ;; => nil
#+end_src

*** f-same-p
#+begin_example
(f-same-p path-a path-b)

{{f-same-p}}
#+end_example

Aliases:
- ~f-same?~
- ~f-equal-p~
- ~f-equal?~

#+begin_src emacs-lisp
(f-same-p "foo.txt" "foo.txt") ;; => t
(f-same-p "/path/to/foo.txt" "/path/to/bar.txt") ;; => nil
(f-same-p "foo/bar/../baz" "foo/baz") ;; => t
#+end_src

*** f-parent-of-p
#+begin_example
(f-parent-of-p path-a path-b)

{{f-parent-of-p}}
#+end_example

Alias: ~f-parent-of?~

#+begin_src emacs-lisp
(f-parent-of-p "/path/to" "/path/to/dir") ;; => t
(f-parent-of-p "/path/to/dir" "/path/to") ;; => nil
(f-parent-of-p "/path/to" "/path/to") ;; => nil
#+end_src

*** f-child-of-p
#+begin_example
(f-child-of-p path-a path-b)

{{f-child-of-p}}
#+end_example

Alias: ~f-child-of?~

#+begin_src emacs-lisp
(f-child-of-p "/path/to" "/path/to/dir") ;; => nil
(f-child-of-p "/path/to/dir" "/path/to") ;; => t
(f-child-of-p "/path/to" "/path/to") ;; => nil
#+end_src

*** f-ancestor-of-p
#+begin_example
(f-ancestor-of-p path-a path-b)

{{f-ancestor-of-p}}
#+end_example

Alias: ~f-ancestor-of?~

#+begin_src emacs-lisp
(f-ancestor-of-p "/path/to" "/path/to/dir") ;; => t
(f-ancestor-of-p "/path" "/path/to/dir") ;; => t
(f-ancestor-of-p "/path/to/dir" "/path/to") ;; => nil
(f-ancestor-of-p "/path/to" "/path/to") ;; => nil
#+end_src

*** f-descendant-of-p
#+begin_example
(f-descendant-of-p path-a path-b)

{{f-descendant-of-p}}
#+end_example

Alias: ~f-descendant-of?~

#+begin_src emacs-lisp
(f-descendant-of-p "/path/to/dir" "/path/to") ;; => t
(f-descendant-of-p "/path/to/dir" "/path") ;; => t
(f-descendant-of-p "/path/to" "/path/to/dir") ;; => nil
(f-descendant-of-p "/path/to" "/path/to") ;; => nil
#+end_src

*** f-hidden-p
#+begin_example
(f-hidden-p path)

{{f-hidden-p}}
#+end_example

Alias: ~f-hidden?~

#+begin_src emacs-lisp
(f-hidden-p "path/to/foo") ;; => nil
(f-hidden-p ".path/to/foo") ;; => t
(f-hidden-p "path/.to/foo") ;; => nil
(f-hidden-p "path/to/.foo") ;; => nil
(f-hidden-p ".path/to/foo" 'any) ;; => t
(f-hidden-p "path/.to/foo" 'any) ;; => t
(f-hidden-p "path/to/.foo" 'any) ;; => t
(f-hidden-p ".path/to/foo" 'last) ;; => nil
(f-hidden-p "path/.to/foo" 'last) ;; => nil
(f-hidden-p "path/to/.foo" 'last) ;; => t
#+end_src

*** f-empty-p
#+begin_example
(f-empty-p path)

{{f-empty-p}}
#+end_example

Alias: ~f-empty?~

#+begin_src emacs-lisp
(f-empty-p "/path/to/empty-file") ;; => t
(f-empty-p "/path/to/file-with-contents") ;; => nil
(f-empty-p "/path/to/empty-dir/") ;; => t
(f-empty-p "/path/to/dir-with-contents/") ;; => nil
#+end_src

** Stats
*** f-size
#+begin_example
(f-size path)

{{f-size}}
#+end_example

#+begin_src emacs-lisp
(f-size "path/to/file.txt")
(f-size "path/to/dir")
#+end_src

*** f-depth
#+begin_example
(f-depth path)

{{f-depth}}
#+end_example

#+begin_src emacs-lisp
(f-depth "/") ;; 0
(f-depth "/var/") ;; 1
(f-depth "/usr/local/bin") ;; 3
#+end_src

*** f-change-time
#+begin_example
(f-change-time path)

{{f-change-time}}
#+end_example

#+begin_src emacs-lisp
(f-change-time "path/to/file.txt")
(f-change-time "path/to/dir")
#+end_src

*** f-modification-time
#+begin_example
(f-modification-time path)

{{f-modification-time}}
#+end_example

#+begin_src emacs-lisp
(f-modification-time "path/to/file.txt")
(f-modification-time "path/to/dir")
#+end_src

*** f-access-time
#+begin_example
(f-access-time path)

{{f-access-time}}
#+end_example

#+begin_src emacs-lisp
(f-access-time "path/to/file.txt")
(f-access-time "path/to/dir")
#+end_src

** Misc
*** f-this-file
#+begin_example
(f-this-file)

{{f-this-file}}
#+end_example

#+begin_src emacs-lisp
(f-this-file) ;; => /path/to/this/file
#+end_src

*** f-path-separator
#+begin_example
(f-path-separator)

{{f-path-separator}}
#+end_example

#+begin_src emacs-lisp
(f-path-separator) ;; => /
#+end_src

*** f-glob
#+begin_example
(f-glob pattern &optional path)

{{f-glob}}
#+end_example

#+begin_src emacs-lisp
(f-glob "path/to/*.el")
(f-glob "*.el" "path/to")
#+end_src

*** f-entries
#+begin_example
(f-entries path &optional fn recursive)

{{f-entries}}
#+end_example

#+begin_src emacs-lisp
(f-entries "path/to/dir")
(f-entries "path/to/dir" (lambda (file) (s-matches? "test" file)))
(f-entries "path/to/dir" nil t)
(f--entries "path/to/dir" (s-matches? "test" it))
#+end_src

*** f-directories
#+begin_example
(f-directories path &optional fn recursive)

{{f-directories}}
#+end_example

#+begin_src emacs-lisp
(f-directories "path/to/dir")
(f-directories "path/to/dir" (lambda (dir) (equal (f-filename dir) "test")))
(f-directories "path/to/dir" nil t)
(f--directories "path/to/dir" (equal (f-filename it) "test"))
#+end_src

*** f-files
#+begin_example
(f-files path &optional fn recursive)

{{f-files}}
#+end_example

#+begin_src emacs-lisp
(f-files "path/to/dir")
(f-files "path/to/dir" (lambda (file) (equal (f-ext file) "el")))
(f-files "path/to/dir" nil t)
(f--files "path/to/dir" (equal (f-ext it) "el"))
#+end_src

*** f-root
#+begin_example
(f-root)

{{f-root}}
#+end_example

#+begin_src emacs-lisp
(f-root) ;; => "/"
#+end_src

*** f-traverse-upwards
#+begin_example
(f-traverse-upwards fn &optional path)

{{f-traverse-upwards}}
#+end_example

#+begin_src emacs-lisp
(f-traverse-upwards
 (lambda (path)
   (f-exists? (f-expand ".git" path)))
 start-path)

(f--traverse-upwards (f-exists? (f-expand ".git" it)) start-path) ;; same as above
#+end_src

*** f-with-sandbox
#+begin_example
(f-with-sandbox path-or-paths &rest body)

{{f-with-sandbox}}
#+end_example

#+begin_src emacs-lisp
(f-with-sandbox foo-path
  (f-touch (f-expand "foo" foo-path)))
(f-with-sandbox (list foo-path bar-path)
  (f-touch (f-expand "foo" foo-path))
  (f-touch (f-expand "bar" bar-path)))
(f-with-sandbox foo-path
  (f-touch (f-expand "bar" bar-path))) ;; "Destructive operation outside sandbox"
#+end_src

* Example

Here's an example of a function that finds the Git project root.

** Using standard Emacs builtin functions
#+begin_src emacs-lisp
(defun find-git-root (&optional dir)
  (unless dir (setq dir (expand-file-name (file-name-directory (buffer-file-name)))))
  (let ((parent (expand-file-name ".." dir)))
    (unless (equal parent dir)
      (if (file-exists-p (expand-file-name ".git" dir))
          dir
        (find-git-root parent)))))
#+end_src

** Using f.el
#+begin_src emacs-lisp
(defun find-git-root (&optional dir)
  (interactive)
  (unless dir (setq dir (f-dirname (buffer-file-name))))
  (let ((parent (f-parent dir)))
    (unless (f-root? parent)
      (if (f-exists? (f-expand ".git" dir))
          dir
        (find-git-root parent)))))
#+end_src

Now, try writing it even simpler yourself. Hint, check out ~f-traverse-upwards~.
