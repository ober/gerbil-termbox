#!/usr/bin/env gxi
(import :std/build-script)

(def project-dir
  (path-normalize (path-directory (this-source-file))))

(defbuild-script
  `((gxc: "libtermbox"
          "-cc-options" ,(string-append "-I" project-dir)
          "-ld-options" "")
    "termbox"
    "draw"))
