(add-to-list 'load-path "~/.emacs.d/google/")
(require 'borg-mode)
;(require 'google3-build-mode)
(require 'protobuf-mode)

(define-derived-mode piccolo-mode python-mode "Piccolo"
  "python-mode variant for Piccolo")

(add-to-list 'ac-modes 'protobuf-mode)
;(add-to-list 'ac-modes 'google3-build-mode)
(add-to-list 'ac-modes 'borg-mode)
(add-to-list 'ac-modes 'mendel-mode)
(setq auto-mode-alist
      (nconc
       (list
        ;; GSLB configuration files
        (cons "/gslb\\.part\\.cfg$" 'gslb-mode)
        ;; .py files belong in Python mode
        (cons "\\.py$" 'python-mode)
        (cons "\\.pi$" 'piccolo-mode)
        ;; Beads files piggy-back on python-mode.
        (cons "\\.cnf$" 'python-mode)
        ;; MPM package definition files are Python syntax.
        (cons "/pkgdef$" 'python-mode)
        (cons "/BUILD$" 'borg-mode)
        (cons "\\.proto$" 'protobuf-mode)
        (cons "\\.protodevel$" 'protobuf-mode)
        (cons "\\.ncl$" 'ncl-mode)
        ;; .blueprint and .itcnf (Guitar config) files are NCL files.
        (cons "\\.blueprint$" 'ncl-mode)
        (cons "\\.itcnf$" 'ncl-mode)
        ;; SWIG files are made mostly of C++.
        (cons "\\.swig$" 'c++-mode)
        ;; gxp files are just xml files with largely html tags. Some
        ;; people find html-mode annoying (e.g. typing < gives &lt),
        ;; so we map .gxp to whatever mode is used by .html files
        (cons "\\.gxp$"
              (or (cdr (assoc "\\.html$" auto-mode-alist))
                 'html-mode))
        ;; We give our makefiles names like Makefile.foo
        (cons "/Makefile" 'makefile-mode)
        ;; Edit XML files in XML mode (rather than the default SGML
        ;; mode)
        (cons "\\.xml$" 'xml-mode)
        ;; Edit CTemplate files with tpl-mode.
        (cons "\\.tpl$" 'tpl-mode)
        ;; Mendel files
        (cons "mendel/.*\\.gcl$" 'mendel-mode)
        (cons "gws.*\\.gcl$" 'mendel-mode)
        ;; Various Borg-related files
        (cons "\\.bcl$" 'borg-mode)
        (cons "\\.borg$" 'borg-mode)
        (cons "\\.btcfg$" 'borg-mode)
        (cons "\\.gcl$" 'borg-mode)
        (cons "\\.gclx$" 'borg-mode)
        (cons "\\.pp$" 'borg-patchpanel-mode)
        (cons "\\.cfg$" 'borgmon-mode)
        (cons "\\.rules$" 'borgmon-mode)
        (cons "\\.probe$" 'borgmon-mode)
        (cons "\\.alert$" 'borgmon-mode)
        ;; borgmon test just for files under testdata dir
        (cons "google3/production/monitoring/.*/testdata/.*\\.data$"
              'borgmon-test-mode)
        ;; Sawzall
        ;(cons "\\.szl$" 'sawzall-mode)
        ;; Scraper
        (cons "\\.scraper$" 'scraper-mode)
        ;; ActionScript
        (cons "\\.as$" 'actionscript-mode)
        ;; Go language
        (cons "\\.go$" 'go-mode)
        ;; Megastore Definition Language
        (cons "\\.mdl$" 'sql-mode)
        ;; GYP
        (cons "\\.gypi?$" 'gyp-mode)
        ;; Text protobuf data, traditionally called ASCII before Proto2,
        ;; including Bigtable schemas. (Not to be confused with protobuf-mode
        ;; for protocol buffer definitions.)
        (cons "\\.ascii[-_]?pb$" 'protobuffer-mode)
        (cons "\\.ascii[-_]?proto$" 'protobuffer-mode)
        (cons "\\.pb[-_]?ascii$" 'protobuffer-mode)
        (cons "\\.pb[-_]?te?xt$" 'protobuffer-mode)
        (cons "\\.proto[-_]?ascii$" 'protobuffer-mode)
        (cons "\\.proto[-_]?te?xt$" 'protobuffer-mode)
        (cons "\\.te?xt[-_]?pb$" 'protobuffer-mode)
        (cons "\\.te?xt[-_]?proto$" 'protobuffer-mode)
        (cons "\\.btschema$" 'protobuffer-mode)
        (cons "\\.repoconfig$" 'protobuffer-mode)
        (cons "/METADATA$" 'protobuffer-mode)
        ;; Rosy service definitions.
        (cons "\\.rosy$" 'protobuf-mode)
        ;; Dabba configurations
        (cons "\\.dabba$" 'dabba-mode)
        ;; Spanner SDL files
        (cons "\\.sdl$" 'spansdl-mode)
        ;; GFE urlmap files
        (cons "/urlmap\\.production.*" 'urlmap-mode)
        (cons "/urlmap\\.any.*" 'urlmap-mode)
        (cons "/urlmap\\.part.*" 'urlmap-mode)
        (cons "/urlmap\\.test.*" 'urlmap-mode)
        )
       auto-mode-alist))

(provide 'my-google)
