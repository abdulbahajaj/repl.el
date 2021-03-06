;;; replel.el --- Use containers as a dev environment -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Abdul Bahajaj

;; Author: Abdul Bahajaj <abdulbahajaj@gmail.com>
;; URL: https://github.com/abdulbahajaj/repl.el
;; Keywords: repl, containers
;; Version: 0.2
;; Package-Requires: ((dash "2.14.1") (docker-tramp "0.1") (emacs "24.5") (s "1.12.0"))

;;; Commentary:

;; Repl.el is a repl.it for emacs. More specifically, it allows you to easily move to
;; predefined environments

;;; Code

(require 'cl-lib)

(cl-defstruct replel--repls-st
  repo name (run-cmd "make run") template (apt-get '()) open-at)

(defconst replel--repo-namespace "replel")

(defconst replel--repls-defined
  (list
   (make-replel--repls-st :name "c"
			  :repo "clang"
			  :open-at "/replel/main.c"
			  :apt-get '())
   (make-replel--repls-st :name "c++"
			  :repo "cpp"
			  :apt-get '())
   (make-replel--repls-st :name "clojure"
			  :repo "clj"
			  :apt-get '("clojure"))
   (make-replel--repls-st :name "go"
			  :repo "go"
			  :apt-get '("golang"))
   (make-replel--repls-st :name "java"
			  :repo "java"
			  :open-at "/replel/Main.java"
			  :apt-get '("openjdk-11-jre-headless"))
   (make-replel--repls-st :name "javascript"
			  :repo "js"
			  :apt-get '("nodejs"))
   (make-replel--repls-st :name "python 3"
			  :repo "py3"
			  :open-at "/replel/main.py"
			  :apt-get '("python3-pip"))
   (make-replel--repls-st :name "python 2"
			  :repo "py2"
			  :open-at "/replel/main.py"
			  :apt-get '("python2"))
   (make-replel--repls-st :name "bash"
			  :repo "bash"
			  :open-at "/replel/main.sh"
			  :apt-get '()))
    "This list defines all builtin repls. It is used by the build command to build
repl images and is used by the `replel-start' command to figure out what repls are available")
(provide 'replel-repls)

;;; replel-repls.el ends here
