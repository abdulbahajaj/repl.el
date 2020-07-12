#!/bin/sh
:; exec emacs --no-site-file --script $0
:; exit 0

(require 'subr-x)
(load (concat default-directory "dash/dash.el"))
(require 'cl-lib)
(require 'dash)

(setq namespace "replel")

(cl-defun get-repo-name (&key lang)
  (format "%s/%s" namespace lang))

(defun run-cmd (cmd)
  (print cmd)
  (print (shell-command-to-string (format "/bin/bash -c \"%s\"" cmd))))

(cl-defun update-replel-image (&key lang deps template)
  (let ((repo (get-repo-name :lang lang)))
    (run-cmd
     (format "docker build -f Dockerfile.replel -t %s:$(git rev-parse head) --build-arg dep=%s --build-arg template=%s %s"
	     repo (string-join deps " ") template default-directory))
    (run-cmd (format "docker push %s" repo))))

(cl-defstruct env lang deps template)

(defun update-all-images (images)
  (--map (update-replel-image :lang (env-lang it)
			      :deps (env-deps it)
			      :template (or (env-template it)
					    (format "templates/template.%s" (env-lang it))))
	 images))

(update-all-images
 (list
  (make-env :lang "clang" :deps '("build-essential") :template "templates/template.c")
  ;; (make-env :lang "cpp" :deps '("build-essential"))
  ;; (make-env :lang "clj" :deps '("clojure"))
  ;; (make-env :lang "cljs" :deps '("clojure"))
  ;; (make-env :lang "go" :deps '("golang"))
  ;; (make-env :lang "java" :deps '("openjdk-11-jre-headless"))
  ;; (make-env :lang "js" :deps '("nodejs"))
  ;; (make-env :lang "py" :deps '("python3-pip"))
  (make-env :lang "py2" :deps '("python-pip") :template "templates/template.py")
  ;; (make-env :lang "sh" :deps '())
  ;; (make-env :lang "go" :deps '("golang"))
  ))


