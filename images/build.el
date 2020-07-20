#!/bin/sh
:; cd $(dirname $0)
:; git clone https://github.com/magnars/dash.el.git dash || :
:; (cd dash && git checkout 732d92eac56023a4fb4a5dc3d9d4e274ebf44bf9)
:; export GIT_COMMIT_HASH=$(git rev-parse head)
:; exec emacs --no-site-file --script $(basename $0)
:; exit 0

(load (concat default-directory "dash/dash.el"))
(load (concat default-directory "../replel-repls.el"))

(require 'subr-x)
(require 'cl-lib)
(require 'dash)
(require 'replel-repls)

(defconst git-commit-hash
  (getenv "GIT_COMMIT_HASH"))

(setq namespace "replel")

(cl-defun get-repo-full-name (&key repo)
  (format "%s/%s:%s" namespace repo git-commit-hash))

(defun run-cmd (cmd)
  (print cmd)
  (print (shell-command-to-string (format "/bin/bash -c \"%s\"" cmd))))

(cl-defun get-build-cmd (&key repo apt-get template)
  (format "docker build -f Dockerfile.replel -t %s --build-arg dep=%s --build-arg template=%s %s"
	     repo (string-join apt-get " ") template default-directory))

(cl-defun update-repl-image (&key repo apt-get template)
  (let ((repo (get-repo-full-name :repo repo)))
    (run-cmd
     (get-build-cmd :repo repo :apt-get apt-get :template template))
    (run-cmd (format "docker push %s" repo))))

(defun update-all-images (images)
  (--map
   (update-repl-image :repo (replel--repls-st-repo it)
		      :apt-get (replel--repls-st-apt-get it)
		      :template (or (replel--repls-st-template it)
				    (format "templates/template.%s" (replel--repls-st-repo it))))
   images))

(defun gen-build-info ()
  `(progn
     (defconst replel--build-info-pined-image-tag ,git-commit-hash)))

(defun write-build-info (build-info)
  (write-region
   (format "%S\n(provide 'replel-build-info)" build-info)
   nil
   "../replel-build-info.el")
  (print "Updated conf file"))

(update-all-images replel--repls-defined)

(write-build-info (gen-build-info))
