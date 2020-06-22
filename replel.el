;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Abdul Bahajaj

;; Author: Abdul Bahajaj <abdulbahajaj@gmail.com>
;; URL: https://github.com/abdulbahajaj/repl.el
;; Keywords: repl, containers
;; Version: 0.1
;; Package-Requires: ((dash "2.14.1") (docker-tramp "0.1") (emacs "24.5") (s 1.12.0))

;;; Commentary:

;; Repl.el is a repl.it for emacs. More specifically, it allows you to easily move to
;; predefined environments

(require 'cl-lib)

(defvar replel--defined-conf (make-hash-table :test 'equal)
  "Contains a list of all defined repls")

(defconst replel--allowed-attrs '(:name :base :run :start-at))


(cl-defun replel--init-hashtable (hashtable &rest vals)
  "Given a hash table"
  (let ((vals (car vals)))
    (while vals
      (puthash (car vals) (cadr vals) hashtable)
      (setq vals (cddr vals))))
  hashtable)

(cl-defun replel--get-hashtable-keys (hashtable)
  "Given a hash table, return a list of all keys"
  (let ((all-keys '()))
    (maphash
     (lambda (key val) (setq all-keys (cons key all-keys)))
     hashtable)
    all-keys))

(cl-defun replel--get-all-repls)

(cl-defun replel--cmd-run (cmd)
  (message cmd)
  (shell-command-to-string
   ;; TODO Maybe support windows? 
   (format "/bin/sh -c \"%s\"" cmd)))


(cl-defun replel--docker-run (&key image-name container-name)
  "Run a docker container with image `image-name` and container `container-name`"
   (replel--cmd-run (format "docker run -t -d --name %s %s" container-name image-name)))

(cl-defun replel--names-list ()
  (replel--get-hashtable-keys replel--defined-conf))

(cl-defun replel-list ()
  (let ((all-replels '()))
    (maphash
     (lambda (key val) (setq all-replels (cons val all-replels)))
     replel--defined-conf)
    all-replels))

(cl-defun replel-define (&rest conf-list)
  "Define a replel"
  (let* ((conf-hashtable (replel--init-hashtable (make-hash-table) conf-list))
	 (replel-name (gethash :name conf-hashtable))
	 (diff (-difference (replel--get-hashtable-keys conf-hashtable) replel--allowed-attrs)))
    (if diff
	(message
	 (format
	  "in the definition of %s you have used one or more undefined attributes: %s"
	  replel-name diff))
      (puthash (gethash :name conf-hashtable) conf-hashtable replel--defined-conf))))

(cl-defun replel--open-container-at (&key container-name path)
  (find-file (format "/docker:%s:%s" container-name path)))

(cl-defun replel-start (replel-name)
  "Given a string name, find the associated replel, run it, tramp to it, and start replel-mode"
  (let ((replel-conf (or (gethash replel-name replel--defined-conf)
			 (replel--init-hashtable (make-hash-table) 
						 `(:base ,replel-name
							 :name ,replel-name
							 :run nil
							 :start-at "/"))))
	(container-name (format "replel-%s-%s-%s"
				replel-name
				(s-replace ":" "." (s-replace " " "-" (current-time-string)))
				(random 99999))))
    (cd "/")
    (replel--docker-run :image-name (gethash :base replel-conf)
			:container-name container-name)
    (replel--open-container-at :container-name container-name
			       :path (gethash :start-at replel-conf))))


(cl-defun replel--docker-image-ls ()
  (s-split "\n" (replel--cmd-run "docker image ls --format='{{.Repository}}'")))

(cl-defun replel-start-select ()
  "Select a replel to run"
  (interactive)
  (replel-start
   (ivy-read "Select image "
	     (-union
	      (replel--names-list)
	      (replel--docker-image-ls)))))

(cl-defun replel--get-replel-name-from-container-name (container-name)
  (s-replace-regexp "-.*" "" (s-replace "replel-" "" container-name)))


(cl-defun replel--cmd-or (l)
  (s-join " || " l))

(cl-defun replel--docker-resume (container-name)
  (replel--cmd-run
   (replel--cmd-or
    (mapcar
     (lambda (cmd) (format cmd container-name))
     '("docker container start %s"
       "docker container unpause %s")))))

(cl-defun replel-resume (container-name)
  (replel--docker-resume container-name)
  (let* ((replel-name (replel--get-replel-name-from-container-name container-name))
	 (replel-conf (gethash replel-name replel--defined-conf))
	 (start-at (gethash :start-at replel-conf)))
    (replel--open-container-at :container-name container-name :path start-at)))

(cl-defun replel--docker-ps (&optional &key filter dformat)
  (replel--cmd-run
   (concat "docker ps --all"
	   (when filter (format " --filter %s" filter))
	   (when dformat (format " --format=\"%s\"" dformat))) ))

(cl-defun replel--ps ()
  (butlast
   (s-split
    "\n"
    (replel--docker-ps :filter "name=\'replel-*\'"
		       :dformat "{{.Names}}"))))


(cl-defun replel-resume-select ()
  "Select a replel to run"
  (interactive)
  (replel-resume
   (concat "replel-"
	   (ivy-read "Select image "
		     (mapcar
		      (lambda (replel-name) (s-replace-regexp "^replel-" "" replel-name))
		      (replel--ps))))))

(cl-defun replel-run ()
  "runs the defined run command for the replel"
  (interactive))

(cl-defun replel-overview ()
  "Displays a list of existing replels. Give the ability to resume, delete, rename, etc"
  (interactive))

(replel-define
 :base "ubuntu"
 :name "test"
 :run "make"
 :start-at "/test.txt")

(provide 'replel)
;;; repel ends here
