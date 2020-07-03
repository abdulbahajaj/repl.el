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

(defconst replel--prefix "replel-")


;; Hashtable functionality

(cl-defun replel--ht-get-keys (hashtable)
  "Given a hash table, return a list of all keys"
  (let ((all-keys '()))
    (maphash
     (lambda (key val) (setq all-keys (cons key all-keys)))
     hashtable)
    all-keys))

(cl-defun replel--ht-set (ht &rest vals)
  (while vals
    (puthash (car vals) (cadr vals) ht)
    (setq vals (cddr vals)))
  ht)

(cl-defun replel--ht (&rest vals)
  (apply 'replel--ht-set (cons (make-hash-table) vals)))



;; Rules

(defvar replel--defined-conf (make-hash-table :test 'equal)
  "Contains a list of all defined repls")

(defconst replel--allowed-attrs '(:image :run :open))

(cl-defun replel--get-conf (replel-name)
  (or (gethash replel-name replel--defined-conf)
      (replel--ht
       :image replel-name
       :run nil
       :open "/")))

(cl-defun replel--names-list ()
  (replel--ht-get-keys replel--defined-conf))

(cl-defun replel-define (&rest conf-list)
  "Define a replel"
  (let* ((conf-hashtable (apply 'replel--ht conf-list))
	 (image-name (gethash :image conf-hashtable))
	 (diff (-difference (replel--ht-get-keys conf-hashtable) replel--allowed-attrs)))
    (if diff
	(message
	 (format
	  "in the definition of %s you have used one or more undefined attributes: %s"
	  replel-name diff))
      (puthash image-name conf-hashtable replel--defined-conf))))


;; Helpers
(cl-defun replel--cmd-run (cmd)
  (let ((default-dir default-directory))
    (cd "/")
    (message cmd)
    (let ((res (shell-command-to-string
		;; TODO Maybe support windows? probably not.
		(format "/bin/sh -c \"%s\"" cmd))))
      (cd default-dir)
      res)))

(cl-defun replel--cmd-or (cmds)
  "Or one or more commands"
  (s-join " || " cmds))


;; Interacting with container runtime

(cl-defun replel--container-run (&key image-name container-name)
  "Run a docker container with image `image-name` and container `container-name`"
  (replel--cmd-run (format "docker run -t -d --name %s %s" container-name image-name)))

(cl-defun replel--container-open (&key container-name path)
  "Goes to the container at a specific file or dir"
  (find-file (format "/docker:%s:%s" container-name (or path "/"))))

(cl-defun replel--container-resume (container-name)
  "Resumes a stopped/paused container"
  (replel--cmd-run
   (replel--cmd-or
    (mapcar
     (lambda (cmd) (format cmd container-name))
     '("docker container start %s"
       "docker container unpause %s")))))



;; Retrieving info

(cl-defun replel--container-image-ls ()
  "Returns a string list of image names"
  (--map
   (let ((image-desc (s-split ":" it)))
     (replel--ht :repository (car image-desc)))
   (butlast
    (s-split "\n"
	     (replel--cmd-run "docker image ls --format='{{.Repository}}'")))))

(cl-defun replel--container-ps (&optional &key filter dformat)
  (replel--cmd-run
   (concat "docker ps --all"
	   (when filter (format " --filter %s" filter))
	   (when dformat (format " --format=\"%s\"" dformat)))))

(cl-defun replel--container-ls ()
  (--map
   (let ((container-desc (s-split ":" it)))
     (replel--ht :name (car container-desc) :image (cadr container-desc)))
   (butlast
    (s-split
     "\n"
     (replel--container-ps :dformat "{{.Names}}:{{.Image}}")))))

(cl-defun replel--get-replel-name-from-container-name (container-name)
  (s-replace-regexp "-.*" "" (s-replace replel--prefix "" container-name)))

(cl-defun replel--current-container-name ()
  (let ((current-path default-directory))
    (s-replace-regexp ":.*" "" (s-replace-regexp "^\/docker:" "" current-path))))

(cl-defun replel--container-get-desc (container-name)
  (car
    (let ((current-container-name container-name))
      (--drop-while (let ((container-name (gethash :name it)))
		      (if (string= current-container-name container-name) nil t))
		    (replel--container-ls)))))

(cl-defun replel--container-image-name (container-name)
  (gethash :image
	   (let ((container-desc (replel--container-get-desc container-name)))
	     (if container-desc container-desc (make-hash-table)))))


;; Replel API

(cl-defun replel--gen-name () (random 9999))

(cl-defun replel-resume (container-name) 
  (replel--container-resume container-name)
  (let* ((image-name (replel--container-image-name container-name))
	 (replel-conf (replel--get-conf image-name))
	 (open (gethash :open replel-conf)))
    (replel--container-open :container-name container-name :path open)))

(cl-defun replel-start (image-name)
  "Given a string name, find the associated replel, run it, tramp to it, and start replel-mode"
  (let ((replel-conf (replel--get-conf image-name))
	(container-name (replel--gen-name)))
    (cd "/")
    (replel--container-run :image-name image-name
			   :container-name container-name)
    (replel--container-open :container-name container-name
			    :path (gethash :open replel-conf))))

(cl-defun replel-resume-select ()
  "Select a replel to run"
  (interactive)
  (replel-resume
   (ivy-read "Select container "
	     (--map (gethash :name it)
		    (replel--container-ls)))))

(cl-defun replel-start-select ()
  "Select a replel to run"
  (interactive)
  (replel-start
   (ivy-read "Select image "
	     (--map (gethash :repository it)
		    (replel--container-image-ls)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Defining repls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defining replels
(replel-define
 :image "ubuntu"
 :run "make"
 :open "/test.txt")

(provide 'replel)
;;; repel ends here
