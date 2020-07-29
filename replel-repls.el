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
			  :apt-get '("openjdk-11-jre-headless"))
   (make-replel--repls-st :name "javascript"
			  :repo "js"
			  :apt-get '("nodejs"))
   (make-replel--repls-st :name "python 3"
			  :repo "py3"
			  :template "templates/py"
			  :open-at "/replel/main.py"
			  :apt-get '("python3-pip"))
   (make-replel--repls-st :name "python 2"
			  :repo "py2"
			  :open-at "/replel/main.py"
			  :template "templates/py"
			  :apt-get '("python-pip"))
   (make-replel--repls-st :name "bash"
			  :repo "bash"
			  :open-at "/replel/main.sh"
			  :apt-get '())
   ))

(cl-defun replel--ht-get-keys (hashtable)
  "Given a hash table, return a list of all keys"
  (let ((all-keys '()))
    (maphash
     (lambda (key val) (setq all-keys (cons key all-keys)))
     hashtable)
    all-keys))

(cl-defun replel--repls-get-repo-from-obj (repl-obj)
  (format "%s/%s:%s"
	  replel--repo-namespace
	  (replel--repls-st-repo repl-obj)
	  replel--build-info-pined-image-tag))

(cl-defun replel--repls-get-repo (name)
  (replel--repls-get-repo-from-obj
   (car (--drop-while (not (string= name (replel--repls-st-name it)))
		      replel--repls-defined))))

(cl-defun replel--repls-get-name ()
  (--map (replel--repls-st-name it) replel--repls-defined))

(cl-defun replel--repls-initialize ()
  (--map (replel-defrule :image it) replel--repls-defined))

(defun replel-repls-run ()
  (interactive)
  (compile "make run"))

(defun replel/test ()
  (interactive)
  (message (let ((process-environment tramp-remote-process-environment)) (getenv "PATH")) ))


(provide 'replel-repls)
