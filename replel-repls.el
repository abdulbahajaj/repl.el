(require 'cl-lib)

(cl-defstruct replel--repls-st
  repo name (run-cmd "make run") template (apt-get '()))

(defconst replel--repo-namespace "replel")

(defconst replel--repls-defined
  (list
   (make-replel--repls-st :name "c"
			  :repo "clang"
			  :apt-get '("build-essential")
			  :template "templates/template.c")
   (make-replel--repls-st :name "c++"
			  :repo "cpp"
			  :apt-get '("build-essential"))
   (make-replel--repls-st :name "clojure"
			  :repo "clj"
			  :apt-get '("clojure"))
   (make-replel--repls-st :name "clojure script"
			  :repo "cljs"
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
			  :apt-get '("python3-pip"))
   (make-replel--repls-st :name "python 2"
			  :repo "py2"
			  :apt-get '("python-pip")
			  :template "templates/template.py")
   (make-replel--repls-st :name "sh"
			  :repo "sh"
			  :apt-get '())))

(cl-defun replel--ht-get-keys (hashtable)
  "Given a hash table, return a list of all keys"
  (let ((all-keys '()))
    (maphash
     (lambda (key val) (setq all-keys (cons key all-keys)))
     hashtable)
    all-keys))

(cl-defun replel--repls-get-repo (name)
  (format "%s/%s:%s"
	  replel--repo-namespace
	  (replel--repls-st-repo
	   (car (--drop-while (not (string= name (replel--repls-st-name it)))
			      replel--repls-defined)))
	  replel--build-info-pined-image-tag))

(cl-defun replel--repls-get-name ()
  (--map (replel--repls-st-name it) replel--repls-defined))


(provide 'replel-repls)
