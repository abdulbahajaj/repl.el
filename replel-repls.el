(require 'cl-lib)

(cl-defstruct replel--repls-st
  repo name (run-cmd "make run") template (apt-get '()))

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

(provide 'replel-repls)
