;;; replel.el --- Use containers as a dev environment -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Abdul Bahajaj

;; Author: Abdul Bahajaj <abdulbahajaj@gmail.com>
;; URL: https://github.com/abdulbahajaj/repl.el
;; Keywords: repl, containers
;; Version: 0.1
;; Package-Requires: ((dash "2.14.1") (docker-tramp "0.1") (emacs "24.5") (s "1.12.0"))

;;; Commentary:

;; Repl.el is a repl.it for emacs. More specifically, it allows you to easily move to
;; predefined environments

;;; Code

(require 'cl-lib)
(require 'replel-build-info)
(require 'replel-repls)

(cl-defstruct replel--container-st
  name repo created-at command running-for status size ports id labels mounts networks)

(cl-defstruct replel--image-st repo)




;;;; Helpers

(cl-defun replel--cmd-run (cmd)
  (let ((default-dir default-directory))
    (cd "/")
    (message cmd)
    (let ((res (shell-command-to-string
		;; TODO Maybe support windows? probably not.
		(format "/bin/sh -c \"%s\"" cmd))))
      (cd default-dir)
      (replel--helper-remove-trailing-space res))))

(cl-defun replel--cmd-or (cmds)
  "Or one or more commands"
  (s-join " || " cmds))




;;;; Interacting with container runtime

(cl-defun replel--container-run (&key image-name cont-name)
  "Run a docker container with image `image-name` and container `cont-name`"
  (replel--cmd-run (format "docker run -t -d --name %s %s" cont-name image-name)))


(cl-defun replel--container-get-tramp-path (&key cont-name path)
  (format "/docker:%s:%s" cont-name (or path "/")))

(cl-defun replel--container-open (&key cont-name path)
  "Goes to the container at a specific file or dir"
  (find-file (replel--container-get-tramp-path :cont-name cont-name :path path)))

(cl-defun replel--container-resume (cont-name)
  "Resumes a stopped/paused container"
  (replel--cmd-run
   (replel--cmd-or
    (mapcar
     (lambda (cmd) (format cmd cont-name))
     '("docker container start %s"
       "docker container unpause %s")))))

(cl-defun replel--container-rename (cont new-name)
  (replel--cmd-run
   (format "docker rename %s %s"
	   (replel--container-st-name cont)
	   new-name)))

(cl-defun replel--container-delete (cont)
  (let ((cont-name (replel--container-st-name cont)))
    (replel--cmd-run
     (format "docker stop %s && docker rm %s" cont-name cont-name))))

(cl-defun replel--container-exec (&key workdir cont-name cmd)
  (replel--cmd-run (format "docker exec -w %s %s %s" workdir cont-name cmd)))




;;;; Retrieving info

(cl-defun replel--container-image-ls ()
  "Returns a string list of image names"
  (let ((seperator "____"))
    (--map
     (let ((image-desc (s-split seperator it)))
       (make-replel--image-st :repo (car image-desc)))
      (s-split "\n"
	       (replel--cmd-run
		(format "docker image ls --format='%s'"
			(string-join '("{{.Repository}}:{{.Tag}}")
				     seperator)))))))

(cl-defun replel--container-ps (&optional &key filter dformat)
  (replel--cmd-run
   (concat "docker ps "
	   (when filter (format " --filter %s" filter))
	   (when dformat (format " --format=\"%s\"" dformat)))))

(cl-defun replel--container-ls ()
  (let ((seperator "_____"))
    (--map
     (let ((container-desc (s-split seperator it)))
       (make-replel--container-st
	:name (nth 0 container-desc)
	:repo (nth 1 container-desc)
	:created-at (s-replace "-" " " (s-replace-regexp " .*" "" (nth 2 container-desc)))
	:command (nth 3 container-desc)
	:running-for (nth 4 container-desc)
	:status (nth 5 container-desc)
	:size (nth 6 container-desc)
	:ports (nth 7 container-desc)
	:id (nth 8 container-desc)
	:labels (nth 9 container-desc)
	:mounts (nth 10 container-desc)
	:networks (nth 11 container-desc)))
     (s-split
      "\n"
      (replel--container-ps
       :dformat
       (string-join
	'("{{.Names}}" "{{.Image}}" "{{.CreatedAt}}" "{{.Command}}"
	  "{{.RunningFor}}" "{{.Status}}" "{{.Size}}" "{{.Ports}}"
	  "{{.ID}}" "{{.Labels}}" "{{.Mounts}}" "{{.Networks}}")
	seperator))))))





;;;; Name generation

(defconst replel--container-naming-list
  '("Aardvark" "Albatross" "Alligator" "Alpaca" "Ant" "Anteater" "Antelope" "Ape"
    "Donkey" "Baboon" "Badger" "Barracuda" "Bat" "Bear" "Beaver" "Bee" "Bison" "Boar"
    "Butterfly" "Camel" "Capybara" "Caribou" "Cassowary" "Cat" "Caterpillar" "Cattle"
    "Chicken" "Chimpanzee" "Chinchilla" "Chough" "Clam" "Cobra" "Cockroach" "Cod" "Cormorant"
    "Crab" "Crane" "Crocodile" "Crow" "Curlew" "Deer" "Dinosaur" "Dog" "Dogfish" "Buffalo"
    "Dolphin" "Dotterel" "Dove" "Dragonfly" "Duck" "Dugong" "Dunlin" "Eagle" "Echidna"
    "Eel" "Eland" "Elephant" "Elk" "Emu" "Falcon" "Ferret" "Finch" "Fish" "Cheetah"
    "Flamingo" "Fly" "Fox" "Frog" "Gaur" "Gazelle" "Gerbil" "Giraffe" "Gnat" "Coyote"
    "Gnu" "Goat" "Goldfinch" "Goldfish" "Goose" "Gorilla" "Goshawk" "Grasshopper" "Grouse"
    "Guanaco" "Gull" "Hamster" "Hare" "Hawk" "Hedgehog" "Heron" "Herring" "Hippopotamus"
    "Hornet" "Horse" "Human" "Hummingbird" "Hyena" "Ibex" "Ibis" "Jackal" "Jaguar"
    "Jay" "Jellyfish" "Kangaroo" "Kingfisher" "Koala" "Kookabura" "Kouprey" "Kudu" "Lapwing"
    "Lark" "Lemur" "Leopard" "Lion" "Llama" "Lobster" "Locust" "Loris" "Louse" "Chamois"
    "Lyrebird" "Magpie" "Mallard" "Manatee" "Mandrill" "Mantis" "Marten" "Meerkat" "Mink"
    "Mole" "Mongoose" "Monkey" "Moose" "Mosquito" "Mouse" "Mule" "Narwhal" "Newt" "Armadillo"
    "Nightingale" "Octopus" "Okapi" "Opossum" "Oryx" "Ostrich" "Otter" "Owl" "Oyster"
    "Panther" "Parrot" "Partridge" "Peafowl" "Pelican" "Penguin" "Pheasant" "Pig" "Pigeon"
    "Pony" "Porcupine" "Porpoise" "Quail" "Quelea" "Quetzal" "Rabbit" "Raccoon" "Rail"
    "Ram" "Rat" "Raven" "Red deer" "Red panda" "Reindeer" "Rhinoceros" "Rook" "Salamander"
    "Salmon" "Sand Dollar" "Sandpiper" "Sardine" "Scorpion" "Seahorse" "Seal" "Shark" "Sheep"
    "Shrew" "Skunk" "Snail" "Snake" "Sparrow" "Spider" "Spoonbill" "Squid" "Squirrel"
    "Starling" "Stingray" "Stinkbug" "Stork" "Swallow" "Swan" "Tapir" "Tarsier" "Termite"
    "Tiger" "Toad" "Trout" "Turkey" "Turtle" "Viper" "Vulture" "Wallaby" "Walrus" "Wasp"
    "Weasel" "Whale" "Wildcat" "Wolf" "Wolverine" "Wombat" "Woodcock" "Woodpecker"
    "Worm" "Wren" "Yak" "Zebra" ))

(defconst replel--container-naming-list-length (length replel--container-naming-list))

(cl-defun replel--gen-name ()
  (string-join
   (cdr
    (--iterate (nth (random replel--container-naming-list-length) replel--container-naming-list) "" 4))
   "-"))




;;;; Helpers

(cl-defun replel--helper-remove-trailing-space (str)
  (s-replace-regexp
   "[\n|\t]+$" "" str))

(defmacro replel--ilm (&rest body)
  `(lambda ()
     (interactive)
     ,@body))




;;;; Replel API

(define-minor-mode replel-mode
  "A minor mode that is enabled when a repl is entered"
  nil
  "replel" )

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

(defun replel-repls-run ()
  (interactive)
  (save-buffer)
  (compile "make run"))

(cl-defun replel--get-entrypoint (cont-name)
   (replel--container-exec :workdir "/replel/"
			   :cont-name cont-name
			   :cmd "make entrypoint"))

(cl-defun replel-resume (cont-name) 
  (replel--container-resume cont-name)
  (let* ((open (replel--get-entrypoint cont-name)))
    (replel--container-open :cont-name cont-name :path open))
  (replel-mode 1))

(cl-defun replel--start (image-name)
  "Given a string name, find the associated replel, run it, tramp to it, and start replel-mode"
  (let ((cont-name (replel--gen-name))
	(default-directory "/"))
    (message
     (replel--container-run :image-name image-name
			    :cont-name cont-name))
    (replel--container-open :cont-name cont-name
			    :path (replel--get-entrypoint cont-name)))
  (replel-mode 1))

(cl-defun replel-resume-select ()
  "Select a replel to run"
  (interactive)
  (replel-resume
   (completing-read "Select container "
	     (--map (replel--container-st-name it)
		    (replel--container-ls)))))

(cl-defun replel-start-image ()
  "Select a replel to run"
  (interactive)
  (replel--start
   (completing-read "Select image "
	     (--map (replel--image-st-repo it)
		    (replel--container-image-ls)))))

(cl-defun replel-start-repl ()
  (interactive)
  (replel--start
   (replel--repls-get-repo
    (completing-read "select repl "
	      (replel--repls-get-name)))))




;;;; UI components

(cl-defun replel--ui-button (&key text bindings)
  (let* ((start-pos (point))
	 (end-pos (+ start-pos (length text))))
    (insert text)
    (let ((key-map (make-sparse-keymap)))
      (--map (define-key key-map (kbd (car it)) (cadr it)) bindings)
      (put-text-property start-pos end-pos 'keymap key-map))))

(cl-defun replel--ui-normalize (str width)
  (let ((len (length str)))
    (if (> len width)
	(format "%s..." (substring str 0 (- width 3)))
      (concat str (s-repeat (- width len) " ")))))

(cl-defun replel--ui-row-get-text (&key cols width-list)
  (concat
   (string-join
    (--map-indexed
     (let ((width (nth it-index width-list)))
       (if width (replel--ui-normalize it width) it))
     cols) " ") "\n"))




;;;; Overview view

(define-derived-mode
  replel-major-mode
  special-mode
  "Replel overview"
  "Goes to Replel overview")

(defconst replel--overview-width-proportion '(23 25 30))

(defconst replel--overview-table-header
  (replel--ui-row-get-text
      :cols (list "STATUS"
		  "REPO"
		  "NAME")
      :width-list replel--overview-width-proportion))

(cl-defun replel-overview ()
  (interactive)
  (let ((replel-buffer (generate-new-buffer "*replel*")))
    (switch-to-buffer replel-buffer)
    (replel-major-mode)
    (replel-overview-refresh)))

(cl-defun replel-overview-refresh ()
  (interactive)
  (setq inhibit-read-only t)
  (let ((current-pos (point))
	)
    (erase-buffer)
    (insert replel--overview-table-header)
    (--map (replel--overview-draw-container it replel--overview-width-proportion)
	   (replel--container-ls))
    (goto-char current-pos))
  (setq inhibit-read-only nil))

(cl-defun replel--overview-gen-container-text (cont width-list)
  (replel--ui-row-get-text :cols (list (replel--container-st-status cont)
				       (replel--container-st-repo cont)
				       (replel--container-st-name cont))
			   :width-list width-list))

(cl-defun replel--overview-draw-container (cont width-list)
  (replel--ui-button
   :text (replel--overview-gen-container-text cont width-list)
   :bindings
   `(("u" ,(replel--ilm
	    (replel-overview-refresh)
	    (message "UPDATED")))
     ("r" ,(lambda (new-name)
	     (interactive "sNew name: ")
	     (replel--container-rename cont new-name)
	     (replel-overview-refresh))
      (message "RENAMED"))
     ("R" ,(replel--ilm
	    (let ((default-directory (replel--container-get-tramp-path
				      :cont-name (replel--container-st-name cont)
				      :path "/replel/")))
	      (replel-repls-run))))
     ("d" ,(replel--ilm
	   (if (y-or-n-p (format "Deleted %s" (replel--container-st-name cont)))
	       (replel--container-delete cont))
	   (replel-overview-refresh)
	   (message "DELETED")))
     ("<return>" ,(replel--ilm
		 (replel-resume
		  (replel--container-st-name cont)))))))

(provide 'replel)

;;; replel.el ends here
