.PHONY: build-images
build-images:
	./images/build.el

.PHONY: clean
clean:
	rm -f *.elc
