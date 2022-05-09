shell:
	guix time-machine -C channels.scm -- shell --pure -D -f guix.scm

docs:
	emacs -Q --script build-docs.el
