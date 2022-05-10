shell:
	guix time-machine -C channels.scm -- shell --pure -D -f guix.scm

all: docs package upload add clean

docs:
	emacs -Q --script build-docs.el

package:
	python3 setup.py sdist bdist_wheel

upload:
	twine upload dist/*

add:
	git add --all

clean:
	git clean -xdf
