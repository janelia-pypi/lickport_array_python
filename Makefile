shell:
	guix time-machine -C .channels.scm -- shell --pure -D -f .guix.scm

all: project package upload add clean

edit:
	emacs -q --no-site-file --no-site-lisp --no-splash -l .emacs --file .documentation.org

documentation:
	emacs --batch -Q  -l .emacs --eval '(process-org ".documentation.org")'

package:
	python3 setup.py sdist bdist_wheel

upload:
	twine upload dist/*

add:
	git add --all

clean:
	git clean -xdf
