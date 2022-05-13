shell:
	guix time-machine -C channels.scm -- shell --pure -D -f guix.scm

all: readme package upload add clean

readme:
	emacs -Q --script process-readme.el

package:
	python3 setup.py sdist bdist_wheel

upload:
	twine upload dist/*

add:
	git add --all

clean:
	git clean -xdf
