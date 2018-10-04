all: build

setup:
	set -e
	npm install
	psc-package build

build: #setup
	npm run build

test: #setup
	npm run test

# since `test` is also a directory, it will not be built without this
# (or `make -B test`):
.PHONY: test

clean:
	npm run clean

zip-src:
	rm -f decosim-ps.zip
	git archive -o decosim-ps.zip HEAD

run:
	firefox public/index.html
