all: build

setup:
	set -e
	npm install
	bower install

build: setup
	npm run -s build

test: setup
	npm run -s test

# since `test` is also a directory, it will not be built without this
# (or `make -B test`):
.PHONY: test

clean:
	npm run -s clean

zip-src:
	rm -f decosim-ps.zip
	git archive -o decosim-ps.zip HEAD

run:
	firefox public/index.html
