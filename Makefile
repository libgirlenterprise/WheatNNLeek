# Copyright (c) 2019 Libgirl
#
# Released under Apache 2.0 license as described in the file LICENSE.txt.

CARGO ?= cargo

all: debug

.PHONY: debug
debug:
	cd src/core; $(CARGO) build

.PHONY: release
release:
	cd src/core; $(CARGO) build --release

.PHONY: clean
clean:
	cd src/core; $(CARGO) clean
.PHONY: test
test:
	cd src/core; $(CARGO) test
	cd src/cl-wheatnnleek-cffi/; rove cl-wheatnnleek-cffi.asd