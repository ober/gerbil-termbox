PACKAGE = gerbil-termbox
PREFIX = $(HOME)/.gerbil

build:
	gerbil build

clean:
	gerbil clean

test: build
	gerbil build --test

install: build
	mkdir -p $(PREFIX)/lib/$(PACKAGE) $(PREFIX)/lib/static
	cp -f .gerbil/lib/$(PACKAGE)/* $(PREFIX)/lib/$(PACKAGE)/
	cp -f .gerbil/lib/static/$(subst /,__,$(PACKAGE))__* $(PREFIX)/lib/static/

.PHONY: build clean test install
