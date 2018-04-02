HASKELL_FILES = $(shell find haskell)
HAKYLL = $(shell stack path --local-install-root)/bin/hakyll

all: deploy

$(HAKYLL): $(HASKELL_FILES)
	stack setup
	stack build
	stack exec hakyll -- clean

build: $(HAKYLL)
	stack exec hakyll -- build

deploy: build
	stack exec hakyll -- deploy

clean:
	stack exec hakyll -- clean

redeploy: clean deploy
