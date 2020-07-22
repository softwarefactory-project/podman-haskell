VER := $(shell awk '/^version: / { print $$2 }' *.cabal)
NAM := $(shell awk '/^name: / { print $$2 }' *.cabal)
DST := "pagesuser@www.softwarefactory-project.io:/var/www/pages/docs.softwarefactory-project.io/podman-haskell/"

doc:
	@stack haddock

publish: doc
	@rsync --delete -avi $(shell stack path --local-doc-root)/$(NAM)-$(VER)/ $(DST)
