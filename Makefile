CABAL=cabal-dev
EXPORTS=PATH=$$PATH:cabal-dev/bin

build: build_without_symlink 
	ln -sf ./dist/build/hollaback/hollaback bin/hollaback

build_without_symlink: configure
	$(CABAL) build

clean:
	$(CABAL) clean
	rm -f bin/hollaback

configure: install_dependencies
	$(CABAL) configure

install_dependencies:
	$(CABAL) install --only-dependencies

spec:
	runhaskell HollaBack/Testing/Main.hs
