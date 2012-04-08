build: build_without_symlink 
	ln -sf ./dist/build/hollaback/hollaback bin/hollaback

build_without_symlink: configure
	cabal build

clean:
	cabal clean
	rm -f bin/hollaback

configure:
	cabal configure

spec:
	runhaskell HollaBack/Testing/Main.hs
