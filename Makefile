build: build_without_symlink 
	ln -sf ./dist/build/hollaback/hollaback bin/hollaback

build_without_symlink: bundle configure
	cabal build

clean:
	cabal clean
	rm -f bin/hollaback

configure:
	cabal configure

bundle: Gemfile Gemfile.lock
	bundle install
quick_spec:
	runhaskell HollaBack/Testing/Main.hs
