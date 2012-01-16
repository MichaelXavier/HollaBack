build: build_without_symlink
	echo "TODO: symlinking"

build_without_symlink: bundle configure
	cabal build

configure:
	cabal configure

bundle: Gemfile Gemfile.lock
	bundle install
quick_spec:
	runhaskell HollaBack/Testing/Main.hs
