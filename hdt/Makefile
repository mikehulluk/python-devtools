
.PHONEY: run build check run

default: build

build:
	cabal configure
	cabal build

check:
	 cabal build --ghc-options='-fno-code'

run: build
	./dist/build/hdt/hdt modereplace -- 'import' '--import'

clean:
	cabal clean
