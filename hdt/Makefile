
.PHONEY: run build check run

default: build

build:
	cabal configure
	cabal build

check:
	 cabal build --ghc-options='-fno-code'

run: build
	rm -rf ~/.hdt/
	#./dist/build/hdt/hdt modereplace -- 'import' '--import'
	#./dist/build/hdt/hdt modereplace -- 'execApply' '--execApply'
	#./dist/build/hdt/hdt modereplace -- 'execApply' '--execApply'
	./dist/build/hdt/hdt modereplace -- 'patch' 'change'
	./dist/build/hdt/hdt modereplace -- 'execApply' '--execApply'
	./dist/build/hdt/hdt modereplace -- 'execApply' '--execApply'
	./dist/build/hdt/hdt modeapply



clean:
	cabal clean
