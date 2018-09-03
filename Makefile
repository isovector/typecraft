all:
	stack build :typecraft --fast -j 4
	stack exec typecraft

profile:
	stack build :typecraft --work-dir=.stack-prof --profile -j4 --ghc-options="-rtsopts"
	stack exec --work-dir=.stack-prof typecraft -- +RTS -p

optimized:
	stack build :typecraft -j4 --ghc-options="-O2"
	stack exec typecraft

core:
	stack build :typecraft -j4 --ghc-options="-ddump-simpl -dsuppress-all"
	find .stack-work -name '*simpl'
