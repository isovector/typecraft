all:
	stack build :typecraft --fast -j 4
	stack exec typecraft

profile:
	stack build :typecraft --work-dir=.stack-prof
	stack exec --work-dir=.stack-prof typecraft
