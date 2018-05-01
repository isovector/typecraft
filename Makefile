all:
	stack build :typecraft --fast -j 4
	stack exec typecraft
