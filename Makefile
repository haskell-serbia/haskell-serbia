build:
	stack build --fast

echo-warn:
	echo "Make certain you have fired up postgres  !"

test: echo-warn
	stack test

static:
	touch Settings/StaticFiles.hs

ghci:
	stack ghci
