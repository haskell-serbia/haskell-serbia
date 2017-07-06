build:
	stack build --fast

echo-warn:
	echo "Make certain you have fired up postgres  !"

test: echo-warn
	stack test

static:
	touch Settings/StaticFiles.hs

ghci:
	stack ghci haskell-serbia:lib

devel:
	stack exec -- yesod devel

ghci-object:
	stack ghci --ghci-options -fobject-code haskell-serbia:lib
