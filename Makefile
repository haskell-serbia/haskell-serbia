build:
	stack build --fast

copy-bins:
	stack build --copy-bins --local-bin-path bin

echo-warn:
	echo "Make certain you have fired up postgres  !"

test: echo-warn
	stack test

static:
	touch Settings/StaticFiles.hs

ghci:
	stack ghci haskell-serbia:lib

dev:
	stack exec -- yesod devel

ghci-object:
	stack ghci --ghci-options -fobject-code haskell-serbia:lib
