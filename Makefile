build:
	stack build --fast

build-watch:
	stack build --fast --file-watch

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

docker-run:
	docker run -it -p 3000:3000 haskell-serbia-docker /usr/local/bin/haskell-serbia-exe
