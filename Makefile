build:
	stack build --fast -j4

build-watch:
	stack build --fast --file-watch

copy-bins:
	stack build --copy-bins --local-bin-path bin

echo-warn:
	echo "Testing in progress"

test: echo-warn
	stack test -j4

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

copy-remote:
	scp -i  ~/Documents/haskell-serbia/haskell-serbia.pem.txt ~/code/haskell-serbia/bin/haskell-serbia  ubuntu@ec2-34-195-52-222.compute-1.amazonaws.com:/home/ubuntu

deploy-bin: build copy-bins
	sudo ./deploy.sh

ssh-aws:
	ssh -i ~/Documents/haskell-serbia/haskell-serbia.pem.txt  ubuntu@ec2-34-195-52-222.compute-1.amazonaws.com
