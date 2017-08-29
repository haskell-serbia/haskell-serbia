#!/bin/bash

sudo chmod a+x haskell-serbia
sudo mv haskell-serbia /var/www/html/haskell-serbia/haskell-serbia_bin
sudo killall haskell-serbia_bin
sleep 3
cd /var/www/html/haskell-serbia/
sudo cp haskell-serbia-sqlite.sqlite3 /var/www/html/
sudo git stash
sudo git pull origin master
sudo cp /var/www/html/haskell-serbia-sqlite.sqlite3 /var/www/html/haskell-serbia
nohup sudo ./haskell-serbia_bin > /dev/null  < /dev/null &
