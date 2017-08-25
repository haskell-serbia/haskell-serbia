#!/bin/bash

sudo chmod a+x haskell-serbia
sudo mv haskell-serbia /var/www/html/haskell-serbia/haskell-serbia_bin
ps -ef | grep haskell | grep -v grep | awk '{print $2}' | xargs kill -9
cd /var/www/html/haskell-serbia/
nohup sudo ./haskell-serbia_bin &
