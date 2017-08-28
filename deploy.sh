
# scp -i  ~/Documents/haskell-serbia/haskell-serbia.pem.txt ~/code/haskell-serbia/bin/haskell-serbia  ubuntu@ec2-34-195-52-222.compute-1.amazonaws.com:/home/ubuntu
ssh -i ~/Documents/haskell-serbia/haskell-serbia.pem.txt  ubuntu@ec2-34-195-52-222.compute-1.amazonaws.com 'bash -s' < install.sh
