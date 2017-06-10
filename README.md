# [haskell-serbia](http://haskell-serbia.com)


### Serbian Haskell user group website
We aim to provide  tutorials that will help beginners in starting out with haskell and a community for helping each other on local scale, organize meetups and workshops and just socializing.

#### Installation

To install locally you can do `stack build` and then `stack exec -- yesod devel`
Website runs on postgres database, there is a sql dump file in the root of the repo which you can import.
```
$ psql haskell_serbia < haskell_serbia.sql 
```
After import is finished you can login as admin user. Login email is `test@test.com` and password is `test`
#### Contributors are welcome!
We are looking forward to any community contributions 
