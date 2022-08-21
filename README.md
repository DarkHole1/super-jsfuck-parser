Super JSFuck parser β
=====
## Features
* Honestly parses JSFuck code: no hacks with string manipulation
* Simplifies it: you can ever get you code without executing JSFuck directly
* Written on Elm: no side effects guarantied by compiler

## How to compile and run locally
There is precompiled version in `dist` folder, but if you want to compile yourself (e.g. after changes):
1. Install elm
```bash
npm install --global elm
```
2. Compile with elm
```bash
elm make src/Main.elm --output=dist/index.js
```
3. If error occured on previous step, make issue/PR :)

## TODO
* More rules for simplifing
    * In fact modern JSFuck uses arrays with length > 1 - but there is no support for it in code
    * It uses regex too
* Documentation
    * Most of parser/simplifier stuff is undocumented
* Error messages improvement (now they're not useful even for devs) 
* Overall UI improvements