Super JSFuck parser β
=====
* Honestly parses JSFuck code: no hacks with string manipulation
* Simplifies it: you can ever get you code without executing JSFuck directly
* Written on Elm: no side effects guarantied by compiler

### TODO
* More rules for simplifing
    * In fact modern JSFuck uses arrays with length > 1 - but there is no support for it in code
    * It uses regex too
* Documentation
    * Most of parser/simplifier stuff is undocumented
* Error messages improvement (now they're not useful even for devs) 
* Overall UI improvements