# scrypt
scrypt is a simple dynamically-typed interpreted language.


## Example
```
write! "Input a number: "
number: int? readln?
if (typeof? number) :: "error"
    end "Couldn't parse input..."

if number < 10
    writeln! "Number is less than 10!"

if number :: 10
    writeln! "Number is equal to 10!"

if number > 10
    writeln! "Number is more than 10!"

if number % 2 :: 0
    writeln! "Number is divisible by 2!"
```
