# Numbers

```
2.

20 factorial.

1000 factorial / 999 factorial.

(1/3).

(1/3) + (4/5).

(1/3) asFloat.

1 class.
```

# Characters

A Character can be instantiated using `$` operator:

```
$A.

$A class.

$B charCode.

Character cr.

Character space.
```

# Strings

```
'smddzcy'.

'smddzcy' size.

'abc' asUppercase.

'Hello World' reverse.
```

You can access each character using `at: message`

```
'smddzcy' at: 1.
```

# Symbols

Symbol is a String which is guaranteed to be globally unique.

```
'smddzcy' asSymbol.

#smddzcy asString.

(2 asString) == (2 asString).

(2 asString) asSymbol == (2 asString) asSymbol.
```

# Arrays

Literal arrays are created at parse time:

```
#(1 2 3).

#( 1 2 3 #(4 5 6)) size.

#(1 2 4) isEmpty.

#(1 2 3) first.

#('hello' 'World') at: 2 put: 'wow'; yourself.
```

Dynamic Arrays are created at execution time:

```
{ (2+3) . (6*6) . (3+5) }.
```

# Useful Array Methods

`collect: block` maps each element of the array with the given block.
`select: pred-block` selects the elements that matches the given predicate block from the array.
`reject: pred-block` removes the elements that matches the given predicate block from the array.

|Smalltalk|Haskell|
|:---:|:---:|
|`xs collect: fn`|map fn xs|
|`xs select: pred`|filter pred xs|
|`xs reject: pred`|filter (not . pred) xs|

```
#(11 38 3 -2 10) do: [:each |
     Transcript show: each printString; cr].

#(11 38 3 -2 10) collect: [:each | each abs].

#(11 38 3 -2 10) collect: [:each | each odd].

"select = filter"
#(11 38 3 -2 10) select: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each > 10].

"reject = remove"
#(11 38 3 -2 10) reject: [:each | each > 10].

#(11 38 3 -2 10)
     do: [:each | Transcript show: each printString]
     separatedBy: [Transcript show: '.'].
```

# Messages

Messages are sent to objects.
There are three types of message: **Unary**, **Binary** and **Keyword**.

Unary messages have the following form: `anObject aMessage`

```
1 class.

false not.

Time now.

Date today.
```

Binary messages have the following form: `anObject infixOp anotherObject`

```
3 * 2.

Date today + 3 weeks.

false | false.

'hey', ' wow'.
```

Keyword messages are messages with arguments: `anObject akey: anotherObject akey2: anotherObject2`

```
4 between: 0 and: 10.

1 max: 3.

Color r:1 g:0 b:0.
```

Unary messages are executed first, then binary messages and finally keyword messages: **Unary > Binary > Keywords**

```
2 + 3 squared.

2 raisedTo: 3 + 2.

-3 abs negated reciprocal.
```

`;` is the cascade operator. It's useful to send messages to the **SAME** receiver:

```
Transcript
       open;
	   show: 'Hello, ';
	   show: 'World!' ;
	   cr.
```

# Blocks (Lambda Functions)

Blocks are anonymous methods that can be stored into variables and executed on demand.

```
[:x | x+2].
```

We can execute a block by sending it `value` messages.

```
[:x | x+2] value: 5.

[:x :y| x + y] value:3 value:5.

[Workspace open] value.
```

Blocks can be assigned to a variable then executed later.

```
|b|
b := [:x | x+2].
b value: 12.
```

# Conditionals

Conditionals are just messages sent to `Boolean` objects

```
1 < 2
  ifTrue: [100]
  ifFalse: [42].
```

# Loops

```
1 to: 100 do:
  [:i | Transcript show: i asString; cr ].

1 to: 100 by: 3 do: [:i | Transcript show: i asString; cr].
```

# Instantiation

Objects are instances of their class. Usually, we send the message `#new` to a class for creating an instance of this class.

```
SimpleButtonMorph new
	label: 'A nice button';
	openCenteredInWorld.
```

# Reflection

Take a look at method `#ifFalse:ifTrue:` source code of class `True`:

```
(True>>#ifFalse:ifTrue:) definition.
```

See all the methods of a class:

```
SmallInteger selectors.
```

Explore the object:

```
Date today explore.
```

Browse the code:

```
Date browse.
```
