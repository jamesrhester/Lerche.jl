# Introduction

Lerchen (German for Lark) is a partial port of the Lark grammar processor from
Python to Julia.  Lark grammars should work unchanged in Lerchen.


# Quick start

Please read the Lark documentation.  When converting from Lark programs written
in Python to Lerchen programs written in Julia, the following changes are necessary:

1. All classes become types
2. All class method calls become Julia method calls with the type as the first argument
(i.e. replacing ``self``)
3. Transformers and visitors should be declared as subtypes of the appropriate
visitor/transformer type
4. Transformation or visitor rules should be preceded by the ``@rule`` macro. Inline
rules use the ``@inline_rule`` macro. 
5. The first argument of transformation and visitor rules is a variable of the
desired transformation/visitor type.
6. Any grammars containing backslash-double quote sequences need to be fixed (see below).
7. Any grammars containing backslash-x to denote a byte value need to be fixed (see below).

# Grammars

Lark grammars should work unchanged in Lerchen, with the caveats
below.  Note that this guarantee applies only to the sequence of
characters after interpretation by the Julia/Python language parser.
In particular note the following differences:

1. Raw strings in Julia are written ``raw"<string contents>"`` instead of 
Python's ``r"<string contents>"``

2. The sequence ``\"`` inside a Python raw, quote-delimited string
encodes a two-character sequence.  However, it corresponds to a single
quote in the analogous Julia case. To obtain the two-character
sequence in Julia, input ``\\"``. A backslash-quote sequence is
required in Lark grammars when representing a double quote as the Lark
grammar defines the sequence backslash-quote to represent a quote; so
these two characters must remain in the string after Julia has 
pre-processed it.

3. While unicode escapes are recognised (backslash-u), the Python
backslash-x combination to insert a particular byte value in the
string is not.

4. Avoid using Julia keywords (such as ``true`` or ``false``) as the
names of rules or aliases.  If your Lark grammar does this, you will
need to change it.

# Examples



# Inconsistencies with Lark

1. Earley and CYK grammars are not implemented. 
2. Dynamic lexer is not implemented. 
3. All errors with messages attached must be at the bottom of the
exception type hierarchy, as these are the only types that can have
contents. Thus an "UnexpectedInput" exception must become e.g 
an UnexpectedCharacter if a message is included.

# Implementation notes

Lerchen is currently noticeably slower than Lark, despite the
advantages of Julia's compilation. There is still plenty of room for
improvement as no effort has been made to use Julia efficiency tricks.
The priority has been on maintaining fidelity with Lark.

Python "yield" has been implemented using Julia Channels.
