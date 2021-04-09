# Lerche.jl: Generating parsers from EBNF grammars

# Summary

Extended Backhaus-Naur Format (EBNF) is a well-established standard
for describing the syntax of structured text []. In a scientific
context, such text is commonly encountered in data files and custom
domain-specific languages (DSLs). When an EBNF description of such a
language or data file is available, that description can be used by
Lerche.jl to automatically generate a parser that processes arbitrary
texts constructed in accordance with that EBNF. In addition, methods 
for transformation of the result of parsing into arbitrary form may
be specified.

# Statement of Need

Standards are the way in which data transfer formats are agreed
between parties not otherwise in contact.  Use of formal description
languages, such as EBNF, remove ambiguity that may arise when using
natural language descriptions. However, when software authors then
manually interpret a given EBNF into custom code, the correctness of
their interpretation is difficult to verify. This issue is magnified
when a single such program is considered the "gold standard" for
determining the correctness of data file construction. A parser 
that is created by automatically ingesting an EBNF
description is, by contrast, guaranteed to be correct.  

For most In a scientific context, where large data files or rapid execution is
important

EBNF parsers exist in a variety of languages [wikipedia]. Generally,
generated parsers will either be produced in the language in which the
EBNF parser was written, or will produce source code in a requested
target language, which must be then compiled or interpreted as a
separate step when incorporated into a larger project.


Data formats and domain-specific languages often
develop in tandem with their formal specification.  Verification of a
formal specification relies upon trust in software that implements
that specification.

For example, the CIF format [] provides
an EBNF specification. As the EBNF is intended to fully specify a
language, a machine-readable EBNF grammar should be sufficient 
for a parser to be automatically produced. A generated parser that
produces the expected results from a series of input files serves to
validate both the EBNF grammar, and provides a standard against which
to compare other parsers. With increasing emphasis on reproducible
data handling workflows, such validation has become more important.

Parser generators that ingest EBNF formats sufficiently similar are 
surprisingly rare.

# Acknowledgements

# References
