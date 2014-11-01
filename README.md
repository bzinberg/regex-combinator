Regex Combinator Language
=========================

Herein lies a combinator language for matchers which compiles down to POSIX
regular expressions (both standards &mdash; BREs and EREs &mdash; are
supported).

Why
---
The need for a clean combinator language arises because regular expression
syntax is so bad that writing down a regular expression (let alone checking
that a regular expression has the intended meaning) takes much more effort than
expressing the desired matcher in plain English.  As Prof. Gerald Sussman
explains:

> Regular expressions are ubiquitous.  On the surface, regular
> expressions look like a combinator language, because expression
> fragments can be combined to make more complex expressions.  But the
> meaning of a fragment is highly dependent on the expression it is
> embedded in.  For example, to include a caret character in a bracket
> expression, [...], it must not be in the first character position.
> If the caret appears after the first character it is just an ordinary
> character, but if it appears as the first character it negates the
> meaning of the bracket expression.  For example, a bracket expression
> may not contain just a caret.


Whereas, expressed a clean language like this one, the meaning of the matcher
is _more_ clear than it would be in plain English.  And because the matcher can
be compiled to a POSIX regular expression, standard tools like `grep` can still
be used.

This matcher supports all features of POSIX regular expressions, including
backreferences (implemented by labels rather than asking you to magically keep
track of parenthesis counts).

For a detailed specification, as well as some entertaining prose by Gerald
Sussman, see `ps.txt`.

Where
-----
To use, load `ps01.scm`.  Examples can be found in `ps.txt`, as well as in the
test suite `regexptest.scm`.

This project was an assignment for 6.945 Adventures in Advanced Symbolic
Programming at MIT.  A set of accompanying questions, as well as a
specification of the combinator language, can be found in `ps.txt`.  The
top-level include, as well as answers to the accompanying questions, are found
in `ps01.scm`.  (Some starter code was originally provided but it has all been
rewritten, except for the Scheme interface to `grep`.)

