% Facts.
man(samed).
man(erkam).
woman(elif).
woman(ece).
loves(_, ice_cream). % _ means everything

% Rules.
% Right hand side is the if clause, left hand side is the then part.
hasMustache(X) :- man(X).

% ; means or
human(X) :- man(X) ; woman(X).

% , means and
couple(samed, elif).
% Anything that starts with an uppercase letter or underscore is a variable.
couple(X, Y) :- couple(Y, X).

fac(0, 1).
fac(N, X) :- N > 0, M is N - 1, fac(M, Y), X is Y * N.

% returns the first element in a list
first([X|_],X).
