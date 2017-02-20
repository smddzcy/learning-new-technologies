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
head([X|_],X).

tail([_|X], X).

last([X], X).
last([_|X], Y) :- last(X, Y).

el_before_last([X, _], X).
el_before_last([_|X], Y) :- el_before_last(X, Y).


element_at(1, [H|_], H).
element_at(N, [_|T], X) :- element_at(M, T, X), N is M + 1.

my_length([_], 1).
my_length([_|T], N) :- my_length(T, M), N is M + 1.

my_append([], X, X).
my_append([H|T], L, [H|L1]) :- my_append(T,L,L1).

my_reverse([], []).
my_reverse([H|T], L) :- my_reverse(T, L1), my_append(L1, [H], L).
