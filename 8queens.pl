:- use_module(library(clpfd)).

queenstate(X) :-
	permutation([0,1,2,3,4,5,6,7],X).

nc(_,A,A).
nc(X,A,B) :-
	nth0(A,X,AV),
	nth0(B,X,BV),
	D #= B-A,
	BV #\= AV + D,
	BV #\= AV - D.

loop(N) :- for(I,0,N),param(N) do
               for(J,0,N),param(I) do 
                      write(foo(I,J)),nl.