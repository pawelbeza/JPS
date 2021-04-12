
% Przyk³ad zastosowania procedury my_trace_rec:
% œledzenie wykonania procedury  path.
%
% Dodatkowy argument: Level--poziom rekurencji do komunikatów œledzenia
%
% Przyk³adowe wywo³anie  path
% z inicjalizacj¹ dodatkowego argumentu (pocz. poziom rekurencji 0):
%
%  path(a,d,Path,0).
%
% Po ka¿dym komunikacie œledzenia wykonania nale¿y wprowadziæ:
% <dowolny znak>  <kropka>  <ENTER>
% dla procedury wbudowanej read


path(Y,Y,[Y], Level):-
	my_trace_rec(4,path,1,Level,['Y'/Y]).

path(X,Y,[X|RestPath], Level)  :-
	X\=Y,
	my_trace_rec(1,path, 2,Level,['X'/X,'Y'/Y]),
        my_trace_rec(2,path, 2,Level,arc),
	arc(X,Z),
	my_trace_rec(3,path, 2,Level,arc,['Z'/Z]),
	NewLevel is Level + 1,
	path(Z,Y,RestPath, NewLevel),
	my_trace_rec(4,path,2,Level,['RestPath'/RestPath]).


arc(a, e).
arc(a, b).
arc(a, c ) .
arc(b, d).
arc( c, d) .


