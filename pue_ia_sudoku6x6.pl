% render solutions nicely.
:- use_rendering(sudoku).

:- use_module(library(clpfd)).

% Example by Markus Triska, taken from the SWI-Prolog manual.

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C), blocks(D, E, F), blocks(G, H, I).

% Modificado el anterior predicado para sudokus 6x6
sudoku6x6(Rows) :-
        length(Rows, 6), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..6,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F],
        blocks6x6(A, B), blocks6x6(C, D), blocks6x6(E, F).
/* Se puede ejecutar así para resolver el problema 6
 * ?- time((problem(6, Filas), sudoku6x6(Filas))).
 * si queremos ver el tiempo que tarda o también, sin el tiempo
 * ?- problem(6, Filas), sudoku6x6(Filas).
 * */
blocks6x6([], []).
blocks6x6([A,B,C|Bs1], [D,E,F|Bs2]) :-
        all_distinct([A,B,C,D,E,F]),
        blocks6x6(Bs1, Bs2).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).
printStudentName() :-
	print("Ejecución de Jesús Martínez Herrero para el curso de Introducción a la Inteligencia Artificial").

problem(1, [[_,_,_, _,_,_, _,_,_],
            [_,_,_, _,_,3, _,8,5],
            [_,_,1, _,2,_, _,_,_],

            [_,_,_, 5,_,7, _,_,_],
            [_,_,4, _,_,_, 1,_,_],
            [_,9,_, _,_,_, _,_,_],

            [5,_,_, _,_,_, _,7,3],
            [_,_,2, _,1,_, _,_,_],
            [_,_,_, _,4,_, _,_,9]]).

% Run script 
% problem(1, Rows), sudoku(Rows), printStudentName