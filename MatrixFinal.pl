:- use_module(library(clpfd)).

/*
Matrix - [Rows Cols [...] [...] ...]

Vector - [X Y Z ...]
*/

/*
element/4 retrieves an element from the matrix at a given row and column position
R1 - Number of rows in given matrix (used to verify element will exist)
C1 - Number of columns in given matrix (used to verify element will exist)
M - Matrix represented as a list of form (Row1 Row2 ...)
R - Row requested element lies in
C - Column requested element lies in
E - Element, unified with requested element if found
*/
element([R1, C1 | M], R, C, E) :- 
  ( R > R1, C > C1 ->
    throw("invalid row or column") % Invalid row or colw
  ;
    getrow(M, R, Col),
    getcolfromrow(Col, C, E)). % Rest of operation

% Helpers for element/4
getcolfromrow([H|_], 1, H).
getcolfromrow([_|T], C, E) :-
  C1 is C - 1,
  getcolfromrow(T, C1, E).

getrow([H|_], 1, H).
getrow([_|T], R, E) :-
  R1 is R - 1,
  getrow(T, R1, E).

getcol(ColNumber, Matrix, Col) :- getcolhelper(ColNumber, Matrix, [], Col).

getcolhelper(_, [], Collector, Collector) :- !.
getcolhelper(ColNumber, [CurrentRow|Rest], Collector, Col) :-
  nth0(ColNumber, CurrentRow, CurrentEntry),
  append(Collector, [CurrentEntry], NewCollector),
  getcolhelper(ColNumber, Rest, NewCollector, Col).

/*
Multiply a matrix by a scalar
R - Number of rows in matrix
C - Number of columns in matrix
T - Rows of the matrix
S - Scalar to multiply matrix
MS - Scalar multiplied matrix
*/
scalar_multiply([R, C|T], S, MS) :- scalar_mul_helper(T, S, [], MS1), append([R, C], MS1, MS).

scalar_mul_helper([], _, MS, MS).
scalar_mul_helper([H|T], S, MS, RESULT) :-
  scalar_helper(H, S, [], R),
  append([R], MS, MS1),
  scalar_mul_helper(T, S, MS1, RESULT).

scalar_helper([], _, RS, RS).
scalar_helper([H|T], S, RS, RR) :-
  N is H * S,
  append([N], RS, RS1),
  scalar_helper(T, S, RS1, RR).

/*
Create the square identity matrix of a given dimension
D - Size of identity matrix
M - Identity matrix of size D
*/
identity(D, M) :- 
  create_row(D, [], R),
  sqr_helper(D, R, [], MM),
  msort(MM, Ms),
  append(Ms, [D, D], Ms1),
  reverse(Ms1, M).

sqr_helper(0, _, MM, MM).
sqr_helper(D, L, MM, M) :-
  shuffle_to_back(L, L1),
  append([L1], MM, M1),
  D1 is D - 1,
  sqr_helper(D1, L1, M1, M).

create_row(1, Collector, R) :- append(Collector, [1], R), !.
create_row(N, Collector, R) :-
  append([0], Collector, R1),
  N1 is N - 1,
  create_row(N1, R1, R).

shuffle_to_back([H|T], R) :- append(T, [H], R).

/*
Dot product of two vectors
H1 - First entry of first vector
T1 - Rest of first vector
H2 - First entry of second vector
T2 - Rest of second vector
Result - Dot product of the two vectors
*/
dot([],[], 0).
dot([H1|T1], [H2|T2], Result) :-
  Product is H1 * H2,
  dot(T1, T2, Result1),
  Result is Product + Result1.

/*
Determinate of a 2x2 matrix
A1 - Top left entry
A2 - Top right entry
B1 - Bottom left entry
B2 - Bottom right entry
*/
det_of_2x2([2, 2, [A1, B1|[]], [A2, B2|[]]], Det) :-
  Diag1 is A1 * B2,
  Diag2 is A2 * B1,
  Det is Diag1 - Diag2, !.

/*
Determinate of an nxn square matrix
*/

/*
Matrix Multiplication
Rows - Number of rows in first matrix
M1 - First matrix
C - Number of columns in second matrix
M2 - Second matrix
R - Result
*/
matrix_mul([Rows, _ | M1], [_, C | M2], R) :- for_each_col(M1, M2, Rows, 0, [], Result), clump_up(C, Result, Cols), transpose(Cols, Cols2), append([Rows, C], Cols2, R), !.%mul_helper2(M1, M2, 0, [], R1), mul_helper2(M1, M2, 1, R1, R2), mul_helper2(M1, M2, 2, R2, R).

for_each_col(_, _, Rows, Rows, Collector, Collector).
for_each_col(M1, M2, Times, Position, Collector, R) :-
  mul_helper2(M1, M2, Position, Collector, R1),
  Pos is Position + 1,
  for_each_col(M1, M2, Times, Pos, R1, R).

mul_helper2([], _, _, Collector, Collector).
mul_helper2([R|Rest], Matrix2, ColCounter, Collector, Result) :-
  getcol(ColCounter, Matrix2, Col),
  dot(R, Col, Dot),
  append(Collector, [Dot], NewCollector),
  mul_helper2(Rest, Matrix2, ColCounter, NewCollector, Result).

clump_up(_, [], []) :- !.
clump_up(Size, List, NewList) :- 
  length(X, Size),
  append(X, Y, List),
  clump_up(Size, Y, Rest),
  append([X], Rest, NewList).