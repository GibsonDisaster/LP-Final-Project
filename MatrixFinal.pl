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

/*
Multiply a matrix by a scalar
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
*/
