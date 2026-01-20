:- module(uninformed_tests, []).
:- use_module(library(plunit)).
:- consult('uninformed.pl').

%   Helpers for testing

% Checks whether Sub is a prefix of List
sublist(Sub, List) :- append(Sub, _, List).

% Floating-point approximate equality
approx_equal(A, B, Tol) :-
    Diff is abs(A - B),
    Diff =< Tol.

% Approximate comparison of two points (X,Y)
approx_point((X1,Y1), (X2,Y2), Tol) :-
    approx_equal(X1, X2, Tol),
    approx_equal(Y1, Y2, Tol).

% Approximate comparison of two whole paths
approx_path([], [], _).
approx_path([H1|T1], [H2|T2], Tol) :-
    approx_point(H1, H2, Tol),
    approx_path(T1, T2, Tol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(uninformed).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%  RECT GRID TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%

test(dfs_rect) :-
    generate_rec_grid(5, 5, 1),
    once(dfs((0,0), (4,4), Path)),
    sublist([(0,0),(1,0),(2,0),(3,0),(4,0)], Path).

test(dls_rect) :-
    generate_rec_grid(5, 5, 1),
    once(dls((0,0), (4,4), 20, Path)),
    sublist([(0,0),(1,0),(2,0),(3,0),(4,0)], Path).

test(ids_rect) :-
    generate_rec_grid(5, 5, 1),
    once(ids((0,0), (4,4), Path)),
    sublist([(0,0),(1,0),(2,0),(3,0),(4,0)], Path).

%  TRIANGULAR GRID 

test(dfs_tri) :-
    generate_tri_grid(4, 4, 1.5),
    once(dfs((0,0), (4.5,5.196), Path)),
    sublist([(0,0),(1.5,0),(0.75,1.299)], Path).

test(dls_tri) :-
    generate_tri_grid(4, 4, 1.5),
    once(dls((0,0), (4.5,5.196), 20, Path)),
    sublist([(0,0),(1.5,0),(0.75,1.299)], Path).

test(dls_tri_exact) :-
    generate_tri_grid(4, 4, 1.5),
    once(dls((0,0), (4.5,5.196), 10, Path)),
    sublist([(0,0),(1.5,0),(0.75,1.299),(2.25,1.299),(3,0)], Path).

test(ids_tri) :-
    generate_tri_grid(4, 4, 1.5),
    once(ids((0,0), (4.5,5.196), Path)),
    approx_path(
        Path,
        [(0,0),(1.5,0),(2.25,1.299),(3,2.598),(3.75,3.897),(4.5,5.196)],
        0.001
    ).

%  HEX GRID TESTS 

test(dfs_hex) :-
    generate_hex_grid(3, 3, 1.5),
    once(dfs((3.0,0.0), (6.0,2.598076211353316), Path)),
    approx_path(
        Path,
        [
            (3.0,0.0),
            (3.75,1.299038105676658),
            (3.0,2.598076211353316),
            (3.75,3.897114317029974),
            (3.0,5.196152422706632),
            (3.749999999999999,3.8971143170299745),
            (5.25,3.897114317029974),
            (6.0,2.598076211353316)
        ],
        0.001
    ).

test(dls_hex) :-
    generate_hex_grid(5, 5, 1.5),
    once(dls((3.0,0.0), (6.0,2.598076211353316), 8, Path)),
    approx_path(
        Path,
        [
            (3.0,0.0),
            (3.75,1.299038105676658),
            (3.0,2.598076211353316),
            (3.75,3.897114317029974),
            (3.0,5.196152422706632),
            (3.749999999999999,3.8971143170299745),
            (5.25,3.897114317029974),
            (6.0,2.598076211353316)
        ],
        0.001
    ).

test(ids_hex) :-
    generate_hex_grid(5, 5, 1.5),
    once(ids((3.0,0.0), (6.0,2.598076211353316), Path)),
    approx_path(
        Path,
        [
            (3.0,0.0),
            (3.75,1.299038105676658),
            (3.0,2.598076211353316),
            (3.749999999999999,1.2990381056766584),
            (5.25,1.299038105676658),
            (6.0,2.598076211353316)
        ],
        0.001
    ).

%  PYTHAGOREAN GRID  

test(dfs_pyt) :-
    generate_pyt_grid(3, 3, 1),
    once(dfs((4,2), (4,3), Path)),
    Path == [(4,2),(4,1),(4,3)].

test(dls_pyt) :-
    generate_pyt_grid(3, 3, 1),
    once(dls((0,0), (4,4), 10, Path)),
    sublist([(0,0),(2,0),(2,1),(4,1),(4,2)], Path).

test(ids_pyt) :-
    generate_pyt_grid(3, 3, 1),
    once(ids((0,0), (4,4), Path)),
    sublist([(0,0),(2,0),(2,1),(4,1),(4,2),(4,4)], Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- end_tests(uninformed).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RUN THE TESTS -> ['uninformed_tests.plt']. -> run_tests(uninformed). 

