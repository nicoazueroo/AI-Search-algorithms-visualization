:- use_module(library(plunit)).
:- consult('informed.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prefix(Prefix, List) is true when List begins with Prefix.
prefix([], _).
prefix([H|T1], [H|T2]) :- prefix(T1, T2).

% Floating‑point comparison (hex grids)
within_eps((X1,Y1), (X2,Y2), Eps) :-
    abs(X1 - X2) =< Eps,
    abs(Y1 - Y2) =< Eps.

path_within_eps([], [], _).
path_within_eps([H1|T1], [H2|T2], Eps) :-
    within_eps(H1, H2, Eps),
    path_within_eps(T1, T2, Eps).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BEGIN INFORMED ALGORITHM TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(informed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECTANGULAR GRID TESTS  (now using prefix/2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(pbfs_rect_bfs) :-
    generate_rec_grid(5, 5, 1),
    once(pbfs_show(bfs, (0,0), (4,4), Path)),
    prefix([(0,0),(0,1),(0,2),(0,3),(0,4)], Path).

test(pbfs_rect_ucs) :-
    generate_rec_grid(5, 5, 3),
    once(pbfs_show(ucs, (0,0), (4,4), Path)),
    prefix([(0,0),(0,1),(0,2),(0,3),(0,4)], Path).

test(pbfs_rect_gbf) :-
    generate_rec_grid(5, 5, 1),
    once(pbfs_show(gbf,(0,0),(4,4),Path)),
    prefix([(0,0),(0,1),(1,1),(1,2),(2,2)], Path).

test(pbfs_rect_ast) :-
    generate_rec_grid(5, 5, 1),
    once(pbfs_show(ast,(0,0),(4,4),Path)),
    prefix([(0,0),(0,1),(0,2),(0,3),(0,4)], Path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRIANGULAR GRID TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(pbfs_tri_bfs) :-
    generate_tri_grid(4,4,1.5),
    once(pbfs_show(bfs,(0,0),(4.5,5.196),Path)),
    Path == [(0,0),(0.75,1.299),(1.5,2.598),(2.25,3.897),(3,5.196),(4.5,5.196)].

test(pbfs_tri_ucs) :-
    generate_tri_grid(4,4,1.5),
    once(pbfs_show(ucs,(0,0),(4.5,5.196),Path)),
    Path == [(0,0),(0.75,1.299),(1.5,2.598),(2.25,3.897),(3,5.196),(4.5,5.196)].

test(pbfs_tri_gbf) :-
    generate_tri_grid(4,4,1.5),
    once(pbfs_show(gbf,(0,0),(4.5,5.196),Path)),
    Path == [(0,0),(0.75,1.299),(1.5,2.598),(2.25,3.897),(3.75,3.897),(4.5,5.196)].

test(pbfs_tri_ast) :-
    generate_tri_grid(4,4,1.5),
    once(pbfs_show(ast,(0,0),(4.5,5.196),Path)),
    Path == [(0,0),(0.75,1.299),(1.5,2.598),(2.25,3.897),(3.75,3.897),(4.5,5.196)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEXAGONAL GRID TESTS (approx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(pbfs_hex_bfs) :-
    generate_hex_grid(3,3,1.5),
    once(pbfs_show(bfs,(3.0,0.0),(6.0,2.598076211353316),Path)),
    Expected = [(3.0,0.0),(3.75,1.299038105676658),(3.0,2.598076211353316),
                (3.749999999999999,1.2990381056766584),
                (5.25,1.299038105676658),(6.0,2.598076211353316)],
    path_within_eps(Path, Expected, 0.001).

test(pbfs_hex_ucs) :-
    generate_hex_grid(3,3,1.5),
    once(pbfs_show(ucs,(3.0,0.0),(6.0,2.598076211353316),Path)),
    Expected = [(3.0,0.0),(3.75,1.299038105676658),(3.0,2.598076211353316),
                (3.749999999999999,1.2990381056766584),
                (5.25,1.299038105676658),(6.0,2.598076211353316)],
    path_within_eps(Path, Expected, 0.001).

test(pbfs_hex_gbf) :-
    generate_hex_grid(3,3,1.5),
    once(pbfs_show(gbf,(3.0,0.0),(6.0,2.598076211353316),Path)),
    Expected = [(3.0,0.0),(3.75,1.299038105676658),(3.0,2.598076211353316),
                (3.7500000000000004,3.897114317029974),
                (5.25,3.897114317029974),(6.0,2.598076211353316)],
    path_within_eps(Path, Expected, 0.001).

test(pbfs_hex_ast) :-
    generate_hex_grid(3,3,1.5),
    once(pbfs_show(ast,(3.0,0.0),(6.0,2.598076211353316),Path)),
    Expected = [(3.0,0.0),(3.75,1.299038105676658),(3.0,2.598076211353316),
                (3.7500000000000004,3.897114317029974),
                (5.25,3.897114317029974),(6.0,2.598076211353316)],
    path_within_eps(Path, Expected, 0.001).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PYTHAGOREAN GRID TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(pbfs_pyt_bfs) :-
    generate_pyt_grid(3,3,1),
    once(pbfs_show(bfs,(0,0),(6,4),Path)),
    Path == [(0,0),(2,0),(2,1),(4,1),(4,2),(4,4),(6,4)].

test(pbfs_pyt_ucs) :-
    generate_pyt_grid(3,3,1),
    once(pbfs_show(ucs,(0,0),(6,4),Path)),
    Path == [(0,0),(2,0),(2,1),(4,1),(4,2),(4,4),(6,4)].

test(pbfs_pyt_gbf) :-
    generate_pyt_grid(3,3,1),
    once(pbfs_show(gbf,(0,0),(6,2),Path)),
    Path == [(0,0),(2,0),(2,1),(4,1),(4,2),(6,2)].

test(pbfs_pyt_ast) :-
    generate_pyt_grid(3,3,1),
    once(pbfs_show(ast,(0,0),(6,2),Path)),
    Path == [(0,0),(2,0),(2,1),(4,1),(4,2),(6,2)].

:- end_tests(informed).


% RUN THE TESTS -> ['informed_tests.plt']. -> run_tests(informed).
