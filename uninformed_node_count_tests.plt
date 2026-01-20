:- begin_tests(uninformed_node_count).

:- ensure_loaded('uninformed.pl').

test(dfs_count_rect) :-
    reset_explored_count,
    generate_rec_grid(5,5,1),
    dfs((0,0), (4,4), _Path),
    get_explored_count(N),
    assertion(N > 0),
    format('DFS RECT explored ~w nodes.~n', [N]).

test(dls_count_rect) :-
    reset_explored_count,
    generate_rec_grid(5,5,1),
    dls((0,0), (4,4), 10, _Path),
    get_explored_count(N),
    assertion(N > 0),
    format('DLS RECT explored ~w nodes.~n', [N]).

test(ids_count_rect) :-
    reset_explored_count,
    generate_rec_grid(5,5,1),
    ids((0,0), (4,4), _Path),
    get_explored_count(N),
    assertion(N > 0),
    format('IDS RECT explored ~w nodes.~n', [N]).

test(dfs_count_tri) :-
    reset_explored_count,
    generate_tri_grid(5,5,1.5),
    dfs((0,0), (4.5, 5.196), _Path),
    get_explored_count(N),
    assertion(N > 0),
    format('DFS TRI explored ~w nodes.~n', [N]).

test(dls_count_tri) :-
    reset_explored_count,
    generate_tri_grid(5,5,1.5),
    dls((0,0), (4.5, 5.196), 10, _Path),
    get_explored_count(N),
    assertion(N > 0),
    format('DLS TRI explored ~w nodes.~n', [N]).

test(ids_count_tri) :-
    reset_explored_count,
    generate_tri_grid(5,5,1.5),
    ids((0,0), (4.5, 5.196), _Path),
    get_explored_count(N),
    assertion(N > 0),
    format('IDS TRI explored ~w nodes.~n', [N]).


:- end_tests(uninformed_node_count).


% RUN THE TESTS -> ['uninformed_node_count_tests.plt']. -> run_tests(uninformed_node_count). 
