:- begin_tests(informed_nodes, [cleanup(all_traces)]).

:- use_module(informed).
:- use_module(utils).

% Resetear contador y obtener explorados (debes tener estos definidos en informed.pl)
% reset_explored_count.
% get_explored_count(Count).

test(pbfs_rect_bfs_nodes) :-
    generate_rec_grid(5, 5, 1),
    reset_explored_count,
    pbfs_show(bfs, (0,0), (4,4), _),
    get_explored_count(Count),
    format("~n[RECT BFS] Nodes explored: ~w~n", [Count]).

test(pbfs_rect_ucs_nodes) :-
    generate_rec_grid(5, 5, 3),
    reset_explored_count,
    pbfs_show(ucs, (0,0), (4,4), _),
    get_explored_count(Count),
    format("~n[RECT UCS] Nodes explored: ~w~n", [Count]).

test(pbfs_rect_gbf_nodes) :-
    generate_rec_grid(5, 5, 1),
    reset_explored_count,
    pbfs_show(gbf, (0,0), (4,4), _),
    get_explored_count(Count),
    format("~n[RECT GBF] Nodes explored: ~w~n", [Count]).

test(pbfs_rect_ast_nodes) :-
    generate_rec_grid(5, 5, 1),
    reset_explored_count,
    pbfs_show(ast, (0,0), (4,4), _),
    get_explored_count(Count),
    format("~n[RECT A*] Nodes explored: ~w~n", [Count]).

test(pbfs_tri_ast_nodes) :-
    generate_tri_grid(4, 4, 1.5),
    reset_explored_count,
    pbfs_show(ast, (0,0), (4.5, 5.196), _),
    get_explored_count(Count),
    format("~n[TRI A*] Nodes explored: ~w~n", [Count]).

test(pbfs_hex_ast_nodes) :-
    generate_hex_grid(3, 3, 1.5),
    reset_explored_count,
    pbfs_show(ast, (3.0, 0.0), (6.0, 2.598076211353316), _),
    get_explored_count(Count),
    format("~n[HEX A*] Nodes explored: ~w~n", [Count]).

test(pbfs_pyt_ast_nodes) :-
    generate_pyt_grid(3, 3, 1),
    reset_explored_count,
    pbfs_show(ast, (0, 0), (6, 2), _),
    get_explored_count(Count),
    format("~n[PYT A*] Nodes explored: ~w~n", [Count]).

:- end_tests(informed_nodes).

% RUN THE TESTS ->  ['informed_node_count_test.plt']. -> run_tests(informed_nodes).



