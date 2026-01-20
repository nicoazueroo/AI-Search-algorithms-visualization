:- begin_tests(informed_time, [cleanup(all_traces)]).

:- use_module(informed).
:- use_module(utils).

test(pbfs_rect_bfs_time) :-
    generate_rec_grid(5, 5, 1),
    time(pbfs_show(bfs, (0,0), (4,4), _)).

test(pbfs_rect_ucs_time) :-
    generate_rec_grid(5, 5, 3),
    time(pbfs_show(ucs, (0,0), (4,4), _)).

test(pbfs_rect_gbf_time) :-
    generate_rec_grid(5, 5, 1),
    time(pbfs_show(gbf, (0,0), (4,4), _)).

test(pbfs_rect_ast_time) :-
    generate_rec_grid(5, 5, 1),
    time(pbfs_show(ast, (0,0), (4,4), _)).

test(pbfs_tri_bfs_time) :-
    generate_tri_grid(4, 4, 1.5),
    time(pbfs_show(bfs, (0,0), (4.5, 5.196), _)).

test(pbfs_tri_ucs_time) :-
    generate_tri_grid(4, 4, 1.5),
    time(pbfs_show(ucs, (0,0), (4.5, 5.196), _)).

test(pbfs_tri_gbf_time) :-
    generate_tri_grid(4, 4, 1.5),
    time(pbfs_show(gbf, (0,0), (4.5, 5.196), _)).

test(pbfs_tri_ast_time) :-
    generate_tri_grid(4, 4, 1.5),
    time(pbfs_show(ast, (0,0), (4.5, 5.196), _)).

test(pbfs_hex_bfs_time) :-
    generate_hex_grid(3, 3, 1.5),
    time(pbfs_show(bfs, (3.0, 0.0), (6.0, 2.598076211353316), _)).

test(pbfs_hex_ucs_time) :-
    generate_hex_grid(3, 3, 1.5),
    time(pbfs_show(ucs, (3.0, 0.0), (6.0, 2.598076211353316), _)).

test(pbfs_hex_gbf_time) :-
    generate_hex_grid(3, 3, 1.5),
    time(pbfs_show(gbf, (3.0, 0.0), (6.0, 2.598076211353316), _)).

test(pbfs_hex_ast_time) :-
    generate_hex_grid(3, 3, 1.5),
    time(pbfs_show(ast, (3.0, 0.0), (6.0, 2.598076211353316), _)).

test(pbfs_pyt_bfs_time) :-
    generate_pyt_grid(3, 3, 1),
    time(pbfs_show(bfs, (0, 0), (6, 4), _)).

test(pbfs_pyt_ucs_time) :-
    generate_pyt_grid(3, 3, 1),
    time(pbfs_show(ucs, (0, 0), (6, 4), _)).

test(pbfs_pyt_gbf_time) :-
    generate_pyt_grid(3, 3, 1),
    time(pbfs_show(gbf, (0, 0), (6, 2), _)).

test(pbfs_pyt_ast_time) :-
    generate_pyt_grid(3, 3, 1),
    time(pbfs_show(ast, (0, 0), (6, 2), _)).

:- end_tests(informed_time).


% RUN TESTS -> ['informed_time_test.plt']. -> run_tests(informed_time).