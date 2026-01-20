
%This line shows the file content as plain text in the browser.


%================
%global variables
%>>>>>use any global variable
gvr0(Gvar,Y):- not(nb_current(Gvar,Y)) -> gvw(Gvar,''); true. %init to '' if necessary)
gvr(Gvar,Y):- gvr0(Gvar,Y), nb_getval(Gvar,Y). %global var read (init to '' if necessary)
gvw(Gvar,Y):- nb_setval(Gvar,Y). %global var write; to reset set to '' with gvw(Gvar,'') 
gva(Gvar,Y):- gvr(Gvar,G), atomic_list_concat([G,Y],G1), gvw(Gvar,G1). %global var append 
%gvr(buf1,X).
%gva(buf1,5).
%>>>>>use buf as a default global variable
gvi :- gvw(buf,''). %global var initialize
gvr(X) :- gvr(buf,X). %global var read
gvw(X) :- gvw(buf,X). %glabal var write
gva(X) :- gva(buf,X). %global var append
gvf(G) :- gvr(G,X), wf(X). %global var to file
gvf :- gvr(X), wf(X). %default global var (buf) to file
%=================
%write SVG to a HTML file
%>>>>> writing a HTML file
wf(L,File) :- tell(File), h1(X), h2(Y), write(X), nl, write(L), nl, write(Y), told. %write an atom to a file
wf(L) :- wf(L,'test.html'). %use the default filename
%lnv(100,200,300,400,green,2,X), txv(100,200,text,blue,Y), atomic_list_concat([X,Y],Res), wf(Res),!.
%=============
%SVG templates
h1('<!DOCTYPE html>
<html>
<head><meta http-equiv="refresh" content="3" /></head>
<body><h2>Prolog Scalable Vector Graphics Library (PSVGL) by Kamen Kanev</h2>
<svg height="1000" width="1000">
').
h2('Sorry, your browser does not support inline SVG.
</svg>
</body>
</html>
').
/*--------------
PSVGL predicates
----------------*/
%======== 
%SVG line
%>>>>>line (segment) with color and width to a variable
%<line x1="1" y1="2" x2="3" y2="4" stroke="red" stroke-width="3" />
lnv(X1A,Y1A,X2A,Y2A,Color,WidthA,Res) :- maplist(round(),[X1A,Y1A,X2A,Y2A,WidthA],[X1,Y1,X2,Y2,Width]),
	atomic_list_concat(['<line x1="',X1,'" y1="',Y1,'" x2="',X2,'" y2="',Y2,'" stroke="',Color,'" stroke-width="',Width,'"/>', '\n'],Res).
lnv(X1,Y1,X2,Y2,Res) :- lnv(X1,Y1,X2,Y2,black,1,Res). %default color (black) and width (1) <*******
%lnv(100,200,300,400,red,2,Res).
%lnv(100,200,300,400,Res)
%>>>>>line (segment) with color and width shown with write
ln(X1,Y1,X2,Y2,Color,Width) :- lnv(X1,Y1,X2,Y2,Color,Width,Res), write(Res). 
ln(X1,Y1,X2,Y2) :- lnv(X1,Y1,X2,Y2,Res), write(Res).
%ln(100,200,300,400,red,2).
%ln(100,200,300,400).
%>>>>>line (segment) with color and width to a global variable (buffer)
lng(X1,Y1,X2,Y2,Color,Width,Gvar) :- lnv(X1,Y1,X2,Y2,Color,Width,Res), gva(Gvar,Res). 
lng(X1,Y1,X2,Y2,Gvar) :- lnv(X1,Y1,X2,Y2,Res), gva(Gvar,Res). %default color and width
lng(X1,Y1,X2,Y2,Color,Width) :- lnv(X1,Y1,X2,Y2,Color,Width,Res), gva(Res). 
lng(X1,Y1,X2,Y2) :- lnv(X1,Y1,X2,Y2,Res), gva(Res). %default color and width
%gvw(buf1,''),lng(100,200,300,400,red,2,buf1),gvr(buf1,X),gvf(buf1).
%gvw(buf1,''),lng(100,200,300,400,buf1),gvr(buf1,X),gvf(buf1).
%gvi,lng(200,100,300,400,red,2),gvr(X),gvf.
%gvi,lng(100,200,300,400),gvr(X),gvf.
%========
%SVG text
%>>>>> with color, fill, and size to a variable
%<text x="1" y="2" stroke="red" fill="blue" font-size="35">text</text>
txv(XA,YA,T,Color,Fill,Size,Res) :-  maplist(round(),[XA,YA],[X,Y]),
	atomic_list_concat(['<text x="', X, '" y="', Y,'" stroke="',Color, '" fill="', Fill, '" font-size="', Size, '">', T, '</text>', '\n'], Res).
txv(X,Y,T,Res) :- txv(X,Y,T,none,black,20,Res). %default color, fill, and size (20) <*******
%txv(100,200,text,red,none,30,Res).
%txv(100,200,text,Res).
%>>>>> with color, fill, and size shown with write
tx(X,Y,T,Color,Fill,Size) :- txv(X,Y,T,Color,Fill,Size,Res), write(Res).
tx(X,Y,T) :- txv(X,Y,T,Res), write(Res). %default color, fill, and size
%tx(100,200,text,red,green,12).
%tx(100,200,text).
%>>>>> with color, fill, and size to a global variable (buffer)
txg(X,Y,T,Color,Fill,Size,Gvar) :- txv(X,Y,T,Color,Fill,Size,Res), gva(Gvar,Res).
txg(X,Y,T,Gvar) :- txv(X,Y,T,Res), gva(Gvar,Res). %default color, fill, and size
txg(X,Y,T,Color,Fill,Size) :- txv(X,Y,T,Color,Fill,Size,Res), gva(Res).
txg(X,Y,T) :- txv(X,Y,T,Res), gva(Res). %default color, fill, and size
%gvw(buf1,''),txg(100,200,'Text',none,blue,30,buf1),gvr(buf1,X),gvf(buf1).
%gvw(buf1,''),txg(100,200,'Text',buf1),gvr(buf1,X),gvf(buf1).
%gvi,txg(100,200,text,red,black,100),gvr(X),gvf.
%gvi,txg(100,200,text),gvr(X),gvf.
/*PSVGL END=========================================================================*/
  % Load the SVG drawing library

% Utility Predicates

clear_edges :- retractall(e(_, _, _, _, _)).
clear_vertices :- retractall(vertex(_, _)), retractall(vertex_id(_, _, _)).


add_edge(X1, Y1, X2, Y2, Cost) :-
    assertz(e(X1, Y1, X2, Y2, Cost)).

scale(Coord, Scaled) :- 
    ScaleFactor = 60,
    Offset = 100,
    Scaled is Coord * ScaleFactor + Offset.

cost_color(Cost, 'black') :- Cost =< 2.
cost_color(Cost, 'red') :- Cost > 2.

cost_thickness(Cost, 1) :- Cost =< 2.
cost_thickness(Cost, 3) :- Cost > 2.


assign_vertex_ids :-
    findall((X,Y), vertex(X,Y), Vertices),
    assign_ids(Vertices, 1).

assign_ids([], _).
assign_ids([(X,Y)|Rest], ID) :-
    ( vertex_id(X,Y) -> true ; assertz(vertex_id(X,Y,ID)) ),
    NextID is ID + 1,
    assign_ids(Rest, NextID).

% Drawing functions

draw_all_edges :-
    gvi,
    draw_all_edges_1,
    draw_all_labels,
    gvr(SVG),
    wf(SVG, 'output.html').

draw_all_edges_1 :-
    forall(e(X1, Y1, X2, Y2, Cost), (
        scale(X1, SX1), scale(Y1, SY1),
        scale(X2, SX2), scale(Y2, SY2),
        cost_color(Cost, Color),
        cost_thickness(Cost, Thickness),
        lng(SX1, SY1, SX2, SY2, Color, Thickness)
    )).

label_exists((X,Y), Labeled) :- memberchk((X,Y), Labeled).

collect_unique_points(Points) :-
    findall((X,Y), (e(X,Y,_,_,_); e(_,_,X,Y,_)), Raw),
    sort(Raw, Points).

draw_all_labels :-
    collect_unique_points(UniquePoints),
    draw_labels_non_overlapping(UniquePoints, []).

draw_labels_non_overlapping([], _).
draw_labels_non_overlapping([(X,Y)|Rest], Labeled) :-
    ( \+ label_exists((X,Y), Labeled) ->
        scale(X, SX), scale(Y, SY),
        RX is round(X), RY is round(Y),
        format(atom(Label), '(~d,~d)', [RX, RY]),
        txg(SX + 8, SY - 8, Label, 'blue', 'none', 14),
        draw_labels_non_overlapping(Rest, [(X,Y)|Labeled])
    ;
        draw_labels_non_overlapping(Rest, Labeled)
    ).


% Angle and Point Calculations


deg2rad(Deg, Rad) :- Rad is Deg * pi / 180.

angle_point(CX, CY, Radius, AngleDeg, X, Y) :-
    deg2rad(AngleDeg, Rad),
    X is CX + Radius * cos(Rad),
    Y is CY + Radius * sin(Rad).


% Grid Generators


generate_rec_grid(W, H, Cost) :-
    clear_edges,
    W1 is W - 1,
    H1 is H - 1,
    forall(between(0, W1, X),
      forall(between(0, H1, Y), (
          XRight is X + 1,
          YDown is Y + 1,
          (XRight =< W1 -> (
              add_edge(X, Y, XRight, Y, Cost),
              add_edge(XRight, Y, X, Y, Cost)
          ) ; true),
          (YDown =< H1 -> (
              add_edge(X, Y, X, YDown, Cost),
              add_edge(X, YDown, X, Y, Cost)
          ) ; true)
      ))).

if_then_else(Condition, Then, Else) :-
    ( Condition -> call(Then) ; call(Else) ).

round3(X, RX) :- RX is round(X * 1000) / 1000.

generate_tri_grid(W, H, Side) :-
    clear_edges,
    Ht is Side * sqrt(3) / 2,
    forall(between(0, H, Row),
      forall(between(0, W, Col), (
        X is Col * (Side / 2),
        Y is Row * Ht,
        (0 is (Row + Col) mod 2 ->
            X1Raw is X, Y1Raw is Y,
            X2Raw is X + Side, Y2Raw is Y,
            X3Raw is X + Side / 2, Y3Raw is Y + Ht
        ;
            X1Raw is X + Side / 2, Y1Raw is Y,
            X2Raw is X, Y2Raw is Y + Ht,
            X3Raw is X + Side, Y3Raw is Y + Ht
        ),
        round3(X1Raw, X1), round3(Y1Raw, Y1),
        round3(X2Raw, X2), round3(Y2Raw, Y2),
        round3(X3Raw, X3), round3(Y3Raw, Y3),

        add_edge(X1, Y1, X2, Y2, 1), add_edge(X2, Y2, X1, Y1, 1),
        add_edge(X2, Y2, X3, Y3, 1), add_edge(X3, Y3, X2, Y2, 1),
        add_edge(X3, Y3, X1, Y1, 1), add_edge(X1, Y1, X3, Y3, 1)

        % XRightRaw is X + Side,
        % round3(XRightRaw, XRight),
        % add_edge(X2, Y2, XRight, Y2, 1),
        % add_edge(XRight, Y2, X2, Y2, 1),
        % YDownRaw is Y + Ht,
        % round3(YDownRaw, YDown),
        % add_edge(X3, Y3, X3, YDown, 1),
        % add_edge(X3, YDown, X3, Y3, 1)
      ))).

generate_hex_grid(W, H, Side) :-
    retractall(e(_, _, _, _, _)),
    retractall(hex_vertices(_, _, _)),
    DX is 1.5 * Side,
    DY is Side * sqrt(3),
    W1 is W - 1,
    H1 is H - 1,
    forall(between(0, W1, Col),
      forall(between(0, H1, Row), (
        X0 is Col * DX,
        OffsetY is (Col mod 2) * (DY / 2),
        Y0 is Row * DY + OffsetY,

        % Calculate 6 corner vertices of hexagon
        angle_point(X0, Y0, Side, 0,   X1, Y1),
        angle_point(X0, Y0, Side, 60,  X2, Y2),
        angle_point(X0, Y0, Side, 120, X3, Y3),
        angle_point(X0, Y0, Side, 180, X4, Y4),
        angle_point(X0, Y0, Side, 240, X5, Y5),
        angle_point(X0, Y0, Side, 300, X6, Y6),

        Vertices = [(X1,Y1), (X2,Y2), (X3,Y3), (X4,Y4), (X5,Y5), (X6,Y6)],
        assertz(hex_vertices(Col, Row, Vertices)),

        assert_unique_edge(X1, Y1, X2, Y2),
        assert_unique_edge(X2, Y2, X3, Y3),
        assert_unique_edge(X3, Y3, X4, Y4),
        assert_unique_edge(X4, Y4, X5, Y5),
        assert_unique_edge(X5, Y5, X6, Y6),
        assert_unique_edge(X6, Y6, X1, Y1)
      ))
    ).

assert_unique_edge(X1, Y1, X2, Y2) :-
    (e(X1, Y1, X2, Y2, _) ; e(X2, Y2, X1, Y1, _)) -> true ;
    add_edge(X1, Y1, X2, Y2, 1).





% Generate Pythagorean Grid

generate_pyt_grid(Rows, TilesPerRow, Cost) :-
    clear_edges,
    TileSize = 2,
    generate_rows(0, Rows, TilesPerRow, 0, 0, TileSize, Cost).

generate_rows(RowNum, TotalRows, _, _, _, _, _) :-
    RowNum >= TotalRows, !.

generate_rows(RowNum, TotalRows, TilesPerRow, X, Y, Size, Cost) :-
    (RowNum mod 2 =:= 0 ->
        RowStartX = X,
        RowStartY = Y
    ;
        RowStartX is X + Size // 2,
        RowStartY is Y + Size // 2
    ),
    generate_stairs(0, TilesPerRow, RowNum, TotalRows, TilesPerRow, RowStartX, RowStartY, Size, Cost),
    NewY is RowStartY + Size,
    RowNum1 is RowNum + 1,
    generate_rows(RowNum1, TotalRows, TilesPerRow, X, NewY, Size, Cost).

generate_stairs(N, Count, _, _, _, _, _, _, _) :-
    N >= Count, !.

generate_stairs(N, Count, RowNum, TotalRows, TilesPerRow, X, Y, Size, Cost) :-
    X1 is X + Size, Y1 is Y,
    X2 is X1,       Y2 is Y + Size,
    X3 is X,        Y3 is Y + Size,

    add_edge(X, Y, X1, Y1, Cost),
    add_edge(X1, Y1, X2, Y2, Cost),
    add_edge(X2, Y2, X3, Y3, Cost),
    add_edge(X3, Y3, X, Y, Cost),

    (N + 1 < Count ->
        NextTopLeftX is X + Size,
        NextTopLeftY is Y + Size // 2,
        add_edge(X1, Y1, NextTopLeftX, NextTopLeftY, Cost)
    ; true),

    (RowNum + 1 < TotalRows ->
        (RowMod is RowNum mod 2,
         (RowMod =:= 0 -> OffsetX = 0 ; OffsetX is Size // 2),
         BelowTopRightX is X + OffsetX + Size,
         BelowTopRightY is Y + Size,
         add_edge(X3, Y3, BelowTopRightX, BelowTopRightY, Cost))
    ; true),

    NewX is X + Size,
    NewY is Y + Size // 2,
    N1 is N + 1,
    generate_stairs(N1, Count, RowNum, TotalRows, TilesPerRow, NewX, NewY, Size, Cost).



% Search Algorithms
% NODE EXPLORATION COUNTER 

:- dynamic explored_count/1.
explored_count(0).

reset_explored_count :-
    retractall(explored_count(_)),
    asserta(explored_count(0)).

increment_explored_count :-
    retract(explored_count(N)),
    N1 is N + 1,
    asserta(explored_count(N1)).

get_explored_count(N) :-
    explored_count(N).


neighbor((X1,Y1), (X2,Y2)) :-
    e(X1, Y1, X2, Y2, _);
    e(X2, Y2, X1, Y1, _).

dfs(Start, Goal, Path) :-
    dfs_recursive(Start, Goal, [Start], Path).

dfs_recursive(Goal, Goal, Visited, Path) :-
    reverse(Visited, Path).
dfs_recursive(Current, Goal, Visited, Path) :-
    neighbor(Current, Next),
    \+ member(Next, Visited),
    dfs_recursive(Next, Goal, [Next|Visited], Path).

dls(Start, Goal, Limit, Path) :-
    dls_recursive(Start, Goal, [Start], Limit, Path).

dls_recursive(Goal, Goal, Visited, _, Path) :-
    reverse(Visited, Path).
dls_recursive(Current, Goal, Visited, Limit, Path) :-
    Limit > 0,
    neighbor(Current, Next),
    \+ member(Next, Visited),
    NewLimit is Limit - 1,
    dls_recursive(Next, Goal, [Next|Visited], NewLimit, Path).

ids(Start, Goal, Path) :-
    between(1, 100, Limit),
    dls(Start, Goal, Limit, Path),
    !.

%  Draw Path in Green

draw_path_edges([]).
draw_path_edges([_]).
draw_path_edges([P1, P2 | Rest]) :-
    P1 = (X1, Y1),
    P2 = (X2, Y2),
    scale(X1, SX1), scale(Y1, SY1),
    scale(X2, SX2), scale(Y2, SY2),
    lng(SX1, SY1, SX2, SY2, 'green', 4),
    draw_path_edges([P2 | Rest]).

% Search

dfs_show(Start, Goal, Path) :-
    dfs(Start, Goal, Path),
    gvi,
    draw_all_edges,
    draw_path_edges(Path),
    draw_all_labels,
    gvr(SVG),
    wf(SVG, 'output.html').

dls_show(Start, Goal, Limit, Path) :-
    dls(Start, Goal, Limit, Path),
    gvi,
    draw_all_edges,
    draw_path_edges(Path),
    draw_all_labels,
    gvr(SVG),
    wf(SVG, 'output.html').

ids_show(Start, Goal, Path) :-
    ids(Start, Goal, Path),
    gvi,
    draw_all_edges,
    draw_path_edges(Path),
    draw_all_labels,
    gvr(SVG),
    wf(SVG, 'output.html').







%  Parameterized BFS Variants

% f(X,Y,G,H,F) where:
% G = Cost from Start to Node (g(n))
% H = Heuristic to Goal (h(n))
% F = Total cost (for ordering, varies by algo)

heuristic((X1,Y1), (X2,Y2), H) :-
    DX is X2 - X1, DY is Y2 - Y1,
    H is sqrt(DX*DX + DY*DY).

f(bfs, _G, _H, F) :- F is 0.
f(ucs, G, _H, F) :- F is G.
f(gbf, _G, H, F) :- F is H.
f(ast, G, H, F) :- F is G + H.

pbfs(Algo, Start, Goal, Path) :-
    f(Algo, 0, 0, F0),
    heuristic(Start, Goal, H0),
    bfs_queue(Algo, [(F0, 0, H0, [Start])], Goal, RevPath),
    reverse(RevPath, Path).

bfs_queue(_, [], _, _) :- fail.

bfs_queue(Algo, [(F,G,H,[Goal|Rest])|_], Goal, [Goal|Rest]) :-
    increment_explored_count.

bfs_queue(Algo, [(F,G,H,[Current|RestPath])|Queue], Goal, Path) :-

    % Count expanded node
    increment_explored_count,

    % Generate children
    findall((FNew, GNew, HNew, [Next,Current|RestPath]),
        (
            neighbor(Current, Next),
            \+ member(Next, [Current|RestPath]),
            e(CurrentX, CurrentY, NextX, NextY, Cost1),
            ( (Current = (CurrentX, CurrentY), Next = (NextX, NextY)) ;
              (Current = (NextX, NextY), Next = (CurrentX, CurrentY)) ),
            GNew is G + Cost1,
            heuristic(Next, Goal, HNew),
            f(Algo, GNew, HNew, FNew)
        ),
    Children),

    append(Queue, Children, NewQueue),
    sort(NewQueue, SortedQueue),
    bfs_queue(Algo, SortedQueue, Goal, Path).







pbfs_show(Algo, Start, Goal, Path) :-
    pbfs(Algo, Start, Goal, Path),
    gvi,
    draw_all_edges,
    draw_path_edges(Path),
    draw_all_labels,
    gvr(SVG),
    wf(SVG, 'output.html').




    /* SAMPLE QUERIES (COPY PASTE TO PROLOG CONSOLE)
    
RECTANGLE

generate_rec_grid(5, 5, 1), pbfs_show(bfs, (0,0), (4,4), Path).
generate_rec_grid(5, 5, 3), pbfs_show(ucs, (0,0), (4,4), Path).
generate_rec_grid(5, 5, 1), pbfs_show(gbf, (0,0), (4,4), Path).
generate_rec_grid(5, 5, 1), pbfs_show(ast, (0,0), (4,4), Path).

TRIANGLE

generate_tri_grid(4, 4, 1.5), pbfs_show(bfs, (0, 0), (4.5, 5.196), Path).   
generate_tri_grid(4, 4, 1.5), pbfs_show(ucs, (0, 0), (4.5, 5.196), Path).    
generate_tri_grid(4, 4, 1.5), pbfs_show(gbf, (0, 0), (4.5, 5.196), Path).    
generate_tri_grid(4, 4, 1.5), pbfs_show(ast, (0, 0), (4.5, 5.196), Path).    

HEXAGONAL

generate_hex_grid(3, 3, 1.5), pbfs_show(bfs, (3.0, 0.0), (6.0, 2.598076211353316), Path).   
generate_hex_grid(3, 3, 1.5), pbfs_show(ucs, (3.0, 0.0), (6.0, 2.598076211353316), Path).   
generate_hex_grid(3, 3, 1.5), pbfs_show(gbf, (3.0, 0.0), (6.0, 2.598076211353316), Path).   
generate_hex_grid(3, 3, 1.5), pbfs_show(ast, (3.0, 0.0), (6.0, 2.598076211353316), Path).  

PYTHAGOREAN


generate_pyt_grid(3, 3, 1), pbfs_show(bfs, (0, 0), (6, 4), Path).
generate_pyt_grid(3, 3, 1), pbfs_show(ucs, (0, 0), (6, 4), Path).
generate_pyt_grid(3, 3, 1), pbfs_show(gbf, (0, 0), (6, 2), Path).
generate_pyt_grid(3, 3, 1), pbfs_show(ast, (0, 0), (6, 2), Path).


NODE COUNT

reset_explored_count,
generate_rec_grid(5, 5, 1),
pbfs_show(bfs, (0,0), (4,4), Path),
get_explored_count(N).

reset_explored_count,
generate_rec_grid(5, 5, 3),
pbfs_show(ucs, (0,0), (4,4), Path),
get_explored_count(N).

TRIANGULAR

reset_explored_count,
generate_tri_grid(4, 4, 1.5),
pbfs_show(bfs, (0, 0), (4.5, 5.196), Path),
get_explored_count(N).

reset_explored_count,
generate_tri_grid(4, 4, 1.5),
pbfs_show(ucs, (0, 0), (4.5, 5.196), Path),
get_explored_count(N).

reset_explored_count,
generate_tri_grid(4, 4, 1.5),
pbfs_show(gbf, (0, 0), (4.5, 5.196), Path),
get_explored_count(N).

PYTAGOREAN


reset_explored_count,
generate_pyt_grid(3, 3, 1),
pbfs_show(ucs, (0, 0), (6, 4), Path),
get_explored_count(N).

reset_explored_count,
generate_pyt_grid(3, 3, 1),
pbfs_show(gbf, (0, 0), (6, 2), Path),
get_explored_count(N).



    
    */




