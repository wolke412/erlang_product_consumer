-module(state_renderer).
-export([start_renderer/0, draw/1]).

-define(RENDER_INTERVAL, 200).

start_renderer() ->
    Pid = spawn(fun() -> render_cycle() end),
    register(state_renderer, Pid),
    {ok, Pid}.

render_cycle() ->
    State = state_manager:get(whereis(state_manager)),
        
    draw(State),

    timer:sleep(?RENDER_INTERVAL),
    render_cycle().

cls() -> 
    %io:format("~c[2J~c[H", [27, 27]).
    io:format("\ec").

dim() -> 
    {ok, Rows} = io:rows(),
    {ok, Cols} = io:columns(),

    { Rows, Cols }.

draw( State ) ->

    cls(),

    { _, Wt } = dim(),
    
    CCellW = trunc( (Wt - 2 - 3  ) / 4 ),
    PCellW = CCellW bsl 1 + 1, % Considerando que é o dobro sempre
    ActualW = PCellW bsl 1 + 2 + 1, % +1 Cells have [Spacing] + Separator

    fline(ActualW),
    cells(ActualW - 2, ["Produtor - Consumidor"] ),
    fline(ActualW),
    
    #{ 
      consumers := C, 
      producers := P, 
      buffer := B 
     } = State,
    
    render_producers(P, CCellW),
    fline(ActualW),

    render_buffer( B, ActualW ),

    fline( ActualW ),
    
    render_consumers(C, CCellW),
    fline( ActualW ).



render_producers(P, CellW) ->
    L = maps:to_list(P),

    cells(CellW, ["", "", "", ""]),
    cells(CellW, ["", "Produtor 1", "Produtor 2", ""]),

    cells(CellW, [ "" ] ++ [ g_producer_product(V) || {_, V} <- L ] ++ [""]),
    cells(CellW, 
          [ "" ] ++ 
          [ progress_bar(g_get_progress(1, V), CellW) 
           || {_, V} <- L 
          ] ++ 
          [""]),

    cells(CellW, ["", "", "", ""]).

render_consumers( C, CellW ) ->
    L = maps:to_list(C),

    cells(CellW, ["","","",""]),
    cells(CellW, [
        "Consumidor 1",
        "Consumidor 2",
        "Consumidor 3",
        "Consumidor 4"
    ]),
    cells(CellW, [ g_producer_product(V) || {_, V} <- L ] ),
    cells(CellW, [ progress_bar(g_get_progress(2, V), CellW) || {_, V} <- L ]),
    cells(CellW, ["","","",""]).

g_producer_product(0) ->
    "WAITING";
g_producer_product({ Type, _Began }) ->
    buf_type_name(Type).


g_get_progress(_, 0 ) -> 0;
g_get_progress( Multiplier, {Type, Began} ) ->
    Millis = spawn:get_current_timestamp(),

    Max = spawn:get_delay(Type) * ( Multiplier ) ,

    clamp((Millis - Began) / Max, 0.0, 1.0).

clamp(V, Min, _Max) when V < Min -> Min;
clamp(V, _Min, Max) when V > Max -> Max;
clamp(V, _Min, _Max) -> V.

render_buffer(B, ActualW) ->
    
    cells(ActualW - 2, [  
                    io_lib:format(" Fila[~p]: ", [length(B)])
                   ]),
    BufString = io_lib:format("~p", [ 
                                     [ buf_type_name(X) || X <- B ] 
                                    ] ),
    cells( ActualW - 2, BufString ).

buf_type_name(X) -> case X of 
                        0 -> "Curto"; 
                        1 -> "Longo" 
                    end .

cell ( CellW, Item ) -> center_in(Item, CellW).

cells( _CellW, [] ) -> 
    column(),
    feed();

cells( CellW, Items ) ->
    column(),
    [H|T] = Items,
    cell(CellW, H),
    cells(CellW, T).


line(W) -> 
    intersection(),
    segment(W - 2),
    intersection().
fline(W) -> 
    line(W),
    feed().
 
feed() -> io:format("~n").

segment(0) -> { ok };
segment(N) -> 
    io:format("-"), 
    segment(N - 1).

intersection() -> io:format("+").

column()->io:format("|").

center_in(Text, Width) when is_list(Text), length(Text) > Width ->
    whitespaces(Width);

center_in( Text, Width )  ->
    
    L = string:length(Text),
    Diff = Width - L,
    Beg = Diff bsr 1,
    End = Diff - Beg,

    whitespaces(Beg),
    io:format(Text),
    whitespaces(End).

whitespaces(0) -> { ok } ;
whitespaces(N) when N < 0 -> { ok } ;
whitespaces(N) when N > 0 -> 
    io:format(" "),
    whitespaces(N - 1).


p_begin() -> " [".
p_end() -> "] ".
p_occupied_char() -> "|".
p_empty_char() -> ".".

progress_bar( Percentage, AvWidth ) ->
    ActualBarAvWidth = AvWidth - 4,
    Bars = trunc(Percentage * ActualBarAvWidth),
    Empty = ActualBarAvWidth - Bars,

    O = p_occupied_char(),
    E = p_empty_char(),

    M = lists:append(lists:duplicate(Bars, O), lists:duplicate(Empty, E)),
    io_lib:format("~s~s~s", [p_begin(), M, p_end()]).
