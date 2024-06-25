-module(state_manager).

-export([
         start_manager/0, 
         update/2, 
         update/3, 
         get/1
        ]).

start_manager() ->
    Pid = spawn(fun() -> loop(#{
                                producers => #{},
                                consumers => #{},
                                buffer => []
                               }) end),
    register(state_manager, Pid),
    {ok, Pid}.

update(buffer, Buffer) -> 
    state_manager ! {update, buffer, Buffer}.
update(consumer, Index, Status) -> 
    state_manager ! {update, consumer, Index, Status};
update(producer, Index, Status) ->
    state_manager ! {update, producer, Index, Status}.

get(Pid) ->
    Pid ! {get, self()},
    receive
        {state, State} -> State
    end.

loop(State) ->
    receive
        {update, buffer, Buffer} ->
            NState = State#{ buffer := Buffer },
            loop(NState);
        
        {update, consumer, I, S } ->
            #{ consumers := Map }= State,
            Nmap = Map#{ I => S },
            NState = State#{ consumers := Nmap },
            loop(NState);

        {update, producer, I, S } -> 
            #{ producers := Map }= State,
            Nmap = Map#{ I => S },
            NState = State#{ producers := Nmap },
            loop(NState);

        {get, PidG} ->
            PidG ! {state, State},
            loop(State)
    end.

