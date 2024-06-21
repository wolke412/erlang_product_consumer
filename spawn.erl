-module(spawn).
-export([
         start/0, 
         producer/2, 
         consumer/2, 
         buffer_manager/0, 
         get_current_timestamp/0,
         get_delay/1,
         delay/1
        ]).

-define(PRODUCER_CURTO, 0).
-define(PRODUCER_LONGO, 1).
-define(TIME_PROD_CURTO, 3_500).
-define(TIME_PROD_LONGO, 7_500).

get_current_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

producer(PidBuf, Index) -> producer(PidBuf, Index, rand:uniform(2) - 1). % 0 ou 1
producer(PidBuf, Index, Type) ->
    
    state_manager:update(producer, Index, { Type, get_current_timestamp()}),

    delay(Type),

    PidBuf ! {produced, Type, Index},

    producer(PidBuf, Index). 


get_delay( X ) -> 
    case X of 
        ?PRODUCER_CURTO -> ?TIME_PROD_CURTO;
        ?PRODUCER_LONGO -> ?TIME_PROD_LONGO
    end.

delay(X) -> timer:sleep(get_delay(X)).


buffer_manager() -> buffer_manager([]).
buffer_manager(Buffer) ->

    state_manager:update(buffer, Buffer),

    receive
        {produced, Type, _Index} ->
            %io:format("Buffer Manager: Producer ~p added type ~p to buffer.~n", [Index, Type]),
            buffer_manager(Buffer ++ [Type]);

        {consume, ConsumerPid} ->

            case Buffer of
                [First | T] ->
                    ConsumerPid ! {message, First},
                    %io:format("Buffer Manager: Sent message ~p to consumer.~n", [First]),
                    buffer_manager(T);
                [] ->
                    %io:format("Buffer Manager: Queue is empty. Waiting for messages.~n"),
                    buffer_manager(Buffer)
            end;

        _ -> buffer_manager(Buffer) % Ignore other messages and continue
    end.


consumer(PidBuf, Index) ->
    

    state_manager:update(consumer, Index, 0),

    PidBuf ! { consume, self() },

    receive
        { message, Type } ->
            state_manager:update(consumer, Index, { Type, get_current_timestamp() }),
            % Já que precisa ser o dobro do tempo
            delay(Type),
            delay(Type),

            consumer(PidBuf, Index)

    after 100 -> consumer( PidBuf, Index)

    end.

start() ->

    % Interface stuff
    {ok, _MPid} = state_manager:start_manager(),    
    {ok, _RPid} = state_renderer:start_renderer(),

    PidBuf = spawn(spawn, buffer_manager, []),

    spawn(spawn, producer, [ PidBuf, 0 ]),
    spawn(spawn, producer, [ PidBuf, 1 ]),
      
    spawn(spawn, consumer, [ PidBuf, 0 ]),
    spawn(spawn, consumer, [ PidBuf, 1 ]),
    spawn(spawn, consumer, [ PidBuf, 2 ]),
    spawn(spawn, consumer, [ PidBuf, 3 ]).
