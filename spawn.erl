-module(spawn).
-export([
         start/0, 
         producer/2, 
         consumer/2, 
         buffer_manager/1, 
         get_current_timestamp/0,
         get_delay/1,
         delay/1
        ]).


% Constantes 
-define(BUFFER_SIZE, 1).


-define(PRODUCER_CURTO, 0).
-define(PRODUCER_LONGO, 1).
-define(TIME_PROD_CURTO, 1_500).
-define(TIME_PROD_LONGO, 2_500).

% Retorna timestamp do sistema em Millis
get_current_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

% Função pública que retorna o tempo de delay em millissegundos
% necessário para um produto de tipo X ser PRODUZIDO
get_delay( X ) -> 
    case X of 
        ?PRODUCER_CURTO -> ?TIME_PROD_CURTO;
        ?PRODUCER_LONGO -> ?TIME_PROD_LONGO
    end.

% Função que de fato aguarda o tempo de produção
% de um produto.
delay(X) -> timer:sleep(get_delay(X)).


% Função que virará o processo administrador
% do buffer de acesso compartilhado.
buffer_manager(Max) -> buffer_manager([], Max).
buffer_manager(Buffer, Max) ->

    % [!]-----------------------------------------
    % | Chama uma função do módulo responsável
    % | por gerir o estado da "interface gráfica".
    % +-------------------------------------------
    % | Não afeta o funcionamento do sistema!
    % |
    state_manager:update(buffer, Buffer),
    
    %%%
    %
    % Este bloco ouve mensagens vindas dos produtores e consumidores;
    % 
    % É valido notar que ele é responsável por criar o funcionameto
    % de looping de escuta desta função;
    % Uma vez que todas as suas alternativas geram em uma nova chamada
    % para ela.
    %
    receive

        % Ao receber um novo produto, insere-o no Buffer
        {produced, Type, ProdPid} ->
      
            case length(Buffer) of
                Max -> 
                    ProdPid ! { wait },
                    buffer_manager(Buffer, Max);

                _ -> 
                    ProdPid ! { inserted },
                    buffer_manager(Buffer ++ [Type], Max)
            end;
    
        % Ao receber um pedido de consumo:
        {consume, ConsumerPid} ->
            
            case Buffer of

                %%%
                %
                % Se houver um elemento First e um resto ou "vazio":
                %   | Envia o elemento para o PID requisitante.
                %   | e chama a função somente com o restante do buffer.
                %    
                % Senão
                %   Volta ao loop de espera
                %
                [First | T] ->
                     
                    ConsumerPid ! {message, First},
                    buffer_manager(T, Max);

                [] -> 
                    ConsumerPid ! { empty },
                    buffer_manager(Buffer, Max)
            end;

        _ -> buffer_manager(Buffer) % Ignora outras mensagens e vida que segue
    end.


%%%
%
% Função produtora;
% Se não houver um tipo definido, um aleatório será gerado.
%
producer(PidBuf, Index) -> producer(PidBuf, Index, rand:uniform(2) - 1). % 0 ou 1
producer(PidBuf, Index, Type) ->
    
    % [!]-----------------------------------------
    % | Chama uma função do módulo responsável
    % | por gerir o estado da "interface gráfica".
    % +-------------------------------------------
    % | Não afeta o funcionamento do sistema!
    % |
    state_manager:update(producer, Index, { Type, get_current_timestamp()}),

    delay(Type),

    % Entra no loop de requisição até
    % que o buffer aceite a a inserção.
    request_insert( PidBuf, Type, Index ),
        

    % Executa a cláusula que gera um tipo randômico
    producer(PidBuf, Index). 

request_insert( PidBuf, Type, Index ) ->
    PidBuf ! {produced, Type, self() },

    receive
        { inserted } -> { ok };
        { wait } -> 
            timer:sleep(100),
            request_insert( PidBuf, Type, Index )
    end.

%%%
%
% Função que virá a se tornar um processo
% "Consumidor". 
% Irá consumir, de forma cíclica, os produtos
% do buffer compartilhado.
% 
% PidBuf => PID do gerenciador de buffer
% Index  => Índice deste consumidor.
%
consumer(PidBuf, Index) ->
    
    % [!]-----------------------------------------
    % | Chama uma função do módulo responsável
    % | por gerir o estado da "interface gráfica".
    % +-------------------------------------------
    % | Não afeta o funcionamento do sistema!
    % |
    state_manager:update(consumer, Index, 0),

    %%%
    %
    % Dispara uma mensagem solicitando um produto
    % ao gestor do buffer.
    %
    PidBuf ! { consume, self() },

    receive
        { message, Type } ->

            % [!]-----------------------------------------
            % | Chama uma função do módulo responsável
            % | por gerir o estado da "interface gráfica".
            % +-------------------------------------------
            % | Não afeta o funcionamento do sistema!
            % |
            state_manager:update(consumer, Index, { Type, get_current_timestamp() }),

            % Já que precisa ser o dobro do tempo
            delay(Type),
            delay(Type),
   
            % Após consumir o produto reinicia o ciclo.
            consumer(PidBuf, Index);
        
        % Se não houver elementos no buffer
        % um retorno é informado.
        { empty } -> 
            timer:sleep(100),
            consumer(PidBuf, Index)
    
    end.


%%%
% 
% Função que inicia a aplicação de fato.
%
start() ->

    % [!]-----------------------------------------
    % | Inicia os módulos responsáveis pela 
    % | "interface gráfica".
    % +-------------------------------------------
    % | Os módulos *NÃO* afetam o funcionamento 
    % | do sistema!
    % |
            
    {ok, _MPid} = state_manager:start_manager(),    
    {ok, _RPid} = state_renderer:start_renderer(),


    %%%
    %
    % Inicia o precesso de gestão do Buffer
    %
    PidBuf = spawn(spawn, buffer_manager, [ ?BUFFER_SIZE ]), 


    %%%
    %
    % Inicia os processos produtores e consumidores
    %
    [spawn(spawn, producer, [ PidBuf, X ]) || X <- lists:seq(0, 1)],
    [spawn(spawn, consumer, [ PidBuf, X ]) || X <- lists:seq(0, 3)].
    


