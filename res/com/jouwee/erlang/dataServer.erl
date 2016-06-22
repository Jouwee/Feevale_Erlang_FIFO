%% -*- erlang -*-
%% Script Erlang para o servidor de dados (O que controla a fila)
-module(dataServer).
-export([main/1, criaProdutores/2, gerenciadorFila/3, produtor/3, criaListaVazia/1, loopProducao/3, consumidor/3, loopConsumidor/3]).
%% -----------------------------------------------------------------------------
%% Função principal do Script
main([TamanhoFila, StringNumeroProdutores, NumeroConsumidores, TempoMedioProducao, DesvioPadraoProducao, TempoMedioConsumo, DesvioPadraoConsumo]) ->
    {NumeroProdutores, Rest} = string:to_integer(StringNumeroProdutores), 
    GerenciadorFila = spawn(?MODULE, gerenciadorFila, [self(), "GerenciadorFila", 10]),
    criaProdutores(GerenciadorFila, NumeroProdutores),
    spawn(?MODULE, consumidor, [self(), "Consumidor1", GerenciadorFila]),
    spawn(?MODULE, consumidor, [self(), "Consumidor2", GerenciadorFila]),
    messageReceiveLoop().
%% -----------------------------------------------------------------------------
%% Cria os produtores
criaProdutores(GerenciadorFila, 0) ->
    done;
criaProdutores(GerenciadorFila, NumeroProdutores) ->
    NomeProdutor = string:concat("Produtor",stringFormat(NumeroProdutores)),
    spawn(?MODULE, produtor, [self(), NomeProdutor, GerenciadorFila]),
    criaProdutores(GerenciadorFila, NumeroProdutores-1).
%% -----------------------------------------------------------------------------
%% Loop de recebimento de mensagens para printar no output
messageReceiveLoop() ->
    receive
        {log, Name, Pid, Message} -> 
            io:format("#Logger ~s ~p ~p\n", [Name, Pid, Message]),
            messageReceiveLoop()
    end.
%% -----------------------------------------------------------------------------
%%                            GERENCIADOR DA FILA
%% -----------------------------------------------------------------------------
%% Método principal do processo de gerenciamento de filas
gerenciadorFila(Logger, Name, TamanhoFila) ->
    Logger ! {log, Name, self(), "Iniciei o gerenciador da fila"},
    Fila = criaListaVazia(TamanhoFila),
    gerenciadorFilaLoop(Logger, Name, TamanhoFila, Fila, 1).
%% -----------------------------------------------------------------------------
%% Loop de mensagens do gerenciador
gerenciadorFilaLoop(Logger, Name, TamanhoFila, Fila, NextId) ->
    Logger ! {log, Name, self(), {updateFila, Fila}},
    receive
        {reserveSpace, NameProdutor, PidProdutor} -> 
            gerenciadorFilaLoop(Logger, Name, TamanhoFila, reservaItem(Logger, NameProdutor, PidProdutor, Fila), NextId);
        {itemProduced, NameProdutor} -> 
            gerenciadorFilaLoop(Logger, Name, TamanhoFila, produzItem(Logger, NameProdutor, NextId, Fila), NextId + 1);
        {reserveConsumeItem, NameConsumidor, PidConsumidor} -> 
            gerenciadorFilaLoop(Logger, Name, TamanhoFila, reservaConsumoItem(Logger, NameConsumidor, PidConsumidor, Fila), NextId);
        {itemConsumed, NameConsumidor} -> 
            gerenciadorFilaLoop(Logger, Name, TamanhoFila, consomeItem(Logger, NameConsumidor, Fila), NextId)
    end,
    gerenciadorFilaLoop(Logger, Name, TamanhoFila, Fila, NextId).
%% -----------------------------------------------------------------------------
%% Reserva uma ocorrência um item
reservaItem(Logger, Name, Pid, Fila) ->
    Logger ! {log, Name, self(), "reservaItem"},
    IndexFirstZero = indexOfStatus({empty}, Fila),
    if
        IndexFirstZero == not_found -> 
            Retorno = Fila,
            Pid ! listIsFull,
            Logger ! {log, Name, self(), "listIsFull"};
        true -> 
            Retorno = lists:sublist(Fila, IndexFirstZero - 1) ++ [{reserved, 0, Name, ""}] ++ lists:nthtail(IndexFirstZero,Fila),
            Pid ! reserved
    end,
    Retorno.
%% -----------------------------------------------------------------------------
%% Produz um item
produzItem(Logger, Name, Id, Fila) ->
    Logger ! {log, Name, self(), "addToList"},
    Index = indexOfProdutor({reserved, Name}, Fila),
    lists:sublist(Fila,Index - 1) ++ [{produced, Id, Name, ""}] ++ lists:nthtail(Index,Fila).
%% -----------------------------------------------------------------------------
%% Reserva o consumo de um item
reservaConsumoItem(Logger, Name, Pid, Fila) ->
    Logger ! {log, Name, self(), "reservaItemParaConsumo"},
    Index = indexOfStatus({produced}, Fila),
    if
        Index == not_found -> 
            Pid ! listIsEmpty,
            Retorno = Fila;
        true ->
            {_, Id, Produtor, _} = lists:nth(Index, Fila),
            Retorno = lists:sublist(Fila,Index - 1) ++ [{reservedForConsumption, Id, Produtor, Name}] ++ lists:nthtail(Index,Fila),
            Pid ! reserved
    end,
    Retorno.
%% -----------------------------------------------------------------------------
%% Consome um item
consomeItem(Logger, Name, Fila) ->
    Logger ! {log, Name, self(), "consomeItem"},
    Index = indexOfConsumidor({reservedForConsumption, Name}, Fila),
    lists:sublist(Fila,Index - 1) ++ lists:nthtail(Index,Fila) ++ [{empty, 0, "", ""}].
%% -----------------------------------------------------------------------------
%% Cria uma lista vazia (Cheia de zeros)
criaListaVazia(0) ->
    [];
criaListaVazia(T) ->
    lists:append([{empty, 0, "", ""}], criaListaVazia(T - 1)).    
%% -----------------------------------------------------------------------------
%%                               PRODUTOR
%% -----------------------------------------------------------------------------
%% Método principal do processo de produção
produtor(Logger, Name, GerenciadorFila) ->
    Logger ! {log, Name, self(), produtorStarted},
    loopProducao(Logger, Name, GerenciadorFila).
%% -----------------------------------------------------------------------------
%% Loop de producao
loopProducao(Logger, Name, GerenciadorFila) ->
    Logger ! {log, Name, self(), {status, waiting}},
    timer:sleep(1000),
    GerenciadorFila ! {reserveSpace, Name, self()},
    receive
        listIsFull ->
            Logger ! {log, Name, self(), "Lista estava cheia, não vou produzir"};
        reserved -> 
            Logger ! {log, Name, self(), {status, producing}},
            timer:sleep(3000),
            GerenciadorFila ! {itemProduced, Name}
    end,
    loopProducao(Logger, Name, GerenciadorFila).
%% -----------------------------------------------------------------------------
%%                               CONSUMIDOR
%% -----------------------------------------------------------------------------
%% Método principal do processo de consumidor
consumidor(Logger, Name, GerenciadorFila) ->
    Logger ! {log, Name, self(), "started"},
    loopConsumidor(Logger, Name, GerenciadorFila).
%% -----------------------------------------------------------------------------
%% Loop de consumidor
loopConsumidor(Logger, Name, GerenciadorFila) ->
    timer:sleep(1000),
    Logger ! {log, Name, self(), " testaSeTemItensAConsumir"},
    GerenciadorFila ! {reserveConsumeItem, Name, self()},
    receive
        listIsEmpty -> 
            Logger ! {log, Name, self(), "semItensAConsumir"};
        reserved -> 
            Logger ! {log, Name, self(), "vouConsumir"},
            timer:sleep(3000),
            GerenciadorFila ! {itemConsumed, Name}
    end,
    loopConsumidor(Logger, Name, GerenciadorFila).
%% -----------------------------------------------------------------------------
%%                               UTILITARIOS
%% -----------------------------------------------------------------------------
%% Retorna o índice de um item em uma lista
indexOfStatus({Item}, List) -> indexOfStatus({Item}, List, 1).
indexOfStatus(_, [], _)  -> not_found;
indexOfStatus({Item}, [{Item, _, _, _}|_], Index) -> Index;
indexOfStatus({Item}, [_|Tl], Index) -> indexOfStatus({Item}, Tl, Index+1).
%% -----------------------------------------------------------------------------
%% Retorna o índice de um item em uma lista produzido por um produtor
indexOfProdutor({Item, Produtor}, List) -> indexOfProdutor({Item, Produtor}, List, 1).
indexOfProdutor(_, [], _)  -> not_found;
indexOfProdutor({Item, Produtor}, [{Item, _, Produtor, _}|_], Index) -> Index;
indexOfProdutor({Item, Produtor}, [_|Tl], Index) -> indexOfProdutor({Item, Produtor}, Tl, Index+1).
%% -----------------------------------------------------------------------------
%% Retorna o índice de um item em uma lista consumido por um consumidor
indexOfConsumidor({Item, Consumidor}, List) -> indexOfConsumidor({Item, Consumidor}, List, 1).
indexOfConsumidor(_, [], _)  -> not_found;
indexOfConsumidor({Item, Consumidor}, [{Item, _, _, Consumidor}|_], Index) -> Index;
indexOfConsumidor({Item, Consumidor}, [_|Tl], Index) -> indexOfConsumidor({Item, Consumidor}, Tl, Index+1).
%% -----------------------------------------------------------------------------
%% Transforma um valor em String
stringFormat(Values) ->
    lists:flatten(io_lib:format("~p", [Values])).