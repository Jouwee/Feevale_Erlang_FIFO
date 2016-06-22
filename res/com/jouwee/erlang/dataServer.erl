%% -*- erlang -*-
%% Script Erlang para o servidor de dados (O que controla a fila)
-module(dataServer).
-export([main/1, criaProdutores/5, criaConsumidores/5, gerenciadorFila/3, produtor/6, criaListaVazia/1, loopProducao/6, consumidor/6, loopConsumidor/6]).
%% -----------------------------------------------------------------------------
%% Função principal do Script
main([StringTamanhoFila, StringNumeroProdutores, StringNumeroConsumidores, StringTempoMedioProducao, StringDesvioPadraoProducao, StringTempoMedioConsumo, StringDesvioPadraoConsumo, StringTempoEspera]) ->
    %% Converte os parâmetros para numeros
    {TamanhoFila, Rest} = string:to_integer(StringTamanhoFila), 
    {NumeroProdutores, Rest} = string:to_integer(StringNumeroProdutores), 
    {NumeroConsumidores, Rest} = string:to_integer(StringNumeroConsumidores), 
    {TempoMedioProducao, Rest} = string:to_integer(StringTempoMedioProducao), 
    {DesvioPadraoProducao, Rest} = string:to_integer(StringDesvioPadraoProducao), 
    {TempoMedioConsumo, Rest} = string:to_integer(StringTempoMedioConsumo), 
    {DesvioPadraoConsumo, Rest} = string:to_integer(StringDesvioPadraoConsumo), 
    {TempoEspera, Rest} = string:to_integer(StringTempoEspera), 
    GerenciadorFila = spawn(?MODULE, gerenciadorFila, [self(), "GerenciadorFila", TamanhoFila]),
    criaProdutores(GerenciadorFila, NumeroProdutores, TempoMedioProducao, DesvioPadraoProducao, TempoEspera),
    criaConsumidores(GerenciadorFila, NumeroConsumidores, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera),
    messageReceiveLoop().
%% -----------------------------------------------------------------------------
%% Cria os produtores
criaProdutores(GerenciadorFila, 0, TempoMedioProducao, DesvioPadraoProducao, TempoEspera) ->
    done;
criaProdutores(GerenciadorFila, NumeroProdutores, TempoMedioProducao, DesvioPadraoProducao, TempoEspera) ->
    NomeProdutor = string:concat("Produtor",stringFormat(NumeroProdutores)),
    spawn(?MODULE, produtor, [self(), NomeProdutor, GerenciadorFila, TempoMedioProducao, DesvioPadraoProducao, TempoEspera]),
    criaProdutores(GerenciadorFila, NumeroProdutores-1, TempoMedioProducao, DesvioPadraoProducao, TempoEspera).
%% -----------------------------------------------------------------------------
%% Cria os consumidores
criaConsumidores(GerenciadorFila, 0, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera) ->
    done;
criaConsumidores(GerenciadorFila, NumeroConsumidores, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera) ->
    NomeConsumidor = string:concat("Consumidor",stringFormat(NumeroConsumidores)),
    spawn(?MODULE, consumidor, [self(), NomeConsumidor, GerenciadorFila, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera]),
    criaConsumidores(GerenciadorFila, NumeroConsumidores-1, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera).
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
produtor(Logger, Name, GerenciadorFila, TempoMedioProducao, DesvioPadraoProducao, TempoEspera) ->
    {A1,A2,A3} = now(), 
    random:seed(A1, A2, A3), 
    random:uniform(),
    Logger ! {log, Name, self(), produtorStarted},
    loopProducao(Logger, Name, GerenciadorFila, TempoMedioProducao, DesvioPadraoProducao, TempoEspera).
%% -----------------------------------------------------------------------------
%% Loop de producao
loopProducao(Logger, Name, GerenciadorFila, TempoMedioProducao, DesvioPadraoProducao, TempoEspera) ->
    Logger ! {log, Name, self(), {status, waiting}},
    timer:sleep(TempoEspera),
    GerenciadorFila ! {reserveSpace, Name, self()},
    receive
        listIsFull ->
            Logger ! {log, Name, self(), "Lista estava cheia, não vou produzir"};
        reserved -> 
            Logger ! {log, Name, self(), {status, producing}},
            wait(Logger, Name, TempoMedioProducao + (random:uniform(DesvioPadraoProducao * 2) - DesvioPadraoProducao)),
            GerenciadorFila ! {itemProduced, Name}
    end,
    loopProducao(Logger, Name, GerenciadorFila, TempoMedioProducao, DesvioPadraoProducao, TempoEspera).
%% -----------------------------------------------------------------------------
%%                               CONSUMIDOR
%% -----------------------------------------------------------------------------
%% Método principal do processo de consumidor
consumidor(Logger, Name, GerenciadorFila, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera) ->
    {A1,A2,A3} = now(), 
    random:seed(A1, A2, A3), 
    random:uniform(),
    Logger ! {log, Name, self(), consumidorStarted},
    loopConsumidor(Logger, Name, GerenciadorFila, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera).
%% -----------------------------------------------------------------------------
%% Loop de consumidor
loopConsumidor(Logger, Name, GerenciadorFila, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera) ->
    Logger ! {log, Name, self(), {status, waiting}},
    timer:sleep(TempoEspera),
    GerenciadorFila ! {reserveConsumeItem, Name, self()},
    receive
        listIsEmpty -> 
            done;
        reserved -> 
            Logger ! {log, Name, self(), {status, consuming}},
            wait(Logger, Name, TempoMedioConsumo + (random:uniform(DesvioPadraoConsumo * 2) - DesvioPadraoConsumo)),
            GerenciadorFila ! {itemConsumed, Name}
    end,
    loopConsumidor(Logger, Name, GerenciadorFila, TempoMedioConsumo, DesvioPadraoConsumo, TempoEspera).
%% -----------------------------------------------------------------------------
%%                               UTILITARIOS
%% -----------------------------------------------------------------------------
%% Aguarda um tempo
wait(Logger, Name, Total) -> wait(Logger, Name, Total, Total).
wait(Logger, Name, Total, Remaining) -> 
    Logger ! {log, Name, self(), {progress, 1 - (Remaining / Total)}},
    if 
        Remaining < 10 ->
            timer:sleep(Remaining);
        true ->
            timer:sleep(10),
            wait(Logger, Name, Total, Remaining - 10)
    end.
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