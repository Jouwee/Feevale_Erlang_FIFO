%% -*- erlang -*-
%% Script Erlang para o servidor de dados (O que controla a fila)
-module(dataServer).
-export([main/1, gerenciadorFila/3, produtor/3, criaListaVazia/1, loopProducao/3, gerenciadorFilaLoop/4, consumidor/3, loopConsumidor/3]).
%% -----------------------------------------------------------------------------
%% Função principal do Script
main(_) ->
    GerenciadorFila = spawn(?MODULE, gerenciadorFila, [self(), "GerenciadorFila", 10]),
    spawn(?MODULE, produtor, [self(), "Produtor1", GerenciadorFila]),
    spawn(?MODULE, produtor, [self(), "Produtor2", GerenciadorFila]),
    spawn(?MODULE, consumidor, [self(), "Consumidor1", GerenciadorFila]),
    messageReceiveLoop().
%% -----------------------------------------------------------------------------
%% Loop de recebimento de mensagens para printar no output
messageReceiveLoop() ->
    receive
        {log, Name, Pid, Message} -> 
            io:format("~s ~p ~p\n", [Name, Pid, Message]),
            messageReceiveLoop()
    end.
%% -----------------------------------------------------------------------------
%%                            GERENCIADOR DA FILA
%% -----------------------------------------------------------------------------
%% Método principal do processo de gerenciamento de filas
gerenciadorFila(Logger, Name, TamanhoFila) ->
    Logger ! {log, Name, self(), "Iniciei o gerenciador da fila"},
    Fila = criaListaVazia(TamanhoFila),
    Logger ! {log, Name, self(), Fila},
    gerenciadorFilaLoop(Logger, Name, TamanhoFila, Fila).
%% -----------------------------------------------------------------------------
%% Loop de mensagens do gerenciador
gerenciadorFilaLoop(Logger, Name, TamanhoFila, Fila) ->
    receive
        {itemProduced, NameProdutor} -> 
            gerenciadorFilaLoop(Logger, Name, TamanhoFila, produzItem(Logger, NameProdutor, Fila));
        {itemConsumed, NameConsumidor} -> 
            gerenciadorFilaLoop(Logger, Name, TamanhoFila, consomeItem(Logger, NameConsumidor, Fila))
    end,
    gerenciadorFilaLoop(Logger, Name, TamanhoFila, Fila).
%% -----------------------------------------------------------------------------
%% Produz um item
produzItem(Logger, Name, Fila) ->
    Logger ! {log, Name, self(), "addToList"},
    IndexFirstZero = indexOf(0, Fila),
    if
        IndexFirstZero == not_found -> 
            Retorno = Fila,
            Logger ! {log, Name, self(), "listIsFull"};
        true -> 
            Retorno = lists:sublist(Fila,IndexFirstZero - 1) ++ [1] ++ lists:nthtail(IndexFirstZero,Fila)
    end,
    Logger ! {log, Name, self(), Retorno},
    Retorno.
%% -----------------------------------------------------------------------------
%% Consome um item
consomeItem(Logger, Name, Fila) ->
    Logger ! {log, Name, self(), "consomeItem"},
    PrimeiraPosicao = lists:nth(1, Fila),
    if
        PrimeiraPosicao == 0 -> 
            Retorno = Fila,
            Logger ! {log, Name, self(), "listIsEmpty"};
        true -> 
            Retorno = lists:nthtail(1, Fila) ++ [0]
    end,
    Logger ! {log, Name, self(), Retorno},
    Retorno.
%% -----------------------------------------------------------------------------
%% Cria uma lista vazia (Cheia de zeros)
criaListaVazia(0) ->
    [];
criaListaVazia(T) ->
    lists:append([0], criaListaVazia(T - 1)).    
%% -----------------------------------------------------------------------------
%%                               PRODUTOR
%% -----------------------------------------------------------------------------
%% Método principal do processo de produção
produtor(Logger, Name, GerenciadorFila) ->
    Logger ! {log, Name, self(), "started"},
    loopProducao(Logger, Name, GerenciadorFila).
%% -----------------------------------------------------------------------------
%% Loop de producao
loopProducao(Logger, Name, GerenciadorFila) ->
    timer:sleep(1000),
    Logger ! {log, Name, self(), " produziu"},
    GerenciadorFila ! {itemProduced, Name},
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
    Logger ! {log, Name, self(), " consumiu"},
    GerenciadorFila ! {itemConsumed, Name},
    loopConsumidor(Logger, Name, GerenciadorFila).
%% -----------------------------------------------------------------------------
%%                               UTILITARIOS
%% -----------------------------------------------------------------------------
%% Retorna o índice de um item em uma lista
indexOf(Item, List) -> indexOf(Item, List, 1).
indexOf(_, [], _)  -> not_found;
indexOf(Item, [Item|_], Index) -> Index;
indexOf(Item, [_|Tl], Index) -> indexOf(Item, Tl, Index+1).
