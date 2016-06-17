%% -*- erlang -*-
%% Script Erlang para o servidor de dados (O que controla a fila)
-module(dataServer).
-export([main/1, gerenciadorFila/3, produtor/2, criaListaVazia/1, loopProducao/2]).
%% -----------------------------------------------------------------------------
%% Função principal do Script
main(_) ->
    GerenciadorFila = spawn(?MODULE, gerenciadorFila, [self(), "GerenciadorFila", 10]),
    spawn(?MODULE, produtor, [self(), "Produtor1"]),
    messageReceiveLoop().
%% -----------------------------------------------------------------------------
%% Loop de recebimento de mensagens para printar no output
messageReceiveLoop() ->
    receive
        {log, Name, Pid, Message} -> 
            io:format("~s ~p ~s\n", [Name, Pid, Message]),
            messageReceiveLoop()
    end.
%% -----------------------------------------------------------------------------
%%                            GERENCIADOR DA FILA
%% -----------------------------------------------------------------------------
%% Método principal do processo de gerenciamento de filas
gerenciadorFila(MainPid, Name, TamanhoFila) ->
    MainPid ! {log, Name, self(), "Iniciei o gerenciador da fila"},
    Fila = criaListaVazia(TamanhoFila),
    MainPid ! {log, Name, self(), Fila}.
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
produtor(Logger, Name) ->
    Logger ! {log, Name, self(), "started"},
    loopProducao(Logger, Name).
%% -----------------------------------------------------------------------------
%% Loop de producao
loopProducao(Logger, Name) ->
    timer:sleep(1000),
    Logger ! {log, Name, self(), " produziu"},
    loopProducao(Logger, Name).
%% -----------------------------------------------------------------------------
%%                               CONSUMIDOR
%% -----------------------------------------------------------------------------
