%% -*- erlang -*-
%% Script Erlang para o servidor de dados (O que controla a fila)
-module(dataServer).
-export([main/1, gerenciadorFila/3, produtor/2, criaListaVazia/1]).

main(_) ->
    GerenciadorFila = spawn(?MODULE, gerenciadorFila, [self(), "GerenciadorFila", 10]),
    spawn(?MODULE, produtor, [self(), "Produtor1"]),
    messageReceiveLoop().

messageReceiveLoop() ->
    receive
        {log, Name, Pid, Message} -> 
            io:format("~s ~p ~s\n", [Name, Pid, Message]),
            messageReceiveLoop()
    end.

gerenciadorFila(MainPid, Name, TamanhoFila) ->
    MainPid ! {log, Name, self(), "Iniciei o gerenciador da fila"},
    Fila = criaListaVazia(TamanhoFila),
    MainPid ! {log, Name, self(), Fila}.

criaListaVazia(0) ->
    [];
criaListaVazia(T) ->
    lists:append([65], criaListaVazia(T - 1)).    

produtor(MainPid, Name) ->
    MainPid ! {log, Name, self(), "Iniciado"}.

