%% -*- erlang -*-
%% Script Erlang para o servidor de dados (O que controla a fila)
-module(dataServer).
-export([main/1, testMethod/1]).

main(_) ->
    io:format("spawn ~s ~p\n", [os:getpid(), self()]),
    Pidzinho = spawn(?MODULE, testMethod, [self()]),
    io:format("fuck ~p\n", [Pidzinho]),
    receive
        {data, DataContent} -> io:format("Message", [])
    end.

testMethod(Pid) ->
    Pid ! {data, 1}.
