-module(favicon).
-export([start/1, favicon_searcher/1]).

start(Path, Threads) when if_string(Path), is_file(Path), is_integer(Threads), Threads > 0 -> 
    start_thread(Threads),
    
    
start_thread(0) -> io.format("Spawning done~n", [])
start_thread(Number) -> 
    spawn(favicon, favicon_searcher, [self(), true]),
    start_thread(Number - 1)
                   
stop_thread([]) -> io.format("Stopping done~n", []);
stop_thread([Head|Tail]) -> 
    Head ! finish,
    stop_thread(Tail).

                         
favicon_searcher(Server_PID, Need_notify) -> 
    if 
        Need_notify -> Server_PID ! self()
    end,
    receive
        finish -> io.format("Worker ~w is off~n", [self()]);
        {url, Url} -> 
            io.format("Process: ~n", [Url]),
            favicon_searcher(Server_PID, false)
    end.