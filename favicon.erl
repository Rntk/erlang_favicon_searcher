-module(favicon).
-export([start/2, favicon_searcher/2]).

start(File_name, Threads) -> 
    start_thread(Threads),
    {ok, Binary_urls} = file:read_file(File_name),
    Raw_urls = binary:split(Binary_urls, <<"\n">>, [global]),
    send_urls(Raw_urls).
    
    
start_thread(0) -> io:format("Spawning done~n", []);
start_thread(Number) -> 
    spawn(favicon, favicon_searcher, [self(), true]),
    start_thread(Number - 1).

favicon_searcher(Server_PID, true) -> 
    Server_PID ! {self()},
    favicon_searcher(Server_PID, false);
favicon_searcher(Server_PID, false) -> 
    Timeout = 5000,
    receive
        finish -> io:format("Worker ~w is off~n", [self()]);
        {url, Url} -> 
            io:format("Process ~w: ~n", [Url]),
            Server_PID ! {self(), Url},
            favicon_searcher(Server_PID, false)
        after Timeout ->
            io:format("Worker ~w is off after waiting ~w ms~n", [self(), Timeout])
    end.

send_urls([]) -> 
    Timeout = 5000,
    receive
        {PID, Favicon_url} -> 
            PID ! finish,
            save_favicon_url(Favicon_url),
            send_urls([])
    after Timeout ->
        io:format("Finish after waiting ~w ms~n", [Timeout])
    end;
send_urls([Raw_url|Raw_urls]) -> 
    Url = binary_to_atom(Raw_url, utf8),
    receive
        {PID} -> PID ! {url, Url};
        {PID, Favicon_url} -> 
            PID ! {url, Url},
            save_favicon_url(Favicon_url)
    end,
    send_urls(Raw_urls).

save_favicon_url(Url) -> io:format("Save url ~s~n", [Url]).