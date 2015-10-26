-module(favicon).
-export([start/2, favicon_searcher/2]).

start(File_name, Threads_number) -> 
    start_thread(Threads_number),
    {ok, Binary_urls} = file:read_file(File_name),
    Raw_urls = binary:split(Binary_urls, <<"\n">>, [global]),
    send_urls(Raw_urls, Threads_number).

    
start_thread(0) -> io:format("Spawning done~n", []);
start_thread(Number) -> 
    spawn(favicon, favicon_searcher, [self(), true]),
    start_thread(Number - 1).

favicon_searcher(Server_PID, true) -> 
    Server_PID ! {pid, self()},
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

send_urls([], 0) -> 
    io:format("All done~n", []);
send_urls([], Threads_number) -> 
    Timeout = 5000,
    receive
        {pid, PID} -> 
            PID ! finish,
            send_urls([], Threads_number - 1);
        {PID, Favicon_url} -> 
            PID ! finish,
            save_favicon_url(Favicon_url),
            send_urls([], Threads_number - 1)
    after Timeout ->
        io:format("Finish after waiting ~w ms~n", [Timeout])
    end;
send_urls([Raw_url|Raw_urls], Threads_number) -> 
    Url = binary_to_atom(Raw_url, utf8),
    receive
        {pid, PID} -> PID ! {url, Url};
        {PID, Favicon_url} -> 
            PID ! {url, Url},
            save_favicon_url(Favicon_url)
    end,
    send_urls(Raw_urls, Threads_number).

save_favicon_url(Url) -> io:format("Save url ~s~n", [Url]).