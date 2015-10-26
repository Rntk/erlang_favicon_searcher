-module(favicon).
-export([start/3, favicon_searcher/1, saver_worker/1, extract_favicon_url/1]).

start(File_name, Threads_number, Result_file) -> 
    %inet:start(),
    register(saver, spawn(favicon, saver_worker, [Result_file])),
    start_thread(Threads_number),
    {ok, Binary_urls} = file:read_file(File_name),
    Raw_urls = binary:split(Binary_urls, <<"\n">>, [global]),
    send_urls(Raw_urls, Threads_number).
    
start_thread(0) -> io:format("Spawning done~n", []);
start_thread(Number) -> 
    spawn(favicon, favicon_searcher, [self()]),
    start_thread(Number - 1).

extract_favicon_url(Page) -> 
    Link = re:run(Page, "<link.*rel.*=.*icon.*>", [ungreedy, caseless]),
    case Link of
        nothing -> "";
        {match, L_link} -> 
            [Link_pos|_] = L_link,
            {Link_s, Link_len} = Link_pos,
            Just_link = binary:part(Page, Link_s, Link_len),
            Href = re:run(Just_link, "href.*=.*(\"|')(.*\..*)(\"|').*", [ungreedy, caseless]),
            case Href of
                nothing -> "";
                {match, L_href} ->
                    [Href_pos|_] = L_href,
                    {Href_s, Href_len} = Href_pos,
                    binary:part(Just_link, Href_s, Href_len);
                _Else ->
                    io:format("Strange href ~w~n", [Href]),
                    ""
            end;
        _Else -> 
            io:format("Strange <link> ~w~n", [Link]),
            ""
    end.

favicon_searcher(Server_PID) -> 
    Server_PID ! {pid, self()},
    Timeout = 5000,
    receive
        finish -> io:format("Worker ~w is off~n", [self()]);
        {url, Url} -> 
            io:format("Process ~s ~n", [Url]),
            %Page = httpc:request(Url),
            Page = Url,
            Favicon_url = extract_favicon_url(Page),
            saver ! {url, Favicon_url},
            favicon_searcher(Server_PID)
        after Timeout ->
            io:format("Worker ~w is off after waiting ~w ms~n", [self(), Timeout])
    end.

send_urls([], 0) -> 
    saver ! finish,
    io:format("All done~n", []);
send_urls([], Threads_number) -> 
    Timeout = 5000,
    receive
        {pid, PID} -> 
            PID ! finish,
            send_urls([], Threads_number - 1)
    after Timeout ->
        io:format("Finish after waiting ~w ms~n", [Timeout])
    end;
send_urls([Raw_url|Raw_urls], Threads_number) -> 
    Url = binary:bin_to_list(Raw_url),
    receive
        {pid, PID} -> PID ! {url, Url}
    end,
    send_urls(Raw_urls, Threads_number).

saver_worker(Result_file) ->
    receive
        finish -> io:format("Saver is off~n", []);
        {url, Url} -> 
            save_favicon_url(Url, Result_file),
            saver_worker(Result_file)
    end.

save_favicon_url(Url, Result_file) -> 
    Writed = file:write_file(Result_file, list_to_binary(Url ++ "\n"), [append]),
    case Writed of
        ok -> io:format("Saved url: ~s~n", [Url]);
        {error, Reason} -> io:format("Not saved url: ~s. Reason ~w~n", [Url, Reason])
    end.