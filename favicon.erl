-module(favicon).
-export([start/3, favicon_searcher/1, saver_worker/1, extract_favicon_url/1, process_page/1, process_favicon_url/2]).

start(File_name, Threads_number, Result_file) -> 
    ssl:start(),
    inets:start(),
    Saver_PID = spawn(favicon, saver_worker, [Result_file]),
    register(saver, Saver_PID),
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
        {match, L_link} -> 
            [Link_pos|_] = L_link,
            {Link_s, Link_len} = Link_pos,
            %Just_link = binary:part(Page, Link_s, Link_len),
            Just_link = string:substr(Page, Link_s, Link_len),
            Href = re:run(Just_link, "href.*=.*(\"|')(.*\..*)(\"|')", [ungreedy, caseless]),
            case Href of
                {match, L_href} ->
                    [_, _, Href_pos|_] = L_href,
                    {Href_s, Href_len} = Href_pos,
                    %{ok, binary:part(Just_link, Href_s, Href_len)}
                    {ok, string:substr(Just_link, Href_s + 1, Href_len)};
                _ -> {error, nothing}
            end;
        _ -> {error, nothing}
    end.

process_favicon_url(Favicon_url, Url) -> 
    case Favicon_url of
        [47, 47|Clear_favicon_url] ->
            Math = re:run(Url, "^(http.*://)", [ungreedy, caseless]),
            case Math of
                {match, Positions} -> 
                    [_, {Scheme_s, Scheme_len}|_] = Positions,
                    Scheme = string:substr(Url, Scheme_s + 1, Scheme_len),
                    Scheme ++ Clear_favicon_url;
                _ -> 
                    io:format("else~n", []),
                    Url ++ Clear_favicon_url
             end;
        [47|_] -> 
            Url ++ Favicon_url;
        _ -> Favicon_url
    end.

process_page(Url) -> 
    Response = httpc:request(Url),
    case Response of
        {ok, {{_, Code, _}, _, Page}} when Code == 200 -> 
            Favicon_result = extract_favicon_url(Page),
            case Favicon_result of
                {ok, Favicon_url} -> {ok, process_favicon_url(Favicon_url, Url)};
                {error, nothing} -> {error, Url}
            end;
        {error, Reason} -> 
            io:format("Not loaded: ~s. Reason ~w~n", [Url, Reason]),
            {error, Url};
        _ -> {error, Url}
    end.

favicon_searcher(Server_PID) -> 
    Server_PID ! {pid, self()},
    Timeout = 5000,
    receive
        finish -> io:format("Worker ~w is off~n", [self()]);
        {url, Url} -> 
            io:format("Process ~s ~n", [Url]),
            {Result, Favicon_url} = process_page(Url),
            saver ! {url, Result, Favicon_url},
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
    Timeout = 60000,
    receive
        finish -> io:format("Saver is off~n", []);
        {url, ok, Url} -> 
            save_favicon_url(Url, Result_file),
            saver_worker(Result_file);
        {url, error, Url} -> 
            save_favicon_url(Url, "bad_" ++ Result_file),
            saver_worker(Result_file)
        after Timeout ->
            io:format("Saver finish after waiting ~w ms~n", [Timeout])
    end.

save_favicon_url(Url, Result_file) -> 
    Writed = file:write_file(Result_file, list_to_binary(Url ++ "\n"), [append]),
    case Writed of
        ok -> io:format("Saved url: ~s~n", [Url]);
        {error, Reason} -> io:format("Not saved url: ~s. Reason ~w~n", [Url, Reason])
    end,
    Writed.