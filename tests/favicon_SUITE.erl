-module(favicon_SUITE).
-compile(export_all).

all() -> [
    slash,
    process_favicon_url
].

slash(_) -> 
    "/" = favicon:slash("http://example.com"),
    [] = favicon:slash("http://example.com/").

process_favicon_url(_) -> "".
    %favicon:process_favicon_url("http://example.com", "http://example.com").