-module(favicon_SUITE).
-compile(export_all).

all() -> [
    module_exists,
    slash,
    process_favicon_url
].

module_exists(_) -> 
    true = code:add_path("."),
    true = code:add_path(".."),
    true = code:add_path("../.."),
    true = code:add_path("../../.."),
    {module, favicon} = code:load_file(favicon).

slash(_) -> 
    Url = "http://example.com",
    "/" = favicon:slash(Url),
    [] = favicon:slash(Url ++ "/").

process_favicon_url(_) -> 
    "http://example.com/favicon.ico" = favicon:process_favicon_url("http://example.com/favicon.ico", "http://example.com"),
    "https://example.com/favicon.ico" = favicon:process_favicon_url("https://example.com/favicon.ico", "https://example.com"),
    "http://example.com/favicon.ico" = favicon:process_favicon_url("//example.com/favicon.ico", "http://example.com"),
    "https://example.com/favicon.ico" = favicon:process_favicon_url("//example.com/favicon.ico", "https://example.com"),
    "http://example.com/favicon.ico" = favicon:process_favicon_url("/favicon.ico", "http://example.com"),
    "https://example.com/favicon.ico" = favicon:process_favicon_url("/favicon.ico", "https://example.com"),
    "http://example.com/favicon.ico" = favicon:process_favicon_url("favicon.ico", "http://example.com"),
    "https://example.com/favicon.ico" = favicon:process_favicon_url("favicon.ico", "https://example.com").