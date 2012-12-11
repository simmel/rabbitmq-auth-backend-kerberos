-module(kinit).

-export([init/0, kinit/2]).
-on_load(init/0).

init() ->
    Kinit = "/local/rabbitmq/mnesia/rabbit\@rabbitmq-test-disk01.it.su.se-plugins-expand/rabbitmq_auth_backend_kerberos-0.0.0/priv/kinit",
    Lol = erlang:load_nif(Kinit, 0),
    rabbit_log:error("WAT: ~p~n", [Lol]),
    Lol.

kinit(User, Password) -> exit(nif_library_not_loaded).
