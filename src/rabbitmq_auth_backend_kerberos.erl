-module(rabbitmq_auth_backend_kerberos).

-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(rabbit_auth_backend).

-export([description/0, check_user_login/2, check_vhost_access/2, check_resource_access/3]).

description() ->
  [{name, <<"Kerberos">>},
    {description, <<"Kerberos authentication">>}].

check_user_login(Username, AuthProps) ->
  Password = proplists:get_value(password, AuthProps),
  Kinit = kinit(Username, Password),
  rabbit_log:error("kinit: ~p!~n", [Kinit]),
  case Kinit of
    true ->
      {ok, #user{username     = Username,
          tags         = [],
          auth_backend = ?MODULE,
          impl         = none}};
    false ->
      {refused, "Nope", []};
    {error, Error} ->
      rabbit_log:error("Error from kinit: ~p!~n", [Error]),
      {refused, "Error", []}
  end.

check_vhost_access(#user{username = Username}, VHost) ->
  true.

check_resource_access(#user{username = Username},
  #resource{virtual_host = VHost, kind = Type, name = Name},
  Permission) ->
  true.

kinit(User,Password) when is_binary(User) ->
  % On <= R14B args can only be a string()
  Username = binary_to_list(User),
  Kinit = code:priv_dir(?MODULE) ++ "/kinit",
  file:change_mode(Kinit, 8#00755),
  try open_port({spawn_executable, Kinit}, [
        exit_status,
        {args, [Username]},
        {line, 1024}
      ]) of
  Port when is_port(Port) -> kinit(Port, Password)
  catch
    error:E when E == enoent; E == eacces ->
      rabbit_log:error("Couldn't execute kinit process: ~p~n", [E]),
      false
  end;

kinit(Port, Password) when is_port(Port) ->
  Port ! {self(), {command, <<Password/binary,"\n">>}},
  loop(Port).

loop(Port) ->
  receive
    {Port, {data, {_, Data}}} ->
      % TODO change loglevel
      rabbit_log:error("~s", [Data]),
      false;
    {Port, {exit_status, 0}} ->
      rabbit_log:error("exit_status: 0~n"),
      loop(Port, true);
    {Port, {exit_status, 1}} ->
      rabbit_log:error("exit_status: 1~n"),
      loop(Port, false);
    {Port, {exit_status, A}} ->
      rabbit_log:error("exit_status: ~s~n", [A]),
      loop(Port, {error, A})
  after
    5000 ->
      {error, timeout}
  end.

loop(Port, Return) when is_boolean(Return); is_tuple(Return) ->
  receive
    {'EXIT', Port, Reason} ->
      rabbit_log:error("EXIT: ~p~n", [Reason]),
      Return
  after
    1000 ->
      {error, timeout}
  end.
