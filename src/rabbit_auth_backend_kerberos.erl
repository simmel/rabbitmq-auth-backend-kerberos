-module(rabbit_auth_backend_kerberos).

-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(rabbit_auth_backend).

-define(APPLICATION, begin {ok, A} = application:get_application(?MODULE), A end).

-export([description/0, check_user_login/2, check_vhost_access/2, check_resource_access/3]).

description() ->
  [{name, <<"Kerberos">>},
   {description, <<"Kerberos authentication">>}].

check_user_login(Username, AuthProps) ->
  Password = proplists:get_value(password, AuthProps),
  Kinit = kinit(Username, Password),
  {ok, AuthZ_module} = application:get_env(?APPLICATION, authZ_module),
  rabbit_log:error("kinit: ~p!~n", [Kinit]),
  case Kinit of
    true ->
      {ok, #user{username     = Username,
                 tags         = [],
                 auth_backend = AuthZ_module,
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

% Needed because 2.x uses an undefined password sometimes
% See http://article.gmane.org/gmane.comp.networking.rabbitmq.general/20092
kinit(User, Password) when Password =:= undefined -> false;

kinit(User, Password) when is_binary(User) ->
  % On <= R14B args can only be a string()
  Username = binary_to_list(User),
  Kinit = code:priv_dir(?APPLICATION) ++ "/kinit",
  file:change_mode(Kinit, 8#00755),
  try open_port({spawn_executable, Kinit}, [
        exit_status,
        eof,
        {args, [Username]},
        {line, 1024}
      ]) of
  Port when is_port(Port) -> kinit(Port, User, Password)
  catch
    error:E when E == enoent; E == eacces ->
      rabbit_log:error("Couldn't execute kinit process: ~p~n", [E]),
      false
  end.

kinit(Port, User, Password) when is_port(Port) ->
  Port ! {self(), {command, <<Password/binary,"\n">>}},
  loop(Port, []).

loop(Port, Acc) ->
  receive
    {Port, {data, {_, Data}}} ->
      % TODO change loglevel
      rabbit_log:error("~s", [Data]),
      loop(Port, [false|Acc]);
    {Port, {exit_status, 0}} ->
      rabbit_log:error("exit_status: 0~n"),
      loop(Port, [true|Acc]);
    {Port, {exit_status, 1}} ->
      rabbit_log:error("exit_status: 1~n"),
      loop(Port, [false|Acc]);
    {Port, eof} ->
      rabbit_log:error("eof: ~p~n", [Acc]),
      [Return|_] = Acc,
      Return;
    {Port, {exit_status, A}} ->
      rabbit_log:error("exit_status: ~s~n", [A]),
      loop(Port, [{error, A}|Acc])
  after
    5000 ->
      [{error, timeout}|Acc]
  end.
