-module(rabbit_auth_backend_kerberos).

-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(rabbit_auth_backend).

-export([description/0, check_user_login/2, check_vhost_access/2, check_resource_access/3]).

description() ->
  [{name, <<"Kerberos">>},
    {description, <<"Kerberos authentication">>}].

check_user_login(Username, AuthProps) ->
  Kinit = kinit(Username, none),
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

% TODO Fault tolerance:
% * What if spawned binary doesn't exist?
% * Isn't executable?
kinit(_,_) ->
  Port = open_port({spawn_executable, "/bin/true"}, [exit_status]),

  receive
    {Port, {exit_status, 0}} ->
      rabbit_log:error("exit_status: 0~n"),
      true;
    {Port, {exit_status, 1}} ->
      rabbit_log:error("exit_status: 1~n"),
      false;
    {Port, {exit_status, A}} ->
      rabbit_log:error("exit_status: ~s~n", [A]),
      {error, A}
  after
    5000 ->
      {error, timeout}
  end.
