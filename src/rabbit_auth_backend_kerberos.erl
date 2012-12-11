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
  Kinit = kinit:kinit(Username, Password),
  {ok, AuthZ_module} = application:get_env(?APPLICATION, authZ_module),
  rabbit_log:error("kinit: ~p ~p!~n", [?APPLICATION, Kinit]),
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
      {error, "Error", Error}
  end.

check_vhost_access(#user{username = Username}, VHost) ->
  true.

check_resource_access(#user{username = Username},
  #resource{virtual_host = VHost, kind = Type, name = Name},
  Permission) ->
  true.
