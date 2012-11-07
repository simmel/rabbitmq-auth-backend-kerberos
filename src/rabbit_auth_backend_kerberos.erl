-module(rabbit_auth_backend_kerberos).

-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(rabbit_auth_backend).

-export([description/0, check_user_login/2, check_vhost_access/2, check_resource_access/3]).

description() ->
  [{name, <<"Kerberos">>},
    {description, <<"Kerberos authentication">>}].

check_user_login(Username, AuthProps) ->
  true.

check_vhost_access(#user{username = Username}, VHost) ->
  true.

check_resource_access(#user{username = Username},
  #resource{virtual_host = VHost, kind = Type, name = Name},
  Permission) ->
  true.
