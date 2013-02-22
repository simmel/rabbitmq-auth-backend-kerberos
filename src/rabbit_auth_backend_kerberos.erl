-module(rabbit_auth_backend_kerberos).

-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(rabbit_auth_backend).

-define(APPLICATION, begin {ok, A} = application:get_application(?MODULE), A end).

-export([init/0, kinit/2, description/0, check_user_login/2, check_vhost_access/2, check_resource_access/3]).
-on_load(init/0).

init() ->
    Kinit = filename:join(code:priv_dir(?APPLICATION), "kinit"),
    erlang:load_nif(Kinit, 0).

kinit(User, Password) -> exit(nif_library_not_loaded).

description() ->
  [{name, <<"Kerberos">>},
   {description, <<"Kerberos authentication">>}].

check_user_login(Username, AuthProps) ->
  Password = proplists:get_value(password, AuthProps),
  Kinit = kinit(Username, Password),
  {ok, AuthZ_module} = application:get_env(?APPLICATION, authZ_module),
  User_record = AuthZ_module:lookup_user(Username),
  Tags = case User_record of
    {ok, _ = #internal_user{tags = T}} -> T;
    _ -> []
  end,
  Empty_password = case User_record of
    {ok, _ = #internal_user{password_hash = <<>>}} -> true;
    _ -> false
  end,
  case Kinit of
    true when Empty_password ->
      {ok, #user{username     = Username,
                 tags         = Tags,
                 auth_backend = AuthZ_module,
                 impl         = none}};
    {error, Error} ->
      rabbit_log:error("Error from kinit: ~p!~n", [Error]),
      {error, "Error", Error};
    _ ->
      {refused, "Nope", []}
  end.

check_vhost_access(#user{username = Username}, VHost) ->
  true.

check_resource_access(#user{username = Username},
  #resource{virtual_host = VHost, kind = Type, name = Name},
  Permission) ->
  true.
