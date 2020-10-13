% Copyright (c) 2013-, Simon Lundström, IT Services
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% Redistributions of source code must retain the above copyright notice, this
% list of conditions and the following disclaimer.
%
% Redistributions in binary form must reproduce the above copyright notice,
% this list of conditions and the following disclaimer in the documentation
% and/or other materials provided with the distribution.
%
% Neither the name of Stockholm University nor the names of its contributors
% may be used to endorse or promote products derived from this software without
% specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.

-module(rabbit_auth_backend_kerberos).

-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(rabbit_authn_backend).

-define(APPLICATION, begin {ok, A} = application:get_application(?MODULE), A end).

-export([init/0, kinit/2, description/0, user_login_authentication/2]).

-rabbit_boot_step(
  {?MODULE,
   [{description, "RabbitMQ Kerberos Authentication Backend"},
    {mfa,         {?MODULE, init,
                   []}},
    {requires,    rabbit_registry},
    {enables,     kernel_ready}]}).


init() ->
  Kinit = filename:join(code:priv_dir(?APPLICATION), ?APPLICATION),
  erlang:load_nif(Kinit, 0).

kinit(_, _) -> exit(nif_library_not_loaded).

description() ->
  [{name, <<"Kerberos">>},
   {description, <<"Kerberos authentication">>}].

user_login_authentication(Username, AuthProps) ->
  Password = proplists:get_value(password, AuthProps),
  Kinit = kinit(Username, Password),
  Ret = rabbit_auth_backend_internal:lookup_user(Username),
  {Tags, Empty_password} = case Ret of
    {ok, User_record} ->
      {internal_user:get_tags(User_record),
       internal_user:get_password_hash(User_record) =:= <<>>};
    _ ->
      {[], false}
  end,
  case Empty_password of
    true ->
      case Kinit of
        {ok, _} ->
          {ok, #auth_user{username     = Username,
              tags         = Tags,
              impl         = none}};
        {refused, Error} ->
          {refused, Error, []};
        Error ->
          Error
      end;
    false ->
      {refused, "User '~s' exists in internal database and has a password set, not authenticating user with Kerberos.", [Username]}
  end.
