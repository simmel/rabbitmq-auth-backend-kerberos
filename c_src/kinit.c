/* Copyright (c) 2013-, Simon Lundström, IT Services
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 * 
 * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 * disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of Stockholm University nor the names of its contributors may be used to endorse or promote products
 * derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <erl_nif.h>
#include <krb5.h>
#include <stdio.h>
#include <string.h>

// MIT is broken
#ifdef MIT
#define KRB5KDC_ERR_KEY_EXPIRED KRB5KDC_ERR_KEY_EXP
#endif

struct kebab {
  krb5_error_code error;
  krb5_principal  principal;
  krb5_context    context;
  krb5_creds      creds;
  char           *password;
  int             password_size;
};

ERL_NIF_TERM error_and_exit(ErlNifEnv* env, struct kebab *kebab, char *tag) {
  char fmt_error_msg[1024];
  const char * krb_error_msg = krb5_get_error_message(kebab->context, kebab->error);
  memset(fmt_error_msg, 0, (int)sizeof(fmt_error_msg));
  snprintf(fmt_error_msg, (int)sizeof(fmt_error_msg), "%s: (%i) %s", tag, kebab->error, krb_error_msg);
  ERL_NIF_TERM error_message = enif_make_string(env, fmt_error_msg, ERL_NIF_LATIN1);
  ERL_NIF_TERM return_atom;

  if (krb_error_msg != NULL)
    krb5_free_error_message(kebab->context, krb_error_msg);

  if (kebab->error == 0)
     return_atom = enif_make_atom(env, "ok");
  // FIXME This should be done in a better way
  else if (kebab->error == KRB5KDC_ERR_C_PRINCIPAL_UNKNOWN ||
           kebab->error == KRB5KRB_AP_ERR_BAD_INTEGRITY ||
           kebab->error == KRB5KDC_ERR_PREAUTH_FAILED ||
           kebab->error == KRB5KDC_ERR_KEY_EXPIRED
          )
     return_atom = enif_make_atom(env, "refused");
  else
    return_atom = enif_make_atom(env, "error");

  if (kebab->context) {
    if (&kebab->creds)
      krb5_free_cred_contents(kebab->context, &kebab->creds);
    if (kebab->principal) {
      krb5_free_principal(kebab->context, kebab->principal);
    }
    krb5_free_context(kebab->context);
  }

  return enif_make_tuple2(env, return_atom, error_message);
}

static ERL_NIF_TERM kinit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  struct kebab kebab;

  ErlNifBinary username;
  ErlNifBinary password;

  // Input needs to be binary
  if (!enif_is_binary(env, argv[0]) || !enif_is_binary(env, argv[1]))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &username) || !enif_inspect_binary(env, argv[1], &password))
    return enif_make_badarg(env);

  kebab.password = (char *)password.data;
  kebab.password_size = password.size;

  // Binaries are not NULL-terminated, let's fix that.
  password.data[password.size] = '\0';
  username.data[username.size] = '\0';

  // Initialize krb5 context
  if ((kebab.error = krb5_init_context(&kebab.context)))
    return error_and_exit(env, &kebab, "krb5_init_context");

  memset(&kebab.creds, 0, sizeof(krb5_creds));

  // Create the principal name from user and /etc/krb5.conf
  if ((kebab.error = krb5_parse_name(kebab.context, (char *)username.data, &kebab.principal)))
    return error_and_exit(env, &kebab, "krb5_parse_name");

  // Do kinit
  if ((kebab.error = krb5_get_init_creds_password(
          kebab.context, &kebab.creds, kebab.principal, (char *)password.data, 0, NULL, 0, NULL, NULL)))
    return error_and_exit(env, &kebab, "krb5_get_init_creds_password");

  return error_and_exit(env, &kebab, NULL);
}

int noop(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"kinit", 2, kinit}
};

ERL_NIF_INIT(rabbit_auth_backend_kerberos, nif_funcs, NULL, NULL, noop, NULL)
