#include <erl_nif.h>

static ERL_NIF_TERM kinit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM error_message = enif_make_string(env, "NEIN NEIN NEIN", ERL_NIF_LATIN1);
    ERL_NIF_TERM return_atom = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, return_atom, error_message);
}

int noop(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"kinit", 2, kinit}
};

ERL_NIF_INIT(rabbit_auth_backend_kerberos, nif_funcs, NULL, NULL, noop, NULL)
