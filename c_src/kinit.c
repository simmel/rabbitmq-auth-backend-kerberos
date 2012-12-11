#include <erl_nif.h>

static ERL_NIF_TERM kinit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}

static ErlNifFunc nif_funcs[] = {
    {"kinit", 2, kinit}
};

ERL_NIF_INIT(kinit, nif_funcs, NULL, NULL, NULL, NULL)
