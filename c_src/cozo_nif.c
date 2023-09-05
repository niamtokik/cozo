/*
 *
 */
#include <ei.h>
#include <erl_nif.h>
#include "cozo_c.h"

/*
 *
 */
extern char *cozo_open_db(const char *engine, const char *path, const char *options, int32_t *db_id);
extern char *cozo_run_query(int32_t db_id, const char *script_raw, const char *params_raw, bool immutable_query);
extern bool cozo_close_db(int32_t id);
extern void cozo_free_str(char *s);

ERL_NIF_TERM atom_ok(ErlNifEnv *env) {
  const char* atom = "ok";
  return enif_make_atom(env, atom);
}

ERL_NIF_TERM atom_error(ErlNifEnv *env) {
  const char* atom = "error";
  return enif_make_atom(env, atom);
}

static ERL_NIF_TERM open_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  char *error = cozo_open_db("mem", "/tmp/test.db", "", &db_id);
  if (error) {
      cozo_free_str(error);
      return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "open_error"));
  }
  return enif_make_tuple2(env, atom_ok(env), enif_make_int(env, db_id));
}

static ERL_NIF_TERM close_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
  bool ret = cozo_close_db(db_id);
  if (ret) {
    return atom_ok(env);
  }
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "close_error"));
}

extern char *cozo_run_query(int32_t db_id, const char *script_raw, const char *params_raw, bool immutable_query);
static ERL_NIF_TERM run_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  const char *script_raw = "?[] <- [['hello', 'world', 'Cozo!']]";
  // const char *params_raw;
  // bool immutable_query;
  
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
  
  char *ret = cozo_run_query(db_id, script_raw, "", true);
  ERL_NIF_TERM str = enif_make_string(env, ret, ERL_NIF_UTF8);
  cozo_free_str(ret);
  return enif_make_tuple2(env, atom_ok(env), str);
}


static ErlNifFunc nif_funcs[] =
  {
   {"open", 0, open_db},
   {"close", 1, close_db},
   {"run", 1, run_query}
  };

ERL_NIF_INIT(cozo,nif_funcs,NULL,NULL,NULL,NULL)
