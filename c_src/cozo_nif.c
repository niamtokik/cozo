/* Copyright (c) 2023 Mathieu Kerjouan
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
  // get the engine string length
  int engine_length;
  if (!enif_get_string_length(env, argv[0], &engine_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  // get the path string length
  int path_length;
  if (!enif_get_string_length(env, argv[1], &path_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  // extract the engine string
  char *engine = enif_alloc(engine_length);
  if (!(enif_get_string(env, argv[0], engine, engine_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  // extract the path string
  char *path = enif_alloc(path_length);
  if (!(enif_get_string(env, argv[1], path, path_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  // create a new db with engine, path without options.
  int db_id;  
  if (!cozo_open_db(engine, path, "", &db_id)) {
    return enif_make_tuple2(env, atom_ok(env), enif_make_int(env, db_id));
  }
  
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "open_error"));
}

static ERL_NIF_TERM close_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
  bool close_result = cozo_close_db(db_id);
  if (close_result) {
    return atom_ok(env);
  }
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "close_error"));
}

extern char *cozo_run_query(int32_t db_id, const char *script_raw, const char *params_raw, bool immutable_query);
static ERL_NIF_TERM run_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  // const char *params_raw;
  // bool immutable_query;
  
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  int script_raw_length;
  if (!enif_get_string_length(env, argv[1], &script_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  int params_raw_length;
  if (!enif_get_string_length(env, argv[2], &params_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  // immutability
  int immutable;
  if (!enif_get_int(env, argv[3], &immutable)) {
    return enif_make_badarg(env);
  }

  // cozo query
  char *script_raw = enif_alloc(script_raw_length);
  if (!(enif_get_string(env, argv[1], script_raw, script_raw_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  // extra parameters
  char *params_raw = enif_alloc(params_raw_length);
  if (!(enif_get_string(env, argv[2], params_raw, params_raw_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  // run the query and store the result in ret variable
  char *cozo_result = cozo_run_query(db_id, script_raw, params_raw, immutable ? true : false);

  // convert ret into a string
  ERL_NIF_TERM result_string = enif_make_string(env, cozo_result, ERL_NIF_UTF8);

  // free the memory mainte
  cozo_free_str(cozo_result);
  
  return enif_make_tuple2(env, atom_ok(env), result_string);
}




static ErlNifFunc nif_funcs[] =
  {
   {"open_db", 2, open_db},
   {"close_db", 1, close_db},
   {"run_query", 4, run_query}
   // {"import_relations", 2, import_relations},
   // {"export_relations", 2, export_relations},
   // {"backup", 2, backup_db},
   // {"restore", 2, restore_db},
   // {"import", 2, import_backup},
  };

ERL_NIF_INIT(cozo,nif_funcs,NULL,NULL,NULL,NULL)
