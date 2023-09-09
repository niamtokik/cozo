/*
 * Copyright 2023 Mathieu Kerjouan
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include <ei.h>
#include <erl_nif.h>
#include "cozo_c.h"

/*
 * cozolib_c external functions
 */
extern void cozo_free_str(char *s);
extern char *cozo_open_db(const char *engine, const char *path, const char *options, int32_t *db_id);
extern bool cozo_close_db(int32_t id);
extern char *cozo_run_query(int32_t db_id, const char *script_raw, const char *params_raw, bool immutable_query);
extern char *cozo_import_relations(int32_t db_id, const char *json_payload);
extern char *cozo_export_relations(int32_t db_id, const char *json_payload);
extern char *cozo_backup(int32_t db_id, const char *out_path);
extern char *cozo_restore(int32_t db_id, const char *in_path);
extern char *cozo_import_from_backup(int32_t db_id, const char *json_payload);

/*
 * cozo_nif prototype
 */
static ERL_NIF_TERM atom_ok(ErlNifEnv *);
static ERL_NIF_TERM atom_error(ErlNifEnv *);
static ERL_NIF_TERM open_db(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM close_db(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM run_query(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM import_relations_db(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM export_relations_db(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM backup_db(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM restore_db(ErlNifEnv *, int, const ERL_NIF_TERM *);
static ERL_NIF_TERM import_backup_db(ErlNifEnv *, int, const ERL_NIF_TERM *);

/*
 * simple 'ok' atom helper.
 */
ERL_NIF_TERM atom_ok(ErlNifEnv *env) {
  const char* atom = "ok";
  return enif_make_atom(env, atom);
}

/*
 * simple 'error' atom helper
 */
ERL_NIF_TERM atom_error(ErlNifEnv *env) {
  const char* atom = "error";
  return enif_make_atom(env, atom);
}

/*
 * Open a new database based on engine, path, option and mutability.
 */
static ERL_NIF_TERM open_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  // get the engine string length
  unsigned int engine_length;
  if (!enif_get_string_length(env, argv[0], &engine_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  // get the path string length
  unsigned int path_length;
  if (!enif_get_string_length(env, argv[1], &path_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  // get option length
  unsigned int options_length;
  if (!(enif_get_string_length(env, argv[2], &options_length, ERL_NIF_UTF8))) {
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

  // extract the option string
  char *options = enif_alloc(options_length);
  if (!(enif_get_string(env, argv[2], options, options_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  // create a new db with engine, path without options.
  int db_id;
  if (!cozo_open_db(engine, path, options, &db_id)) {
    return enif_make_tuple2(env, atom_ok(env), enif_make_int(env, db_id));
  }

  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "open_error"));
}

/*
 * Close an already opened database based by its ID.
 */
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

/*
 * cozo_run_query interface
 */
static ERL_NIF_TERM run_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  unsigned int script_raw_length;
  if (!enif_get_string_length(env, argv[1], &script_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  unsigned int params_raw_length;
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

/*
 * cozo_import_relations interface
 */
static ERL_NIF_TERM import_relations_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  unsigned int json_length;
  if (!enif_get_string_length(env, argv[1], &json_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  char *json = enif_alloc(json_length);
  if (!(enif_get_string(env, argv[1], json, json_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *relations_result = cozo_import_relations(db_id, json);
  ERL_NIF_TERM result_string = enif_make_string(env, relations_result, ERL_NIF_UTF8);
  cozo_free_str(relations_result);
  return enif_make_tuple2(env, atom_ok(env), result_string);
}

/*
 * cozo_export_relations interface
 */
static ERL_NIF_TERM export_relations_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  unsigned int path_length;
  if (!enif_get_string_length(env, argv[1], &path_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  char *path = enif_alloc(path_length);
  if (!(enif_get_string(env, argv[1], path, path_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *relations_result = cozo_export_relations(db_id, path);
  ERL_NIF_TERM result_string = enif_make_string(env, relations_result, ERL_NIF_UTF8);
  cozo_free_str(relations_result);
  return enif_make_tuple2(env, atom_ok(env), result_string);
}

/*
 * cozo_backup interface
 */
static ERL_NIF_TERM backup_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  unsigned int path_length;
  if (!enif_get_string_length(env, argv[1], &path_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  char *path = enif_alloc(path_length);
  if (!(enif_get_string(env, argv[1], path, path_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *relations_result = cozo_backup(db_id, path);
  ERL_NIF_TERM result_string = enif_make_string(env, relations_result, ERL_NIF_UTF8);
  cozo_free_str(relations_result);
  return enif_make_tuple2(env, atom_ok(env), result_string);
}

/*
 * cozo_restore interface
 */
static ERL_NIF_TERM restore_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  unsigned int path_length;
  if (!enif_get_string_length(env, argv[1], &path_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  char *path = enif_alloc(path_length);
  if (!(enif_get_string(env, argv[1], path, path_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *relations_result = cozo_restore(db_id, path);
  ERL_NIF_TERM result_string = enif_make_string(env, relations_result, ERL_NIF_UTF8);
  cozo_free_str(relations_result);
  return enif_make_tuple2(env, atom_ok(env), result_string);
}

/*
 * cozo_import_from_backup interface
 */
static ERL_NIF_TERM import_backup_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }

  unsigned int json_length;
  if (!enif_get_string_length(env, argv[1], &json_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  char *json = enif_alloc(json_length);
  if (!(enif_get_string(env, argv[1], json, json_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *relations_result = cozo_import_from_backup(db_id, json);
  ERL_NIF_TERM result_string = enif_make_string(env, relations_result, ERL_NIF_UTF8);
  cozo_free_str(relations_result);
  return enif_make_tuple2(env, atom_ok(env), result_string);
}

/*
 * exporter NIFs
 */
static ErlNifFunc nif_funcs[] =
  {
   {"open_db_nif", 3, open_db},
   {"close_db_nif", 1, close_db},
   {"run_query_nif", 4, run_query},
   {"import_relations_db_nif", 2, import_relations_db},
   {"export_relations_db_nif", 2, export_relations_db},
   {"backup_db_nif", 2, backup_db},
   {"restore_db_nif", 2, restore_db},
   {"import_backup_db_nif", 2, import_backup_db}
  };
ERL_NIF_INIT(cozo_nif,nif_funcs,NULL,NULL,NULL,NULL)
