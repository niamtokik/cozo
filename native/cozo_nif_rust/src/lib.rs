use serde_rustler::to_term;
use rustler::NifResult;
use rustler::Env;
use rustler::Error;
use rustler::NifMap;
use rustler::NifTuple;
use rustler::Term;
use rustler::ResourceArc;
use cozo::*;

mod atoms {
    rustler::atoms! {
        ok,
        undefined,
        error
    }
}

/* 
 *
 */
struct DbResource {
    pub db: DbInstance
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(DbResource, env);
    true
}

/* open database with strings as arguments. Strings
 * are represented as binary on the Erlang side, not
 * classical Erlang list of small integers.
 */
#[rustler::nif(schedule = "DirtyCpu")]
fn open_db(engine: &str, path: &str, options: &str) -> Result<ResourceArc<DbResource>, Error> {
    let db = DbInstance::new(engine, path, options).unwrap();
    let resource = ResourceArc::new(DbResource{ db: db });
    Ok(resource)
}

/* a work in progress function. I think more than one function
 * to run query should exist:
 *   1. run query based on cozoscript
 *   2. run query based on cozo bytecode
 *   3. run query on json? (don't even sure if it's supported)
 */
#[rustler::nif]
fn run_query(_db: i64, _query: &str) -> i64 {
    return 1;
}

/* work in progress. cozoscript query is compiled and bytecode is
 * returned as binary.
 *   - execute_single function
 */
// #[rusler::nif]
// fn compile_query(resource: ResourceArc<DbResource>) -> InputProgram {
// }

/* probably not the best way to close the database, but I did not find
 * any other solutions right now. Are we also closing the database 
 * correctly when we are dropping the resource? Not sure. Keeping
 * track of the opened database somewhere could perhaps to debug
 * this part of the code. Question though, how to be sure a database
 * was closed on cozo side?
 */
#[rustler::nif]
fn close_db(env: Env, resource: ResourceArc<DbResource>) -> Result<Term, Error> {
    drop(resource);
    Ok(atoms::ok().to_term(env))
}

/* other interfaces needed present in DbInstance
 *   - export_relations
 *   - import_relations
 *   - backup_db
 *   - restore_backup
 *   - import_from_backup
 *   - register_fixed_rule
 *   - unregister_fixed_rule
 *   - register_callback
 *   - unregister_callback
 *   - compact_relation
 *   - explain_compiled
 *   - run_sys_op
 *   - list_running
 *   - list_indices
 *   - list_columns
 *   - list_relations
 *   - evaluate_expressions
 */

rustler::init!("cozo_nif_rust",
    [ open_db
    , run_query
    , close_db
    ],
    load = load
);
