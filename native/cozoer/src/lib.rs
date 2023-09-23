use serde_rustler::to_term;
use rustler::NifResult;
use rustler::Env;
use rustler::Error;
use rustler::NifMap;
use rustler::NifTuple;
use rustler::NifUnitEnum;
use rustler::NifTaggedEnum;
use rustler::NifUntaggedEnum;
use rustler::ResourceArc;
use rustler::Term;
use cozo::*;

mod atoms {
    rustler::atoms! {
        ok,
        undefined,
        error,
        mem,
        sqlite,
        rocksdb,
        sled,
        tikv,
        result_set
    }
}

struct DbResource {
    pub db: DbInstance
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(DbResource, env);
    true
}

rustler::init!("cozoer",
    [
    ],
    load = load
);

