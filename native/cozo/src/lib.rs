// use std::path::Path;
use cozo::DbInstance;
// use rustler::types::OwnedBinary;
use rustler::{Atom, Env, Error, NifStruct, ResourceArc, Term};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        eof
    }
}

#[rustler::nif]
fn open(engine: &str, path: &str, options: &str) -> Result<i64, Error> {
   let db = DbInstance::new(engine, path, options).unwrap();
   return Ok(1);
}

#[rustler::nif]
fn close(_db_id: i32) -> Result<Atom, Error> {
   return Ok(atoms::ok());
}

rustler::init!("cozorer_nif", [open, close]);
