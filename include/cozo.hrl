% define cozo types and records.
-type db_id()         :: pos_integer().
-type db_engine()     :: mem | sqlite | rocksdb.
-type db_path()       :: string().
-type db_options()    :: map().
-type db_parent()     :: pid().
-record(cozo, { id = undefined        :: undefined | db_id()
              , db_engine = mem       :: db_engine()
              , db_path = ""          :: db_path()
              , db_options = #{}      :: db_options()
              , db_parent = undefined :: undefined | db_parent()
              }).
