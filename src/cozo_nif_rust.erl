%%%===================================================================
%%% @doc DRAFT. DO NOT USE.
%%%
%%% == Commands ==
%%%
%%% ```
%%% % command
%%% {
%%%   :create store {a, b, c}
%%% }
%%% '''
%%%
%%% ```
%%% cozo:create_relation(Db, store, [{a, integer}, {b, integer}, {c, integer}]).
%%% '''
%%%
%%% == Queries ==
%%%
%%% ```
%%% % queries
%%% {
%%%   ?[a,b,c] <- [[1,2,3]]
%%%   :put store {a, b, c}
%%% }
%%% '''
%%%
%%% ```
%%% cozo_draft:put(Db, {store, 1, 2, 3}).
%%% cozo_draft:put(Db, #store{ a=1, b=2, c=3 }).
%%% cozo_draft:put(Db, [#store{ a=1, b=2, c=3 }]).
%%% '''
%%%
%%% ```
%%% cozo_draft:q([ #store{a = A, b = B, c = C} || [A,B,C] <- [[]] )
%%% '''
%%%
%%% == DRAFT ==
%%%
%%% ```
%%% cozo_draft:query(Db, store, [a,b,c]).
%%%
%%% % ?[a] := *store[a,b,c], b==2
%%% cozo_draft:query(Db, store, [{{'$1', '$2', '_'}, [{'=:=', '$1', 2}], ['$1']}]).
%%%
%%% % ?[a] := *store[a,b,c], a<b
%%% cozo:select(Db, store, [{'$1', '$2', '$3'}, [{'<', '$1', '$2'}], ['$1']]).
%%% '''
%%%
%%%
%%% @end
%%%===================================================================
-module(cozo_nif_rust).
-compile(export_all).




