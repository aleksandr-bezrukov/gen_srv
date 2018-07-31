-module(gen_srv).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, cleaner/0]).
-export([insert/3, lookup/1, lookup_by_date/2]).

-define(CACHE,?MODULE).

start_link([{drop_interval, Seconds}]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{drop_interval, Seconds}], [])
    ,{ok,self()}
    .

lookup(Key) -> 
    gen_server:call(?MODULE,{lookup,Key})
    .

create_cache() ->
    ets:new(?CACHE,[public,named_table])
    .

insert(Key,Value,Seconds) -> 
    ExpirationTimeGregorian = calendar:datetime_to_gregorian_seconds({date(), time()}) + Seconds
    ,gen_server:call(?MODULE,{insert,Key,Value,ExpirationTimeGregorian})
    ,ok
    .

%DateFrom = {{2015,1,1},{00,00,00}}.
%DateTo = {{2015,1,10},{23,59,59}}.

lookup_by_date(DateFrom,DateTo) ->
    DateFromGregorian = calendar:datetime_to_gregorian_seconds(DateFrom)
    ,DateToGregorian = calendar:datetime_to_gregorian_seconds(DateTo)
    ,gen_server:call(?MODULE,{lookup_by_date,DateFromGregorian,DateToGregorian})
    .

cleaner() ->
    Now = calendar:datetime_to_gregorian_seconds({date(), time()})
    %,io:format("Now=~w ~n",[Now])
    ,delete_expired(ets:select(?CACHE,[{{'$1','$2','$3'},[{'<','$3',Now}],[['$1']]}]))
    .

delete_expired(ExpiredList) ->
    case ExpiredList of
	[] -> ok;
	[[H]|T] -> ets:delete(?CACHE,H)
	,delete_expired(T)
    end
    .

init([{drop_interval,MilliSeconds}]) ->
    {ok,_TRef} = timer:apply_interval(MilliSeconds*1000, ?MODULE, cleaner, [])
    ,{ok, create_cache()}
    .

handle_call({lookup_by_date,DateFromGregorian,DateToGregorian},_From,State) ->
    Reply = ets:select(?CACHE,[{{'$1','$2','$3'},[{'>','$3',DateFromGregorian},{'<','$3',DateToGregorian}],[['$1']]}])
    ,{reply,Reply,State}
    ;

handle_call({insert,Key,Value,ExpirationTimeGregorian},_From,State) ->
    Reply = ets:insert(?CACHE,{Key,Value,ExpirationTimeGregorian})
    ,{reply,Reply,State}
    ;

handle_call({lookup,Key},_From,State) ->
    Reply = ets:lookup(?CACHE,Key)
    ,{reply,Reply,State}
    .

handle_cast({mock, _Ch}, State) ->
    {noreply, State}
    .