-module(gen_srv_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%init(_Args) ->
%    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
%    ChildSpecs = [#{id => gen_srv,
%                    start => {gen_srv, start_link, [[{drop_interval,3}]]},
%                    restart => permanent,
%                    shutdown => brutal_kill,
%                    type => worker,
%                    modules => [gen_srv]}],
%    {ok, {SupFlags, ChildSpecs}}.


init([]) ->
    Procs = [{gen_srv, {gen_srv, start_link, [[{drop_interval,30}]]},
        permanent, 5000, worker, [gen_srv]}],
{ok, {{one_for_one, 1, 5}, Procs}}.

%init([]) ->
%	Procs = [],
%	{ok, {{one_for_one, 1, 5}, Procs}}.
