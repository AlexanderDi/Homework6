-module(homework6_sup).
-behaviour(supervisor).


-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
	{ok, {{one_for_one, 100, 1}, _Child = [
		#{
			id       => shomework6_server,
			start    => {homework6_server, start_link, []},
			restart  => permanent,
			shutdown => 2000,
			type     => worker
		}
	]}}.
