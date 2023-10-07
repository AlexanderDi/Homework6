-module(homework6_server).
-behaviour(gen_server).

-export([insert/3]).
-export([lookup/1]).

-export([start_link/0]).
-export([init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    code_change/3,
    terminate/2
]).


insert(Key, Value, TTL) when is_integer(TTL), TTL > 0 ->
    ok = gen_server:cast(?MODULE, {insert, Key, Value, TTL}).


lookup(Key) ->
    _ = gen_server:call(?MODULE, {lookup, Key}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-record(state, {}).


init([]) ->
    _ = ets:new(?MODULE, [named_table, set, private, {keypos, 1}]),
    _ = timer:send_after(2000, self(), cleanup),
    {ok, #state{}}.


handle_call({lookup, Key}, _From, State = #state{}) ->
    Now = os:system_time(second),
    case ets:lookup(?MODULE, Key) of
        [{_, Value, TTL}] when TTL > Now ->
            {reply, Value, State};
        _any ->
            {reply, undef, State}
    end;

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.


handle_cast({insert, Key, Value, TTL}, State = #state{}) ->
    true = ets:insert(?MODULE, {Key, Value, TTL + os:system_time(second)}),
    {noreply, State};

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.


handle_info(cleanup, State = #state{}) ->
    _ = timer:send_after(2000, self(), cleanup),
    Now = os:system_time(second),
    done = cleanup(Now, _Key = ets:first(?MODULE)),
    {noreply, State};

handle_info(_Info, State = #state{}) ->
    {noreply, State}.


code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.


terminate(_Reason, _State = #state{}) ->
    ok.

cleanup(_Now, '$end_of_table') ->
    done;
cleanup(Now, Key) ->
    NextKey = ets:next(?MODULE, Key),
    case ets:lookup(?MODULE, Key) of
        [{_, _, TTL}] when TTL < Now ->
            true = ets:delete(?MODULE, Key);
        _any ->
            ok
    end,
    cleanup(Now, NextKey).
