-module(mod_rank).

-behaviour(gen_server).

-export([start_link/3]).

-export([insert/4, sync_insert/4, reset/1, wait/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("common.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
          process_name,
          table,
          max_size
         }).

insert(Process, Id, Value, Ext) ->
    gen_server:cast(Process, {insert, Id, Value, Ext}).

sync_insert(Process, Id, Value, Ext) ->
    gen_server:call(Process, {insert, Id, Value, Ext}).

reset(Process) ->
    gen_server:cast(Process, reset).

start_link(Process, Table, MaxSize) ->
    gen_server:start_link({local, Process}, ?MODULE, [Process, Table, MaxSize], []).

wait(Process) ->
    gen_server:call(Process, wait).

init([Process, Table, MaxSize]) ->
    {ok, #state{
            table = Table,
            max_size = MaxSize,
            process_name = Process
           }}.

handle_call({insert, Id, Value, Ext}, _From,
            #state{
               table = Table,
               max_size = MaxSize
              } = State) ->
    lib_rank:insert(Table, MaxSize, Id, Value, Ext),
    {reply, ok, State, 0};
handle_call(wait, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    ?DEBUG("unknow request: ~p~n", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({insert, Id, Value, Ext}, 
            #state{
               table = Table,
               max_size = MaxSize
              } = State) ->
    lib_rank:insert(Table, MaxSize, Id, Value, Ext),
    {noreply, State, 0};
handle_cast(reset, #state{
                      table = Table
                     } = State) ->
    ?DEBUG("~p reset~n", [Table]),
    {atomic, ok} = mnesia:clear_table(Table),
    {noreply, State};
handle_cast(_Msg, State) ->
    ?DEBUG("unknow cast: ~p~n", [_Msg]),
    {noreply, State}.

handle_info(timeout, #state{
                        table = Table,
                        max_size = MaxSize
                       } = State) ->
    lib_rank:clean_over_flow(Table, MaxSize),
    {noreply, State};
handle_info(_Info, State) ->
    ?DEBUG("unknow info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

