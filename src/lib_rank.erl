%% 排行榜引擎是从小到大排序
%% 清理要从first开始，然后next
%% top要从last开始取，然后prev
%% rank值计算，获取当前的key之后，next到last，计算有多少个位置
-module(lib_rank).

-export([insert/5, top/2, last/2, last_rank/1, rank/2, down/3, up/3, all/1]).

-export([clean_over_flow/2]).

-export([delete/2]).

-include("erank.hrl").
-include("common.hrl").

insert(Table, MaxSize, Id, Value, Ext) ->      
    case check_insert(Table, MaxSize, Id, Value) of
        false ->
            ignore;
        true ->
            mnesia:dirty_write(Table, #rank{
                                         id = Id,
                                         value = Value,
                                         ext = Ext
                                        })
    end.

check_insert(Table, MaxSize, Id, Value) ->
    Size = mnesia:table_info(Table, size),
    LowValue = mnesia:dirty_first(Table),
    case mnesia:dirty_index_read(Table, Id, #rank.id) of
        [] ->
            %% 找不到对应的OldValue，那么跟Size和当前最小的值做对比
            Size =< MaxSize orelse
                Value > LowValue;
        [#rank{
            value = OldValue
           }] ->
            if
                OldValue =< Value ->
                    %% 新分数比榜单上面还要好
                    mnesia:dirty_delete(Table, OldValue),
                    true;
                true ->
                    %% 分数比较差，不做更新
                    false
            end
    end.

clean_over_flow(Table, MaxSize) ->
    ?DEBUG("clean_over_flow", []),
    Size = mnesia:table_info(Table, size),
    Num = Size - MaxSize,
    if
        Num > 0 ->              
            ?DEBUG("rank ~p run clean ~p ~n", [Table, Num]),
            clean_over_flow2(Table, mnesia:dirty_first(Table), Num),
            ?DEBUG("run end ~n", []);
        true ->
            ignore
    end.

clean_over_flow2(_, '$end_of_table', _) ->
    ok;
clean_over_flow2(Table, Key, Num) 
  when Num > 0 ->
    mnesia:dirty_delete(Table, Key),
    clean_over_flow2(Table, mnesia:dirty_next(Table, Key), Num-1);
clean_over_flow2(_, _, _) ->
    ok.

top(Table, Len) ->
    prev2(Table, mnesia:dirty_last(Table), Len).

last(Table, Len) ->    
    next2(Table, mnesia:dirty_first(Table), Len).

last_rank(Table) ->
    mnesia:table_info(Table, size).

rank(Table, Id) ->
    case mnesia:dirty_index_read(Table, Id, #rank.id) of
        [] ->
            -1;            
        [#rank{
            value = Value
           }] ->
            rank2(Table, mnesia:dirty_next(Table, Value), 0) + 1
    end.

%%算出前面还有几个人
rank2(_, '$end_of_table', Num) ->
    Num;
rank2(Table, Key, Num) ->
    rank2(Table, mnesia:dirty_next(Table, Key), Num + 1).            


down(Table, Id, N) when N > 0 ->
    case mnesia:dirty_index_read(Table, Id, #rank.id) of
        [] ->
            no_rank;            
        [#rank{
            value = Value
           }] ->
           {ok, prev2(Table, mnesia:dirty_prev(Table, Value), N)}
    end.
    
prev2(_, '$end_of_table', _) ->
    [];
prev2(_, _, 0) ->
    [];
prev2(Table, Key, N) ->
    [read(Table, Key) | prev2(Table, mnesia:dirty_prev(Table, Key), N-1)].

up(Table, Id, N) when N > 0 ->
    case mnesia:dirty_index_read(Table, Id, #rank.id) of
        [] ->
            no_rank;            
        [#rank{
            value = Value
           }] ->
           {ok, lists:reverse(next2(Table, mnesia:dirty_next(Table, Value), N))}
    end.
    
next2(_, '$end_of_table', _) ->
    [];
next2(_, _, 0) ->
    [];
next2(Table, Key, N) ->
    [read(Table, Key) | next2(Table, mnesia:dirty_next(Table, Key), N-1)].

all(Table) ->    
    all2(Table, mnesia:dirty_last(Table)).

all2(_, '$end_of_table') ->
    [];
all2(Table, Key) ->
    [read(Table, Key) | all2(Table, mnesia:dirty_prev(Table, Key))].

delete(Table, Id) ->
    case mnesia:dirty_index_read(Table, Id, #rank.id) of
        [] ->
            ok;
        [#rank{
            value = Value
           }] ->
            mnesia:dirty_delete(Table, Value)
    end.
            
read(Table, Key) ->
    hd(mnesia:dirty_read(Table, Key)).
