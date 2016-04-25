-define(DEBUG(Format, Args), ok).


%% -define(DEBUG(Format, Args), io:format("(~w:~p:~p:~p) : " ++ Format ++ "~n", 
%%                                        [erlang:localtime(), self(), ?MODULE, ?LINE] ++ Args)).
