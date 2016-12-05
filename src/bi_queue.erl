-module(bi_queue).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	State = queue:new(),
    {ok, State}.

handle_call(history, _From, State) ->
	MsgList = queue:to_list(State),
    {reply, MsgList, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({join, Msg}, State) ->
	NewState = join(State, Msg),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

join(State, Msg) ->
	Len = queue:len(State),
	State1 = 
		case Len >= 10 of
			true -> 
				{_, StateTmp} = queue:out(State),
				StateTmp;
			false -> 
				State
		end,
	queue:in(Msg, State1).