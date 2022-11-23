-module(achaea_server).

-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([register/1]).
-export([unregister/0]).
-export([unregister/1]).
-export([registered/1]).
-export([command/1]).
-export([update/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {registered_pids = #{} :: #{binary() => [pid()]}}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(MudDataType) ->
    gen_server:cast(achaea_server, {register, self(), MudDataType}).

unregister() ->
    gen_server:cast(achaea_server, {unregister, self()}).

unregister(MudDataType) ->
    gen_server:cast(achaea_server, {unregister, self(), MudDataType}).

registered(MudDataType) ->
    gen_server:call(achaea_server, {registered, MudDataType}).

update(Update) ->
    gen_server:cast(achaea_server, {update, Update}).

command(Command) ->
    gen_server:cast(achaea_server, {command, self(), Command}).

%% gen_server.

init([]) ->
    io:format("Achaea server running~n", []),
    RegisteredPids = #{raw => [],
                       gmcp => [],
                       fgcolor => [],
                       element => [],
                       text => [],
                       version => [],
                       support => []},
	{ok, #state{registered_pids = RegisteredPids}}.

handle_call({registered, MudDataType}, _From, State = #state{registered_pids = RegisteredPids}) ->
    case RegisteredPids of
        #{MudDataType := Pids} ->
            {reply, Pids, State};
        _ ->
            {reply, none, State}
    end;
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({register, Pid, TypeBin},
            State = #state{registered_pids = RegisteredPids0}) ->

    Type = type_atom(TypeBin),

    case RegisteredPids0 of
        #{Type := Pids} ->
            case lists:member(Pid, Pids) of
                false ->
                    RegisteredPids = RegisteredPids0#{Type => [Pid | Pids]},
                    {noreply, State#state{registered_pids = RegisteredPids}};
                true ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State#state{registered_pids = #{Type => [Pid]}}}
    end;
handle_cast({unregister, Pid, MudDataType},
            State = #state{registered_pids = RegisteredPids0}) ->
    case RegisteredPids0 of
        #{MudDataType := Pids} ->
            {noreply, State#state{registered_pids = Pids -- [Pid]}};
        _ ->
            {noreply, State}
    end;
handle_cast({unregister, Pid},
            State = #state{registered_pids = RegisteredPids0}) ->
    RegisteredPids =
        maps:map(fun(_, Pids) ->
                     Pids -- [Pid]
                 end,
                 RegisteredPids0),
    {noreply,
     State#state{registered_pids = RegisteredPids}};
handle_cast({update, Binary}, State = #state{registered_pids = RegisteredPids}) ->
    ParsedElements = mudspy_parse:parse(Binary),
    [send_update(Elem, RegisteredPids)|| Elem <- ParsedElements],
    {noreply, State};
handle_cast(_Ignored, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% TODO turn the Bin into JSON so that the web pages can parse it
send_update({Type, Bin}, RegisteredPids) ->
    %Sample = case Update of
    %             <<First:20/binary, _/binary>> ->
    %                 <<First/binary, " ...">>;
    %             Short ->
    %                 Short
    %         end,
    %io:format("Sending binary to pid ~p: (sample) ~p~n", [Pid, Sample]),
    #{Type := Pids, raw := RawPids} = RegisteredPids,
    [Pid ! {send, Bin} || Pid <- Pids ++ RawPids].

type_atom(<<"raw">>) ->
    raw;
type_atom(<<"gmcp">>) ->
    gmcp;
type_atom(<<"fgcolor">>) ->
    fgcolor;
type_atom(<<"text">>) ->
    text;
type_atom(<<"version">>) ->
    version;
type_atom(<<"support">>) ->
    support.
