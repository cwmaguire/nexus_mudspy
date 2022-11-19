%%%-------------------------------------------------------------------
%% @doc webserver top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mudspy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    AchaeaServerSpec =
        #{id => achaea_server,
          start => {achaea_server, start_link, []},
          modules => [achaea_server]},
    ChildSpecs = [AchaeaServerSpec],
    {ok, {SupFlags, ChildSpecs}}.
