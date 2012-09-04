-module(eunit_ct_merge_plugin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    eunit_ct_merge_plugin_sup:start_link().

stop(_State) ->
    ok.
