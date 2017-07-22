% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% Reads CouchDB's ini file and gets queried for configuration parameters.
% This module is initialized with a list of ini files that it consecutively
% reads Key/Value pairs from and saves them in an ets table. If more an one
% ini file is specified, the last one is used to write changes that are made
% with store/2 back to that ini file.

-module(config).
-behaviour(gen_server).
-vsn(1).

-export([start_link/1, stop/0, reload/0]).

-export([all/0]).
-export([get/1, get/2, get/3]).
-export([set/3, set/4, set/5]).
-export([delete/2, delete/3, delete/4]).

-export([get_integer/3, set_integer/3]).
-export([get_float/3, set_float/3]).
-export([get_boolean/3, set_boolean/3]).

-export([features/0, enable_feature/1, disable_feature/1]).

-export([listen_for_changes/2]).
-export([subscribe_for_changes/1]).
-export([parse_ini_file/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(FEATURES, "features").

-define(TIMEOUT, 30000).

-record(config, {
    notify_funs=[],
    ini_files=undefined,
    write_filename=undefined
}).


start_link(IniFiles) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, IniFiles, []).

stop() ->
    gen_server:cast(?MODULE, stop).


reload() ->
    gen_server:call(?MODULE, reload, ?TIMEOUT).

all() ->
    lists:sort(gen_server:call(?MODULE, all, infinity)).

get_integer(Section, Key, Default) when is_integer(Default) ->
    try
        to_integer(get(Section, Key, Default))
    catch
        error:badarg ->
            Default
    end.

set_integer(Section, Key, Value) when is_integer(Value) ->
    set(Section, Key, integer_to_list(Value));
set_integer(_, _, _) ->
    error(badarg).

to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Int) when is_integer(Int) ->
    Int;
to_integer(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

get_float(Section, Key, Default) when is_float(Default) ->
    try
        to_float(get(Section, Key, Default))
    catch
        error:badarg ->
            Default
    end.

set_float(Section, Key, Value) when is_float(Value) ->
    set(Section, Key, float_to_list(Value));
set_float(_, _, _) ->
    error(badarg).

to_float(List) when is_list(List) ->
    list_to_float(List);
to_float(Float) when is_float(Float) ->
    Float;
to_float(Int) when is_integer(Int) ->
    list_to_float(integer_to_list(Int) ++ ".0");
to_float(Bin) when is_binary(Bin) ->
    list_to_float(binary_to_list(Bin)).

get_boolean(Section, Key, Default) when is_boolean(Default) ->
    try
        to_boolean(get(Section, Key, Default))
    catch
        error:badarg ->
            Default
    end.

set_boolean(Section, Key, true) ->
    set(Section, Key, "true");
set_boolean(Section, Key, false) ->
    set(Section, Key, "false");
set_boolean(_, _, _) ->
    error(badarg).

to_boolean(List) when is_list(List) ->
    case list_to_existing_atom(List) of
        true  ->
            true;
        false ->
            false;
        _ ->
            error(badarg)
    end;
to_boolean(Bool) when is_boolean(Bool) ->
    Bool.

get(Section) when is_binary(Section) ->
    ?MODULE:get(binary_to_list(Section));
get(Section) when is_list(Section) ->
    Matches = ets:match(?MODULE, {{Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].

get(Section, Key) ->
    ?MODULE:get(Section, Key, undefined).

get(Section, Key, Default) when is_binary(Section) and is_binary(Key) ->
    ?MODULE:get(binary_to_list(Section), binary_to_list(Key), Default);
get(Section, Key, Default) when is_list(Section), is_list(Key) ->
    case ets:lookup(?MODULE, {Section, Key}) of
        [] when Default == undefined -> Default;
        [] when is_boolean(Default) -> Default;
        [] when is_float(Default) -> Default;
        [] when is_integer(Default) -> Default;
        [] when is_list(Default) -> Default;
        [] when is_atom(Default) -> Default;
        [] -> error(badarg);
        [{_, Match}] -> Match
    end.

set(Section, Key, Value) ->
    ?MODULE:set(Section, Key, Value, true, nil).

set(Section, Key, Value, Persist) when is_boolean(Persist) ->
    ?MODULE:set(Section, Key, Value, Persist, nil);
set(Section, Key, Value, Reason) ->
    ?MODULE:set(Section, Key, Value, true, Reason).

set(Sec, Key, Val, Persist, Reason) when is_binary(Sec) and is_binary(Key) ->
    ?MODULE:set(binary_to_list(Sec), binary_to_list(Key), Val, Persist, Reason);
set(Section, Key, Value, Persist, Reason)
        when is_list(Section), is_list(Key), is_list(Value) ->
    gen_server:call(?MODULE, {set, Section, Key, Value, Persist, Reason},
        ?TIMEOUT);
set(_Sec, _Key, _Val, _Persist, _Reason) ->
    error(badarg).


delete(Section, Key) when is_binary(Section) and is_binary(Key) ->
    delete(binary_to_list(Section), binary_to_list(Key));
delete(Section, Key) ->
    delete(Section, Key, true, nil).

delete(Section, Key, Persist) when is_boolean(Persist) ->
    delete(Section, Key, Persist, nil);
delete(Section, Key, Reason) ->
    delete(Section, Key, true, Reason).

delete(Sec, Key, Persist, Reason) when is_binary(Sec) and is_binary(Key) ->
    delete(binary_to_list(Sec), binary_to_list(Key), Persist, Reason);
delete(Section, Key, Persist, Reason) when is_list(Section), is_list(Key) ->
    gen_server:call(?MODULE, {delete, Section, Key, Persist, Reason},
        ?TIMEOUT).


features() ->
    lists:usort([list_to_atom(Key) || {Key, "true"} <- ?MODULE:get(?FEATURES)]).


enable_feature(Feature) when is_atom(Feature) ->
    ?MODULE:set(?FEATURES, atom_to_list(Feature), "true", false).


disable_feature(Feature) when is_atom(Feature) ->
    ?MODULE:delete(?FEATURES, atom_to_list(Feature), false).


listen_for_changes(CallbackModule, InitialState) ->
    config_listener_mon:subscribe(CallbackModule, InitialState).

subscribe_for_changes(Subscription) ->
    config_notifier:subscribe(Subscription).


init(IniFiles) ->
    ets:new(?MODULE, [named_table, set, protected, {read_concurrency, true}]),
    lists:map(fun(IniFile) ->
        {ok, ParsedIniValues} = parse_ini_file(IniFile),
        ets:insert(?MODULE, ParsedIniValues)
    end, IniFiles),
    WriteFile = case IniFiles of
        [_|_] -> lists:last(IniFiles);
        _ -> undefined
    end,
    debug_config(),
    {ok, #config{ini_files=IniFiles, write_filename=WriteFile}}.


terminate(_Reason, _State) ->
    ok.


handle_call(all, _From, Config) ->
    Resp = lists:sort((ets:tab2list(?MODULE))),
    {reply, Resp, Config};
handle_call({set, Sec, Key, Val, Persist, Reason}, _From, Config) ->
    true = ets:insert(?MODULE, {{Sec, Key}, Val}),
    couch_log:notice("~p: [~s] ~s set to ~s for reason ~p",
        [?MODULE, Sec, Key, Val, Reason]),
    case {Persist, Config#config.write_filename} of
        {true, undefined} ->
            ok;
        {true, FileName} ->
            config_writer:save_to_file({{Sec, Key}, Val}, FileName);
        _ ->
            ok
    end,
    Event = {config_change, Sec, Key, Val, Persist},
    gen_event:sync_notify(config_event, Event),
    {reply, ok, Config};
handle_call({delete, Sec, Key, Persist, Reason}, _From, Config) ->
    true = ets:delete(?MODULE, {Sec,Key}),
    couch_log:notice("~p: [~s] ~s deleted for reason ~p",
        [?MODULE, Sec, Key, Reason]),
    case {Persist, Config#config.write_filename} of
        {true, undefined} ->
            ok;
        {true, FileName} ->
            config_writer:save_to_file({{Sec, Key}, ""}, FileName);
        _ ->
            ok
    end,
    Event = {config_change, Sec, Key, deleted, Persist},
    gen_event:sync_notify(config_event, Event),
    {reply, ok, Config};
handle_call(reload, _From, Config) ->
    DiskKVs = lists:foldl(fun(IniFile, DiskKVs0) ->
        {ok, ParsedIniValues} = parse_ini_file(IniFile),
        lists:foldl(fun({K, V}, DiskKVs1) ->
            dict:store(K, V, DiskKVs1)
        end, DiskKVs0, ParsedIniValues)
    end, dict:new(), Config#config.ini_files),
    % Update ets with anything we just read
    % from disk
    dict:fold(fun(K, V, _) ->
        ets:insert(?MODULE, {K, V})
    end, nil, DiskKVs),
    % And remove anything in ets that wasn't
    % on disk.
    ets:foldl(fun({K, _}, _) ->
        case dict:is_key(K, DiskKVs) of
            true ->
                ok;
            false ->
                ets:delete(?MODULE, K)
        end
    end, nil, ?MODULE),
    {reply, ok, Config}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    couch_log:error("config:handle_info Info: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


parse_ini_file(IniFile) ->
    IniFilename = config_util:abs_pathname(IniFile),
    IniBin =
    case file:read_file(IniFilename) of
        {ok, IniBin0} ->
            IniBin0;
        {error, enoent} ->
            Fmt = "Couldn't find server configuration file ~s.",
            Msg = list_to_binary(io_lib:format(Fmt, [IniFilename])),
            couch_log:error("~s~n", [Msg]),
            throw({startup_error, Msg})
    end,

    Lines = re:split(IniBin, "\r\n|\n|\r|\032", [{return, list}]),
    {_, ParsedIniValues} =
    lists:foldl(fun(Line, {AccSectionName, AccValues}) ->
            case string:strip(Line) of
            "[" ++ Rest ->
                case re:split(Rest, "\\]", [{return, list}]) of
                [NewSectionName, ""] ->
                    {NewSectionName, AccValues};
                _Else -> % end bracket not at end, ignore this line
                    {AccSectionName, AccValues}
                end;
            ";" ++ _Comment ->
                {AccSectionName, AccValues};
            Line2 ->
                case re:split(Line2, "\s?=\s?", [{return, list}]) of
                [Value] ->
                    MultiLineValuePart = case re:run(Line, "^ \\S", []) of
                    {match, _} ->
                        true;
                    _ ->
                        false
                    end,
                    case {MultiLineValuePart, AccValues} of
                    {true, [{{_, ValueName}, PrevValue} | AccValuesRest]} ->
                        % remove comment
                        case re:split(Value, " ;|\t;", [{return, list}]) of
                        [[]] ->
                            % empty line
                            {AccSectionName, AccValues};
                        [LineValue | _Rest] ->
                            E = {{AccSectionName, ValueName},
                                PrevValue ++ " " ++ LineValue},
                            {AccSectionName, [E | AccValuesRest]}
                        end;
                    _ ->
                        {AccSectionName, AccValues}
                    end;
                [""|_LineValues] -> % line begins with "=", ignore
                    {AccSectionName, AccValues};
                [ValueName|LineValues] -> % yeehaw, got a line!
                    RemainingLine = config_util:implode(LineValues, "="),
                    % removes comments
                    case re:split(RemainingLine, " ;|\t;", [{return, list}]) of
                    [[]] ->
                        % empty line means delete this key
                        ets:delete(?MODULE, {AccSectionName, ValueName}),
                        {AccSectionName, AccValues};
                    [LineValue | _Rest] ->
                        {AccSectionName,
                            [{{AccSectionName, ValueName}, LineValue} | AccValues]}
                    end
                end
            end
        end, {"", []}, Lines),
    {ok, ParsedIniValues}.


debug_config() ->
    case ?MODULE:get("log", "level") of
        "debug" ->
            io:format("Configuration Settings:~n", []),
            lists:foreach(fun({{Mod, Key}, Val}) ->
                io:format("  [~s] ~s=~p~n", [Mod, Key, Val])
            end, lists:sort(ets:tab2list(?MODULE)));
        _ ->
            ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_test() ->
    ?assertEqual(1, to_integer(1)),
    ?assertEqual(1, to_integer(<<"1">>)),
    ?assertEqual(1, to_integer("1")),
    ?assertEqual(-1, to_integer("-01")),
    ?assertEqual(0, to_integer("-0")),
    ?assertEqual(0, to_integer("+0")),
    ok.

to_float_test() ->
    ?assertEqual(1.0, to_float(1)),
    ?assertEqual(1.0, to_float(<<"1.0">>)),
    ?assertEqual(1.0, to_float("1.0")),
    ?assertEqual(-1.1, to_float("-01.1")),
    ?assertEqual(0.0, to_float("-0.0")),
    ?assertEqual(0.0, to_float("+0.0")),
    ok.

-endif.
