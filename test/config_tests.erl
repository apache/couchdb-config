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

-module(config_tests).

-beahiour(config_listener).

-export([handle_config_change/5, handle_config_terminate/3]).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(SHORT_TIMEOUT, 100).
-define(TIMEOUT, 1000).

-define(CONFIG_FIXTURESDIR,
        filename:join([?BUILDDIR(), "src", "config", "test", "fixtures"])).
-define(CONFIG_FIXTURE_1,
        filename:join([?CONFIG_FIXTURESDIR, "config_tests_1.ini"])).
-define(CONFIG_FIXTURE_2,
        filename:join([?CONFIG_FIXTURESDIR, "config_tests_2.ini"])).
-define(CONFIG_FIXTURE_TEMP,
    begin
        FileName = filename:join([?TEMPDIR, "config_temp.ini"]),
        {ok, Fd} = file:open(FileName, write),
        ok = file:truncate(Fd),
        ok = file:close(Fd),
        FileName
    end).

-define(DEPS, [couch_stats, couch_log, folsom, lager,
               goldrush, syntax_tools, compiler, config]).


setup() ->
    setup(?CONFIG_CHAIN).
setup({temporary, Chain}) ->
    setup(Chain);
setup({persistent, Chain}) ->
    setup(lists:append(Chain, [?CONFIG_FIXTURE_TEMP]));
setup(Chain) ->
    ok = application:set_env(config, ini_files, Chain),
    test_util:start_applications(?DEPS).

setup_empty() ->
    setup([]).

setup_config_listener() ->
    setup(),
    {ok, Pid} = spawn_listener(),
    config:listen_for_changes(?MODULE, {Pid, self(), []}),
    {Pid, self()}.

teardown({Pid, _}) ->
    stop_listener(Pid),
    [application:stop(App) || App <- ?DEPS];
teardown(_) ->
    [application:stop(App) || App <- ?DEPS].

teardown(_, _) ->
    [application:stop(App) || App <- ?DEPS].

handle_config_change("remove_handler", _Key, _Value, _Persist, _State) ->
    remove_handler;
handle_config_change("update_state", Key, _Value, _Persist, {Listener, Subscriber, Items}) ->
    NewState = {Listener, Subscriber, [Key|Items]},
    ok = reply(NewState, NewState),
    {ok, NewState};
handle_config_change(Section, Key, Value, Persist, State) ->
    ok = reply({{Section, Key, Value, Persist}, State}, State),
    {ok, State}.
handle_config_terminate(Self, Reason, State) ->
    ok = reply({stop, Self, Reason, State}, State),
    ok.

reply(Reply, {Listener, _, _}) ->
    call_sync(Listener, {set, Reply}).

wait_reply(Listener) ->
    call_sync(Listener, get).

config_test_() ->
    {
        "CouchDB config tests",
        [
            config_get_tests(),
            config_set_tests(),
            config_del_tests(),
            config_override_tests(),
            config_persistent_changes_tests(),
            config_no_files_tests(),
            config_listener_behaviour_tests()
        ]
    }.

config_get_tests() ->
    {
        "Config get tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                should_load_all_configs(),
                should_locate_daemons_section(),
                should_locate_mrview_handler(),
                should_return_undefined_atom_on_missed_section(),
                should_return_undefined_atom_on_missed_option(),
                should_return_custom_default_value_on_missed_option(),
                should_only_return_default_on_missed_option(),
                should_fail_to_get_binary_value(),
                should_return_any_supported_default()
            ]
        }
    }.

config_set_tests() ->
    {
        "Config set tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                should_update_option(),
                should_create_new_section(),
                should_fail_to_set_binary_value()
            ]
        }
    }.

config_del_tests() ->
    {
        "Config deletion tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                should_return_undefined_atom_after_option_deletion(),
                should_be_ok_on_deleting_unknown_options()
            ]
        }
    }.

config_override_tests() ->
    {
        "Configs overide tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [
                {{temporary, [?CONFIG_DEFAULT]},
                 fun should_ensure_in_defaults/2},
                {{temporary, [?CONFIG_DEFAULT, ?CONFIG_FIXTURE_1]},
                 fun should_override_options/2},
                {{temporary, [?CONFIG_DEFAULT, ?CONFIG_FIXTURE_2]},
                 fun should_create_new_sections_on_override/2},
                {{temporary, [?CONFIG_DEFAULT, ?CONFIG_FIXTURE_1,
                              ?CONFIG_FIXTURE_2]},
                 fun should_win_last_in_chain/2}
            ]
        }
    }.

config_persistent_changes_tests() ->
    {
        "Config persistent changes",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [
                {{persistent, [?CONFIG_DEFAULT]},
                 fun should_write_changes/2},
                {{temporary, [?CONFIG_DEFAULT]},
                 fun should_ensure_that_default_wasnt_modified/2},
                {{temporary, [?CONFIG_FIXTURE_TEMP]},
                 fun should_ensure_that_written_to_last_config_in_chain/2}
            ]
        }
    }.

config_no_files_tests() ->
    {
        "Test config with no files",
        {
            foreach,
            fun setup_empty/0, fun teardown/1,
            [
                should_ensure_that_no_ini_files_loaded(),
                should_create_non_persistent_option(),
                should_create_persistent_option()
            ]
        }
    }.

config_listener_behaviour_tests() ->
    {
        "Test config_listener behaviour",
        {
            foreach,
            fun setup_config_listener/0, fun teardown/1,
            [
                fun should_handle_value_change/1,
                fun should_pass_correct_state_to_handle_config_change/1,
                fun should_pass_correct_state_to_handle_config_terminate/1,
                fun should_pass_subscriber_pid_to_handle_config_terminate/1,
                fun should_not_call_handle_config_after_related_process_death/1,
                fun should_remove_handler_when_requested/1
            ]
        }
    }.

should_load_all_configs() ->
    ?_assert(length(config:all()) > 0).

should_locate_daemons_section() ->
    ?_assert(length(config:get("daemons")) > 0).

should_locate_mrview_handler() ->
    ?_assertEqual("{couch_mrview_http, handle_view_req}",
                  config:get("httpd_design_handlers", "_view")).

should_return_undefined_atom_on_missed_section() ->
    ?_assertEqual(undefined,
                  config:get("foo", "bar")).

should_return_undefined_atom_on_missed_option() ->
    ?_assertEqual(undefined,
                  config:get("httpd", "foo")).

should_return_custom_default_value_on_missed_option() ->
    ?_assertEqual("bar",
                  config:get("httpd", "foo", "bar")).

should_only_return_default_on_missed_option() ->
    ?_assertEqual("0",
                  config:get("httpd", "port", "bar")).

should_fail_to_get_binary_value() ->
    ?_assertException(error, badarg,
                  config:get(<<"foo">>, <<"bar">>, <<"baz">>)).

should_return_any_supported_default() ->
    Values = [undefined, "list", true, false, 0.1, 1],
    Tests = [{lists:flatten(io_lib:format("for type(~p)", [V])), V}
        || V <- Values],
    [{T, ?_assertEqual(V, config:get(<<"foo">>, <<"bar">>, V))}
        || {T, V} <- Tests].

should_update_option() ->
    ?_assertEqual("severe",
        begin
            ok = config:set("log", "level", "severe", false),
            config:get("log", "level")
        end).

should_create_new_section() ->
    ?_assertEqual("bang",
        begin
            undefined = config:get("new_section", "bizzle"),
            ok = config:set("new_section", "bizzle", "bang", false),
            config:get("new_section", "bizzle")
        end).

should_fail_to_set_binary_value() ->
    ?_assertException(error, badarg,
        config:set(<<"foo">>, <<"bar">>, <<"baz">>, false)).

should_return_undefined_atom_after_option_deletion() ->
    ?_assertEqual(undefined,
        begin
            ok = config:delete("log", "level", false),
            config:get("log", "level")
        end).

should_be_ok_on_deleting_unknown_options() ->
    ?_assertEqual(ok, config:delete("zoo", "boo", false)).

should_ensure_in_defaults(_, _) ->
    ?_test(begin
        ?assertEqual("500",
                     config:get("couchdb", "max_dbs_open")),
        ?assertEqual("5986",
                     config:get("httpd", "port")),
        ?assertEqual(undefined,
                     config:get("fizbang", "unicode"))
    end).

should_override_options(_, _) ->
    ?_test(begin
        ?assertEqual("10",
                     config:get("couchdb", "max_dbs_open")),
        ?assertEqual("4895",
                     config:get("httpd", "port"))
    end).

should_create_new_sections_on_override(_, _) ->
    ?_test(begin
        ?assertEqual("80",
                     config:get("httpd", "port")),
        ?assertEqual("normalized",
                     config:get("fizbang", "unicode"))
    end).

should_win_last_in_chain(_, _) ->
    ?_assertEqual("80", config:get("httpd", "port")).

should_write_changes(_, _) ->
    ?_test(begin
        ?assertEqual("5986",
                     config:get("httpd", "port")),
        ?assertEqual(ok,
                     config:set("httpd", "port", "8080")),
        ?assertEqual("8080",
                     config:get("httpd", "port")),
        ?assertEqual(ok,
                     config:delete("httpd", "bind_address", "8080")),
        ?assertEqual(undefined,
                     config:get("httpd", "bind_address"))
    end).

should_ensure_that_default_wasnt_modified(_, _) ->
    ?_test(begin
        ?assertEqual("5986",
                     config:get("httpd", "port")),
        ?assertEqual("127.0.0.1",
                     config:get("httpd", "bind_address"))
    end).

should_ensure_that_written_to_last_config_in_chain(_, _) ->
    ?_test(begin
        ?assertEqual("8080",
                     config:get("httpd", "port")),
        ?assertEqual(undefined,
                     config:get("httpd", "bind_address"))
    end).

should_ensure_that_no_ini_files_loaded() ->
    ?_assertEqual(0, length(config:all())).

should_create_non_persistent_option() ->
    ?_assertEqual("80",
        begin
            ok = config:set("httpd", "port", "80", false),
            config:get("httpd", "port")
        end).

should_create_persistent_option() ->
    ?_assertEqual("127.0.0.1",
        begin
            ok = config:set("httpd", "bind_address", "127.0.0.1"),
            config:get("httpd", "bind_address")
        end).

should_handle_value_change({Pid, _}) ->
    ?_test(begin
        ok = config:set("httpd", "port", "80", false),
        ?assertMatch({{"httpd", "port", "80", false}, _}, wait_reply(Pid))
    end).
should_pass_correct_state_to_handle_config_change({Pid, _}) ->
    ?_test(begin
        ok = config:set("httpd", "port", "80", false),
        ?assertMatch({_, {Pid, _, []}}, wait_reply(Pid)),
        ok = config:set("update_state", "foo", "any", false),
        ?assertMatch({Pid, _, ["foo"]}, wait_reply(Pid))
    end).
should_pass_correct_state_to_handle_config_terminate({Pid, _}) ->
    ?_test(begin
        %% prepare some state
        ok = config:set("httpd", "port", "80", false),
        ?assertMatch({_, {Pid, _, []}}, wait_reply(Pid)),
        ok = config:set("update_state", "foo", "any", false),
        ?assertMatch({Pid, _, ["foo"]}, wait_reply(Pid)),

        %% remove handler
        ok = config:set("remove_handler", "any", "any", false),
        Reply = wait_reply(Pid),
        ?assertMatch({stop, _, remove_handler, _}, Reply),

        {stop, Subscriber, _Reason, State} = Reply,
        ?assert(is_pid(Subscriber)),
        ?assertMatch({Pid, Subscriber, ["foo"]}, State)
    end).
should_pass_subscriber_pid_to_handle_config_terminate({Pid, SubscriberPid}) ->
    ?_test(begin
        ok = config:set("remove_handler", "any", "any", false),
        Reply = wait_reply(Pid),
        ?assertMatch({stop, _, remove_handler, _}, Reply),

        {stop, Subscriber, _Reason, _State} = Reply,
        ?assertMatch(SubscriberPid, Subscriber)
    end).
should_not_call_handle_config_after_related_process_death({Pid, _}) ->
    ?_test(begin
        ok = config:set("remove_handler", "any", "any", false),
        Reply = wait_reply(Pid),
        ?assertMatch({stop, _, remove_handler, _}, Reply),

        ok = config:set("httpd", "port", "80", false),
        ?assertMatch(undefined, wait_reply(Pid))
    end).
should_remove_handler_when_requested({Pid, _}) ->
    ?_test(begin
        ?assertEqual(1, n_handlers()),

        ok = config:set("remove_handler", "any", "any", false),
        Reply = wait_reply(Pid),
        ?assertMatch({stop, _, remove_handler, _}, Reply),

        ?assertEqual(0, n_handlers())
    end).

call_sync(Listener, Msg) ->
    Ref = make_ref(),
    Listener ! {Ref, self(), Msg},
    receive
        {ok, Ref, Reply} -> Reply
    after ?TIMEOUT ->
        throw({error, {timeout, call_sync}})
    end.

spawn_listener() ->
    {ok, spawn(fun() -> loop(undefined) end)}.

stop_listener(Listener) ->
    call_sync(Listener, stop).

loop(State) ->
    receive
        {Ref, From, stop} ->
            From ! {ok, Ref, ok},
            ok;
        {Ref, From, {set, Value}} ->
            From ! {ok, Ref, ok},
            loop(Value);
        {Ref, From, get} ->
            From ! {ok, Ref, State},
            loop(undefined)
    end.

n_handlers() ->
    length(gen_event:which_handlers(config_event)).
