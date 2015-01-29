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

-define(DEPS, [couch_stats, couch_log, folsom, lager, goldrush, syntax_tools, compiler]).


setup() ->
    setup(?CONFIG_CHAIN).
setup({temporary, Chain}) ->
    setup(Chain);
setup({persistent, Chain}) ->
    setup(lists:append(Chain, [?CONFIG_FIXTURE_TEMP]));
setup(Chain) ->
    [ok = application:start(App) || App <- lists:reverse(?DEPS)],
    {ok, Pid} = test_util:start_config(Chain),
    Pid.

setup_empty() ->
    setup([]).

teardown(Pid) ->
    config:stop(),
    [ok = application:stop(App) || App <- ?DEPS],
    erlang:monitor(process, Pid),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout_error, config_stop})
    end.
teardown(_, Pid) ->
    teardown(Pid).


config_test_() ->
    {
        "CouchDB config tests",
        [
            config_get_tests(),
            %% config_set_tests(),
            %% config_del_tests(),
            config_override_tests()
            %% config_persistent_changes_tests(),
            %% config_no_files_tests()
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
                should_get_binary_option()
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
                should_set_binary_option()
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
                should_be_ok_on_deleting_unknown_options(),
                should_delete_binary_option()
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

should_get_binary_option() ->
    ?_assertEqual(<<"baz">>,
                  config:get(<<"foo">>, <<"bar">>, <<"baz">>)).

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

should_set_binary_option() ->
    ?_assertEqual(<<"baz">>,
        begin
            ok = config:set(<<"foo">>, <<"bar">>, <<"baz">>, false),
            config:get(<<"foo">>, <<"bar">>)
        end).

should_return_undefined_atom_after_option_deletion() ->
    ?_assertEqual(undefined,
        begin
            ok = config:delete("log", "level", false),
            config:get("log", "level")
        end).

should_be_ok_on_deleting_unknown_options() ->
    ?_assertEqual(ok, config:delete("zoo", "boo", false)).

should_delete_binary_option() ->
    ?_assertEqual(undefined,
        begin
            ok = config:set(<<"foo">>, <<"bar">>, <<"baz">>, false),
            ok = config:delete(<<"foo">>, <<"bar">>, false),
            config:get(<<"foo">>, <<"bar">>)
        end).

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
