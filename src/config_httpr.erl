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

-module(config_httpr).


-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    delete_resource/2,
    to_json/2,
    from_json/2
]).


-include_lib("webmachine/include/webmachine.hrl").
-include_lib("chttpd2/include/chttpd2.hrl").


-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.


content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.


content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.


allowed_methods(ReqData, Context) ->
    {['GET', 'PUT', 'DELETE'], ReqData, Context}.


resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.


-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    Parts = wrq:path_tokens(ReqData),
    Data = get_config(Parts),
    couch_log:error("PATH IS(~p): ~p", [Parts, Data]),
    {?JSON_ENCODE(Data), ReqData, State}.


-spec from_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
from_json(ReqData, State) ->
    %% how to handle intentional pattern matching fail on malformed requests?
    [Section, Key] = wrq:path_tokens(ReqData),
    %% this will also throw an error when body is undefined
    Value = chttpd2_util:json_body(ReqData),
    Persist = wrq:get_req_header("X-Couch-Persist", ReqData) /= "false",
    OldValue = config:get(Section, Key, ""),
    ok = config:set(Section, Key, binary_to_list(Value), Persist),
    couch_log:error("Setting value(~p): ~p [~p]", [Persist, Value, OldValue]),
    Resp = wrq:set_resp_body(?JSON_ENCODE(list_to_binary(OldValue)), ReqData),
    Resp1 = wrq:set_resp_header("Location", "/_config/foo/baroob", Resp),
    {true, Resp1, State}.


delete_resource(ReqData, State) ->
    %% how to handle intentional pattern matching fail on malformed requests?
    [Section, Key] = wrq:path_tokens(ReqData),
    Persist = wrq:get_req_header("X-Couch-Persist", ReqData) /= "false",
    case config:get(Section, Key, null) of
        %% why does this clause throw a 500?
        null ->
            {false, ReqData, State};
        OldValue ->
            config:delete(Section, Key, Persist),
            Resp = wrq:set_resp_body(
                ?JSON_ENCODE(list_to_binary(OldValue)),
                ReqData
            ),
            {true, Resp, State}
    end.


get_config([]) ->
    {config:all_list()};
get_config([Section]) ->
    {config:section_list(Section)};
get_config([Section, Key]) ->
    case config:get(Section, Key, null) of
    null ->
        %% move this to resource_exists/2?
        throw({not_found, unknown_config_value});
    Value ->
        %% Should we turn this into a proper json value?
        list_to_binary(Value)
    end.




