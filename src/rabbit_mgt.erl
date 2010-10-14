%% The MIT License

%% Copyright (c) David Reid <dreid@dreid.org>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% @doc RabbitMQ management API client.
-module(rabbit_mgt).

-export([vhosts/1, create_vhost/2, delete_vhost/2]).
-export([set_permission/4]).

-define(MGT_PORT, 55672).

vhosts(Host) ->
    {200, Body} = get_(endpoint(Host, vhosts)),
    Body.

create_vhost(Host, VHost) ->
    {204, _} = put_(endpoint(Host, vhost, [{vhost, VHost}]), []),
    ok.

set_permission(Host, VHost, User, Body) ->
    {204, _} = put_(endpoint(Host, permission_vhost_user, [{vhost, VHost},
                                                           {user, User}]),
                    Body),
    ok.

delete_vhost(Host, VHost) ->
    {204, _} = delete_(endpoint(Host, vhost, [{vhost, VHost}])),
    ok.

%% Private
%% =======
endpoint(Host, Name) ->
    endpoint(Host, Name, []).
endpoint(Host, Name, Params) ->
    mochifmt:f("http://{host}:{port}/api/" ++ ep(Name),
               [{host, Host}, {port, ?MGT_PORT} | Params]).

ep(connections) ->
    "connections";
ep(connection) ->
    "connections/{connection}";
ep(channels) ->
    "channels";
ep(channel) ->
    "channels/{channel}";
ep(exchanges) ->
    "exchanges";
ep(exchanges_vhost) ->
    "exchanges/{vhost}";
ep(exchange) ->
    "exchanges/{vhost}/{name}";
ep(exchange_bindings) ->
    "exchanges/{vhost}/{name}/bindings";
ep(queues) ->
    "queues";
ep(queues_vhost) ->
    "queues/{vhost}";
ep(queue) ->
    "queues/{vhost}/{queue}";
ep(queue_bindings) ->
    "queues/{vhost}/{queue}/bindings";
ep(bindings) ->
    "bindings";
ep(bindings_vhost) ->
    "bindings/{vhost}";
ep(bindings_vhost_queue_exchange) ->
    "bindings/{vhost}/{queue}/{exchange}";
ep(binding) ->
    %% I don't quite understand what props is supposed to
    %% be here.
    %%
    %% From the rabbitmq docs:
    %% An individual binding between a queue and an exchange. The props part
    %% of the URI is a "name" for the binding composed of its routing key and
    %% properties. While you can create a binding by PUTing to this URI, it
    %% may be more convenient to POST to the URI above.
    "binding/{vhost}/{queue}/{exchange}/{props}";
ep(vhosts) ->
    "vhosts";
ep(vhost) ->
    "vhosts/{vhost}";
ep(users) ->
    "users";
ep(user) ->
    "users/{user}";
ep(user_permissions) ->
    "users/{user}/permissions";
ep(permissions) ->
    "permissions";
ep(permission_vhost_user) ->
    "permissions/{vhost}/{user}";
ep(Unknown) ->
    throw({error, {unknown_endpoint, Unknown}}).


body(Data = [{_, _} | _]) ->
    mochiweb_util:urlencode(Data);
body(Data = {struct, _}) ->
    iolist_to_binary(mochijson2:encode(Data));
body(Data) ->
    Data.


get_(Endpoint) ->
    get_(Endpoint, []).


get_(Endpoint0, Data) ->
    Endpoint = case Data of
                   [] ->
                       Endpoint0;
                   [{_, _} | _] ->
                       Endpoint0 ++ "?" ++ mochiweb_util:urlencode(Data)
               end,

    {200, Body} = request("GET", Endpoint, []),
    {200, mochijson2:decode(Body)}.



put_(Endpoint, Data) ->
    request("PUT", Endpoint, Data).


post_(Endpoint, Data) ->
    request("POST", Endpoint, Data).


delete_(Endpoint) ->
    request("DELETE", Endpoint, []).


request(Method, Endpoint, Data) ->
    application:start(crypto),
    application:start(ssl),
    application:start(lhttpc),

    case lhttpc:request(Endpoint, Method,
                        [{"Authorization", "Basic Z3Vlc3Q6Z3Vlc3Q="},
                         {"Content-Type", "application/json"}],
                        body(Data), 10000, []) of
        {ok, {{Code, _}, _, Body}} ->
            {Code, Body};
        {error, Error} ->
            {error, Error}
    end.
