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

%% @doc Basic integration tests for the bunnyc client library.
-module(gbi_bunnyc).

-include("util.hrl").

setup(VHost0) ->
    VHost = gbi_util:setup(VHost0),

    {ok, _Pid} = bunnyc:start_link(
                   test,
                   gbi_util:connect(VHost),
                   <<"test">>,
                   []),

    VHost.

teardown(VHost) ->
    bunnyc:stop(test),
    application:stop(gen_bunny),
    ok = gbi_util:teardown(VHost).


simple_publish_test_() ->
    {setup,
     fun () -> setup(simple_publish) end,
     fun teardown/1,
     fun(_VHost) ->
             ?_test([begin
                         bunnyc:publish(test, <<"test">>, <<"foo">>),
                         {Resp, Msg} = bunnyc:get(test, true),
                         ?assertEqual(true, is_record(Resp, 'basic.get_ok')),
                         ?assertEqual(true, is_record(Msg, amqp_msg)),
                         ?assertEqual(<<"foo">>, bunny_util:get_payload(Msg))
                     end])
     end}.

simple_ack_test_() ->
    {setup,
     fun () -> setup(simple_ack) end,
     fun teardown/1,
     fun(VHost) ->
             ?_test([begin
                         bunnyc:publish(test, <<"test">>, <<"foo">>),
                         {Resp, Msg} = bunnyc:get(test, false),

                         ?assertEqual(true, is_record(Resp, 'basic.get_ok')),
                         ?assertEqual(true, is_record(Msg, amqp_msg)),
                         ?assertEqual(<<"foo">>, bunny_util:get_payload(Msg)),

                         ?WAIT,
                         {struct, QueueStats1} = rabbit_mgt:queue(
                                                   gbi_util:rabbit_host(),
                                                   VHost, <<"test">>),

                         ?assertEqual(1, mochilists:get_value(
                                           <<"messages_unacknowledged">>,
                                           QueueStats1)),

                         bunnyc:ack(test, Resp#'basic.get_ok'.delivery_tag),

                         ?WAIT,
                         ?WAIT,
                         {struct, QueueStats2} = rabbit_mgt:queue(
                                                   gbi_util:rabbit_host(),
                                                   VHost, <<"test">>),


                         ?assertEqual(0, mochilists:get_value(
                                           <<"messages_unacknowledged">>,
                                           QueueStats2))
                     end])
     end}.

simple_reconnect_test_() ->
    {timeout, 30, fun() ->
     {setup,
      fun() ->
              setup(simple_reconnect) end,
      fun teardown/1,
      fun(_VHost) ->
              ?_test([begin
                          Connections = rabbit_mgt:connections(
                                          gbi_util:rabbit_host()),

                          ?assertEqual(length(Connections), 1),

                          {struct, Props} = hd(Connections),

                          ?assertEqual(proplists:get_value(<<"vhost">>, Props),
                                       <<"simple_reconnect">>),

                          Name = proplists:get_value(<<"name">>, Props),

                          ok = rabbit_mgt:close_connection(
                                 gbi_util:rabbit_host(), Name),

                          timer:sleep(6000),

                          Connections2 = rabbit_mgt:connections(
                                           gbi_util:rabbit_host()),

                          ?assertEqual(length(Connections2), 1),

                          {struct, Props2} = hd(Connections2),
                          ?assertEqual(
                             proplists:get_value(<<"vhost">>, Props2),
                             <<"simple_reconnect">>),

                          ?assertNot(
                             Name =:= proplists:get_value(<<"name">>, Props2))
                      end])
      end} end}.
