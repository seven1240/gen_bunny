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

%% @doc Basic integration tests for the gen_bunny library.
-module(gbi_gen_bunny).

-include("util.hrl").

setup(VHost0, NoAck) ->
    VHost = gbi_util:setup(VHost0),

    {ok, _Pid} = bunnyc:start_link(
                   test,
                   gbi_util:connect(VHost),
                   <<"bunny.test">>,
                   []),

    {ok, Pid} = test_gb:start_link(gbi_util:connect(VHost),
                                   [{no_ack, NoAck}]),
    {VHost, Pid}.

teardown({VHost, Pid}) ->
    test_gb:stop(Pid),
    bunnyc:stop(test),
    application:stop(gen_bunny),
    ok = gbi_util:teardown(VHost).

simple_consumer_test_() ->
    {setup,
     fun () -> setup(simple_consumer, true) end,
     fun teardown/1,
     fun({_VHost, Pid}) ->
             ?_test([begin
                         bunnyc:publish(test, <<"bunny.test">>, <<"foo">>),
                         ?WAIT,
                         [Msg] = test_gb:get_messages(Pid),
                         ?assertEqual(true, is_record(Msg, amqp_msg)),
                         ?assertEqual(<<"foo">>, bunny_util:get_payload(Msg))
                     end])
     end}.


simple_consumer_ack_test_() ->
    {setup,
     fun () -> setup(simple_consumer_ack, false) end,
     fun teardown/1,
     fun({VHost, Pid}) ->
             ?_test([begin
                         bunnyc:publish(test, <<"bunny.test">>, <<"foo">>),
                         ?WAIT,
                         [{Tag,Msg}] = test_gb:get_messages(Pid),
                         ?assertEqual(true, is_record(Msg, amqp_msg)),
                         ?assertEqual(<<"foo">>, bunny_util:get_payload(Msg)),

                         ?WAIT,
                         {struct, QueueStats1} = rabbit_mgt:queue(
                                                   gbi_util:rabbit_host(),
                                                   VHost, <<"bunny.test">>),

                         ?assertEqual(1, mochilists:get_value(
                                           <<"messages_unacknowledged">>,
                                           QueueStats1)),

                         test_gb:ack_stuff(Pid, Tag),

                         ?WAIT,
                         ?WAIT,
                         {struct, QueueStats2} = rabbit_mgt:queue(
                                                   gbi_util:rabbit_host(),
                                                   VHost, <<"bunny.test">>),


                         ?assertEqual(0, mochilists:get_value(
                                           <<"messages_unacknowledged">>,
                                           QueueStats2))
                     end])
     end}.


simple_consumer_reconnect_test_() ->
    {timeout, 30, fun() ->
     {setup,
      fun() ->
              setup(simple_consumer_reconnect, false) end,
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
