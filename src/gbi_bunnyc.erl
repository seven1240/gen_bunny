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

simple_publish_test() ->
    VHost = gbi_util:setup(simple_publish),

    {ok, _Pid} = bunnyc:start_link(
                   test,
                   gbi_util:connect(VHost),
                   <<"test">>,
                   []),
    bunnyc:publish(test, <<"test">>, <<"foo">>),
    {Resp, Msg} = bunnyc:get(test, true),
    ?assertEqual(true, is_record(Resp, 'basic.get_ok')),
    ?assertEqual(true, is_record(Msg, amqp_msg)),
    ?assertEqual(<<"foo">>, bunny_util:get_payload(Msg)),

    ok = gbi_util:teardown(VHost).
