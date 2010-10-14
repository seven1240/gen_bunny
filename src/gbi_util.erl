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

%% @doc Utility functions for writing gen_bunny integration tests.

-module(gbi_util).
-compile([export_all]).

-include("util.hrl").

setup(VHost0) ->
    VHost = list_to_binary(atom_to_list(VHost0)),
    rabbit_mgt:create_vhost(?HOST, VHost),
    rabbit_mgt:set_permission(?HOST, VHost, <<"guest">>,
                               {struct, [{scope, client},
                                         {configure, ?WC},
                                         {write, ?WC},
                                         {read, ?WC}]}),
    VHost.

teardown(VHost) ->
    rabbit_mgt:delete_vhost(?HOST, VHost).

connect(VHost) ->
    {network, #amqp_params{virtual_host=VHost}}.
