Erlang Workshop
===============

This in an Erlang Workshop by Lmabda Academy.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Erlang Workshop](#erlang-workshop)
    - [0. Clone repository](#0-clone-repository)
    - [1. Create project and 1st module](#1-create-project-and-1st-module)
    - [2. Create tests](#2-create-tests)
    - [3. Perform code coverage analysis](#3-perform-code-coverage-analysis)
    - [4. Add documentation](#4-add-documentation)
    - [5. Create a server](#5-create-a-server)
    - [6. Add asynchronous interface](#6-add-asynchronous-interface)
    - [7. Make it gen_server](#7-make-it-genserver)
    - [8. Supervise](#8-supervise)
    - [9. Package as application](#9-package-as-application)
    - [10. Add external dependency](#10-add-external-dependency)
    - [11. Release](#11-release)

<!-- markdown-toc end -->

## 0. Clone repository

```
$ git clone https://github.com/lambdaacademy/erlang-workshop.git
$ cd erlang-workshop
```

## 1. Create project and 1st module

First create an Erlang application:

```shell
$ ./rebar3 new lib name="abacus" desc="Erlang Abacus"
```

> [rebar3] is an Erlang build tool. It has built in help: `./rebar3 help`.

Move `rebar3` into the `abacus` directory that was created. From now on, all
the actions will be performed in that directory.

Edit the `src/abacus.erl` and create 4 functions for arithmetic operations:

1. `addition/2`
2. `subtraction/2`
3. `multiplication/2`
4. `division/2`.

Save the files, compile, and run an Erlang shell:

```shell
$ ./rebar3 compile
$ ./rebar3 shell
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
1> abacus:addition(2,3).
5
```

> With `rebar3 shell` the recommended way to quit the Erlang shell is to either use `init:stop().`
> or `q()` as Ctrl+C Ctrl+C breaks your OS shell formatting.

## 2. Create tests

If you haven't done previous step, start with `git checkout 1-create-project-and-1st-module`.

Create the `test` directory and add `test/abacus_tests.erl` starting with

```erlang
-module(abacus_tests).
-include_lib("eunit/include/eunit.hrl").
```

> [EUnit] is a Lightweight Unit Testing Framework for Erlang.

Write 4 unit tests for the `abacus` functions implemented in the first step:

```erlang
add_test() ->
    ?assertEqual(2+3, abacus:addition(2,3)).
%% ...
```

Save and run the tests with `rebar3`:

```shell
$ ./rebar3 eunit
Finished in 0.026 seconds
4 tests, 0 failures
```

## 3. Perform code coverage analysis

If you haven't done previous step, start with `git checkout 2-create-tests`.

Edit `rebar.config` and enable cover:

```erlang
{cover_enabled,true}.
```

> [Cover] is a Coverage Analysis Tool for Erlang.

Run the eunit tests again to generate the coverage data. Then perform the
analysis and open the report:

```shell
$ ./rebar3 do eunit, cover -v
Finished in 0.026 seconds
4 tests, 0 failures
  |------------------------|------------|
  |                module  |  coverage  |
  |------------------------|------------|
  |                abacus  |      100%  |
  |------------------------|------------|
  |                 total  |      100%  |
  |------------------------|------------|
$ open _build/test/cover/index.html
```

## 4. Add documentation

If you haven't done previous step, start with `git checkout 3-perform-code-coverage-analysis`.

Edit `abacus.erl` to add module and the API functions documentation
in the EDoc format:

```erlang
%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus API module.
%%
%% It provides basic mathematical operations:
%% <li>addition,</li>
%% <li>subtraction,</li>
%% <li>multiplication</li>,
%% <li>division</li>.
-module(abacus).

%% ...
%% @doc Adds two integers
%%
%% This adds `X' to `Y'.
addition(X, Y) ->
    X+Y.
```

> [Edoc] is the Erlang documentation generator.

Save. Generate and open EDoc documentation.

```
$ ./rebar3 edoc

$ open doc/index.html
```

## 5. Create a server

If you haven't done the previous step, start with `git checkout 4-add-documentation`.

Add `src/abacus_srv.erl`. Implement `abacus_srv:start/1` and `abacus_srv:stop/1`
API functions that will spawn and kill the `abacus_srv` process respectively.

```erlang
start(Name) ->
    Pid = spawn(?MODULE, init, [Name]),
    register(?Module, Pid).
stop(Name) ->
    exit(whereis(Pid), shutdown).
```

Implement the `addition/3`, `subtraction/3`, `multiplication/3`, `division/3`
functions in the `abacus_srv` module that will send messages to the `abacus_srv`
process which in turn will call corresponding functions from the `abacus` module
and send the response back the the caller:

```erlang
addition(Name, X, Y) ->
    Request = {addition, X, Y},
    send_request(Name, Request).
send_request(Name, Request) ->
    Name ! {abacus_request, self(), Request},
    receive
        {abacus_resopnse, {ok, Result}} ->
            Result;
        {abacus_resopnse, {error, _} = Error} ->
            Error
    after 5000 ->
            {error, timeout}
    end.
```

Save, compile and test the server:

```shell
./rebar3 shell
...
1> abacus_srv:start(a1).
<0.95.0>
2> abacus_srv:addition(a1,10,3).
13
3> abacus_srv:division(a1,10,3).
3
4> whereis(a1).
<0.95.0>
5> abacus_srv:stop(a1).
true
6> whereis(a1).
undefined
```

## 6. Add asynchronous interface

If you haven't done the previous step, start with
`git checkout 5-create-a-server`.

Add asynchronous version of the `abacus` functions to the `src/abacus_srv.erl`
(`async_addition/1`, ...) and an API for retrieving the results of asynchronous
calls: `abacus_srv:result_by_reference/1`. The asynchronous functions are
expected to return references (see [make_ref/1]) which are to be used to get
the results:

```erlang
async_addition(Name, X, Y) ->
    Request = {addition, X, Y},
    send_async_request(Name, Request).
send_async_request(Name, Request) ->
    Ref = make_ref(),
    Name ! {abacus_async_request, Ref, Request},
    Ref.

result_by_reference(Name, Ref) ->
    Request = {result_by_reference, Ref},
    send_request(Name, Request).
```

The server's process needs to store the results of asynchronous requests in its
state. Extend the `loop/0` and `response/1` functions to handle the asynchronous
requests and retrieving the results respectively:

```erlang
loop(State) ->
    receive
        ...
        {abacus_async_request, Ref, Request} ->
            Response = response(Request, State),
            NewState = store_response(Ref, Response, State),
            loop(NewState);
        ...
    end.

...
response({division, X, Y}, _State) ->
    abacus:division(X, Y);
response({result_by_reference, Ref}, State) ->
    retrieve_response(Ref, State).
```

Save, compile and test the server:

```shell
./rebar3 shell
...
1> abacus_srv:start(a).
<0.100.0>
2> Ref1=abacus_srv:async_addition(a,13,2).
#Ref<0.0.1.465>
3> Ref2=abacus_srv:async_subtraction(a,13,2).
#Ref<0.0.1.471>
4> abacus_srv:result_by_reference(a,Ref1).
15
5> abacus_srv:result_by_reference(a,Ref2).
11
6> abacus_srv:result_by_reference(a,make_ref()).
unknown_reference
```

## 7. Make it gen_server

If you haven't done the previous step, start with
`git checkout 6-add-asynchronous-interface`.

Copy the `abacus_srv.erl` into `abacus_gen_srv.erl`. Make it [gen_server]
behaviour (check what [behaviour] is) :

```erlang
%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%
%% @doc This is the Abacus Server.
-module(abacus_gen_srv).
-behaviour(gen_server).
```

Change the life-cycle API to use respective `gen_server` functions
and change the `init/1` to match the [specification][gen_server_init].
Also add the `terminate/2` according to the [specification][gen_server_terminate].

```erlang
start(Name) ->
    ServerName = {local, Name},
    CallbackModule = ?MODULE,
    InitArgs = [],
    Opts = [],
    gen_server:start(ServerName, CallbackModule, InitArgs, Opts).

stop(Name) ->
    gen_server:stop(Name).

init(_) ->
    {ok, #{}}.

terminate(_Reason, _State) ->
    ok.
```

Change the synchronous API to use appropriate `gen_server` API and implement
corresponding callback functions:

```erlang
addition(Name, X, Y) ->
    Request = {addition, X, Y},
    gen_server:call(Name, Request).

handle_call(Request, _From, State) ->
    Response = response(Request, State),
    {reply, {ok, Response}, State}.
```

Change the asynchronous API to use appropriate `gen_server` API and implement
corresponding callback functions:

```erlang
async_addition(Name, X, Y) ->
    Request = {addition, X, Y},
    Ref = make_ref(),
    gen_server:cast(Name, {Ref, Request}),
    {ok, Ref}.

handle_cast({Ref, Request}, State) ->
    Response = response(Request, State),
    NewState = store_response(Ref, Response, State),
    {noreply, NewState}.
```

Don't forget to export the `gen_server` callback functions:

```erlang
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).
```

Save, compile and test the server:

```shell
./rebar3 shell
...
1> abacus_gen_srv:start(a).
{ok,<0.95.0>}
2> abacus_gen_srv:multiplication(a,12,3).
{ok,36}
3> {ok,Ref}=abacus_gen_srv:async_division(a,12,3).
{ok,#Ref<0.0.2.656>}
4> abacus_gen_srv:result_by_reference(a,Ref).
{ok,4}
5> abacus_gen_srv:stop(a).
```

## 8. Supervise

If you haven't done the previous step, start with
`git checkout 7-make-it-gen_server`.

Add `abacus_sup.erl` module that will implement the [supervisor]
behaviour:

```erlang
-module(abacus_sup).
-behavoiur(supervisor).
```

Implement the supervisor `start_link/0` function that will start the supervisor:

```erlang
start_link(AbacusName) ->
    SupervisorName = {local, ?MODULE},
    CallbackModule = ?MODULE,
    InitArgs = [AbacusName],
    supervisor:start_link(SupervisorName, CallbackModule, InitArgs).
```

Implement the `init/1` callback function that configures the supervisor itself
and starts its children:

```erlang
init([AbacusName]) ->
    SupFlags = #{intensity => 5, %% max 5 restarts
                 period => 10, %% in 10 seconds
                 strategy => one_for_one},
    AbacusStart = {abacus_gen_srv, start_link, [AbacusName]}, %% MFA
    Child = #{id => abacus_gen_srv,
              start => AbacusStart,
              restart => permanent,
              shutdown => 1000,
              type => worker,
              modules => [abacus_gen_srv]},
    Children = [Child],
    {ok, {SupFlags, Children}}.
```

As the supervisor requires that its children [link][links] back to it, change the
`abacus_gen_srv:start/1` to `abacus_gen_srv:start_link/1` and adjust the
call to `gen_server`:

```erlang
start_link(Name) ->
    ServerName = {local, Name},
    CallbackModule = ?MODULE,
    InitArgs = [],
    Opts = [],
    gen_server:start_link(ServerName, CallbackModule, InitArgs, Opts).
```

Save, compile and test the supervisor:

```shell
$ ./rebar3 shell
...
6> abacus_sup:start_link(server1).
{ok,<0.109.0>}
7> abacus_gen_srv:addition(server1,9,10).
{ok,19}
8> whereis(server1).
<0.110.0>
9> supervisor:which_children(abacus_sup).
[{abacus_gen_srv,<0.110.0>,worker,[abacus_gen_srv]}]
10> abacus_gen_srv:stop(server1).
ok
11> whereis(server1).
<0.116.0> %% note the different pid
12> exit(whereis(server1), kill).
true
13> whereis(server1).
<0.119.0> %% note the different pid
```

## 9. Package as application

If you haven't done the previous step, start with
`git checkout 8-supervise`.

Add the `abacus_app.erl`: the callback module for [application] behaviour.
Implement the `abacus_app:start/2` which will start the application's
top-level supervisor and an "empty" `abacus_sup:stop/1` function:

```erlang
-module(abacus_app).
-behavoiur(application).

%% Life-cycle API
-export([start/2,
         stop/1]).

start(_Type, _Args) ->
    abacus_sup:start_link(server1).

stop(_) ->
    ok.
```

Modify the `abacus.app.src` ([Application Resource File][app]) so that the
application is properly started:

```erlang
{application, abacus,
 [{description, "Erlang Abacus"},
  {vsn, "0.1.0"},
  {registered, []},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},
  {mod, {abacus_app, []}}, %% <- the missing line
  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

Instead of hard-coding the abacus server name as `server1`, pass it through
the application environment variables using `application:get_env/3`:

```erlang
-define(DEFAULT_ABACUS_NAME, server1).

start(_Type, _Args) ->
    Application = abacus,
    Key = server_name,
    ServerName = application:get_env(Application, Key, ?DEFAULT_ABACUS_NAME),
    abacus_sup:start_link(ServerName).
```

Finally add the `config/abacus.config` file with the following content:

```erlang
[{abacus,
  [{server_name, abs}]
 }].
```

Save, compile and test the application:

```shell
./rebar3 shell --config config/abacus.config
1> application:ensure_all_started(abacus).
{ok,[abacus]}
2> whereis(abs).
<0.100.0>
3> abacus_gen_srv:division(abs, 12, 5).
{ok,2}
4> application:stop(abacus).
ok
5> whereis(abs).
undefined
```

Start the application once again (as described above) and run
[Observer]:

```erlang
observer:start().
```

In Observer navigate to `Applications` -> `abacus`. This will show
the supervision tree of the `abacus` application.

## 10. Add external dependency

If you haven't done the previous step, start with
`git checkout 9-package-as-application`.

Modify the `rebar.config` file and add [lager](`lager`) dependency
that will be used for logging:

```erlang
{erl_opts, [debug_info, {parse_transform, lager_transform}]}.
{deps, [
        {lager, "~> 3.2"}
       ]}.
```

Modify the `abacus.app.src` file so that the `lager` application
is started before `abacus`:

```erlang
...
{applications,
   [kernel,
    stdlib,
    lager
   ]},
...
```

Add some logging to your code-base:

```erlang
init(_) ->
    {registered_name, Name} = erlang:process_info(self(),
                                                  registered_name),
    lager:info("Started abacus_gen_srv: ~p", [Name]),
    {ok, #{}}.

handle_call(Request, _From, State) ->
    lager:debug("Handling sync request: ~p", [Request]),
    Response = response(Request, State),
    {reply, {ok, Response}, State}.
```

Configure `lager` handler along with the log levels in the
`config/abacus.config` file:

```erlang
[
 {abacus, [
           {server_name, abs}
          ]},
 {lager, [
          {handlers, [
                      {lager_console_backend, info},
                      {lager_file_backend, [{file, "log/debug.log"}, {level, debug}]}
                     ]}
         ]}
].
```

Save, compile and test the application as in the previous step.

## 11. Release

If you haven't done the previous step, start with
`git checkout 10-add-external-dependency`.

Add the following [relx][`relx`] section in the `rebar.config`:

```erlang
{relx, [
        {release, {abacus, "0.1.0"}, [abacus]},
        {include_erts, true}
       ]}.
```

Save. Build the release and start it:

```shell
./rebar3 release
...
===> release successfully created!
17:25:24.648 [info] Application lager started on node 'abacus@szm-mac'
17:25:24.652 [info] Started abacus_gen_srv: abs
17:25:24.652 [info] Application abacus started on node 'abacus@szm-mac'
Eshell V8.1  (abort with ^G)
(abacus@szm-mac)1> application:which_applications().
[{abacus,"Erlang Abacus","0.1.0"},
 {lager,"Erlang logging framework","3.2.1"},
 {goldrush,"Erlang event stream processor","0.1.8"},
 {compiler,"ERTS  CXC 138 10","7.0.2"},
 {syntax_tools,"Syntax tools","2.1"},
 {stdlib,"ERTS  CXC 138 10","3.1"},
 {kernel,"ERTS  CXC 138 10","5.1"}]
(abacus@szm-mac)2> abacus_gen_srv:multiplication(abs, 13, 4).
{ok,52}
(abacus@szm-mac)3>
```

[rebar3]: https://www.rebar3.org
[EUnit]: http://erlang.org/doc/apps/eunit/chapter.html
[Cover]: http://erlang.org/doc/man/cover.html
[EDoc]: http://erlang.org/doc/apps/edoc/chapter.html
[make_ref/1]: http://erlang.org/doc/man/erlang.html#make_ref-0
[gen_server]: http://erlang.org/doc/man/gen_server.html
[gen_server_init]: http://erlang.org/doc/man/gen_server.html#Module:init-1
[gen_server_terminate]: http://erlang.org/doc/man/gen_server.html#Module:terminate-2
[supervisor]: http://erlang.org/doc/man/supervisor.html
[application]: http://erlang.org/doc/man/application
[app]: http://erlang.org/doc/man/app.html
[Observer]: http://erlang.org/doc/man/observer
[lager]: https://github.com/basho/lager
[relx]: https://github.com/erlware/relx
[behaviour]: http://erlang.org/doc/design_principles/des_princ.html#id69904
[links]: http://erlang.org/doc/man/erlang.html#link-1
