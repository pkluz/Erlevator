-module(ecs).
-behaviour(gen_server).

-export([
    start/0,
    start/2,
    stop/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    status/0,
    status/1,
    request/2,
    update/3,
    step/0,
    step/1
]).

% SETTINGS

-define(DefaultOpRange, {-3, 5}).
-define(DefaultNumElevators, 2).

% API

start() ->
    start(?DefaultNumElevators, ?DefaultOpRange). % Default: Five elevators serving 57 floors.

start(NumberOfElevators, {Low, High}) when Low < High ->
    Number = max(NumberOfElevators, 1),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Number, {Low, High}], []);

start(_, _) ->
    message({invalid_args, "Start"}).

stop() ->
    gen_server:call(?MODULE, stop).

status() ->
    gen_server:call(?MODULE, status).

status(ElevatorID) when is_integer(ElevatorID) ->
    gen_server:call(?MODULE, {status, ElevatorID});

status(_) ->
    message({invalid_args, "Status"}).

request(RequestFloor, RequestDestination) when is_integer(RequestFloor)
                                          andalso is_integer(RequestDestination)
                                          andalso RequestFloor =/= RequestDestination ->
    gen_server:call(?MODULE, {request, RequestFloor, RequestDestination});

request(_, _) ->
    message({invalid_args, "Request"}).

update(ElevatorID, NewFloor, NewQueue) when is_integer(ElevatorID) andalso is_integer(NewFloor) andalso is_list(NewQueue) ->
    gen_server:call(?MODULE, {update, ElevatorID, NewFloor, NewQueue});

update(_,_,_) ->
    message({invalid_args, "Update"}).

step() ->
    step(1).

step(Size) when is_integer(Size) ->
    gen_server:call(?MODULE, {step, max(Size, 1)});

step(_) ->
    message({invalid_args, "Step"}).

% INTERNAL (GEN_SERVER)

init([NumberOfElevators, {Low, High}]) ->
    process_flag(trap_exit, true),
    {ok, #{ elevator_pids => [ elevator:start(ID, {Low, High}) || ID <- lists:seq(1, NumberOfElevators) ]}}.

handle_call(status, _From, State) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), status} || PID <- PIDs ],
    Result = collect(PIDs),
    {reply, Result, State};

handle_call({status, EID}, _From, State) ->
    PID = elevator_process_by_id(State, EID),
    case PID =:= void of
        true ->
            {reply, message({invalid_elevator_id, EID}), State};
        false ->
            PID ! {self(), status},
            Result = collect([PID]),
            {reply, Result, State}
    end;

handle_call({step, Size}, _From, State) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), {step, Size}} || PID <- PIDs ],
    Result = collect(PIDs),
    {reply, Result, State};

handle_call({request, RequestFloor, RequestDestination}, _From, State) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), {suitability, RequestFloor, RequestDestination}} || PID <- PIDs ],
    SuitabilityResult = collect(PIDs),
    {ElevatorPID, _} = lists:foldl(fun({NextPID, NextSuitability}, {CurrentPID, CurrentSuitability}) ->
        case NextSuitability > CurrentSuitability of 
            true ->
                {NextPID, NextSuitability};
            false ->
                {CurrentPID, CurrentSuitability}
        end
    end, {void, 0}, SuitabilityResult),
    case ElevatorPID =:= void of
        true ->
            {reply, message({no_elevator_can_serve, "Request"}), State};
        false ->
            ElevatorPID ! {self(), {request, RequestFloor, RequestDestination}},
            Result = collect([ElevatorPID]),
            {reply, Result, State}
    end;

handle_call({update, EID, NewFloor, NewQueue}, _From, State) ->
    PID = elevator_process_by_id(State, EID),
    case PID =:= void of
        true ->
            {reply, message({invalid_elevator_id, EID}), State};
        false -> 
            PID ! {self(), {update, NewFloor, NewQueue}},
            {reply, ok, State}
    end.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% HELPERS

collect([]) -> [];
collect(PIDs) ->
    receive
        {PID, {status, Description}} ->
            [Description | collect(PIDs -- [PID])];
        {PID, {suitability, Value}} ->
            [ {PID, Value} | collect(PIDs -- [PID])];
        {PID, {id, ID}} ->
            [ {PID, ID} | collect(PIDs -- [PID])];
        {PID, {step, Status}} ->
            [ {PID, Status} | collect(PIDs -- [PID])];
        {PID, {request, Status}} ->
            [ {PID, Status} | collect(PIDs -- [PID])];
        {PID, Response } ->
            [ Response | collect(PIDs -- [PID])];
        _ -> c:flush(), [] % Fail quietly and discard messages.
    end.

message({invalid_elevator_id, EID}) ->
    "Elevator with ID " ++ integer_to_list(EID) ++ " does not exist.";
message({process_no_response, PID}) ->
    "PID (" ++ pid_to_list(PID) ++ ") did not respond.";
message({invalid_args, FunctionName}) ->
    FunctionName ++ " > Invalid Argument(s).";
message({no_elevator_can_serve, FunctionName}) ->
    FunctionName ++ " > No elevator can serve the desired request.".

elevator_processes(State) ->
    maps:get(elevator_pids, State, []).

elevator_process_by_id(State, EID) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), id} || PID <- PIDs ],
    Result = collect(PIDs),
    Elevator = utils:head(lists:filter(fun({_, ID}) -> ID =:= EID end, Result)),
    case Elevator =:= void of
        true -> void;
        false ->
            {PID, _} = Elevator,
            PID
    end.
