-module(ecs).
-behaviour(gen_server).

-export([
    start/0,
    start/2,
    stop/0,
    status/0,
    status/1,
    request/2,
    update/4,
    step/0,
    step/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% SETTINGS

-define(DefaultOpRange, {-3, 5}).
-define(DefaultNumElevators, 2).

% API

% START:
% Starts the Elevator Control System (ECS) Process.
start() ->
    start(?DefaultNumElevators, ?DefaultOpRange).
start(NumberOfElevators, {Low, High}) when is_integer(NumberOfElevators)
                                      andalso is_integer(Low)
                                      andalso is_integer(High)
                                      andalso Low < High ->
    Number = max(NumberOfElevators, 1),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Number, {Low, High}], []);
start(_, _) ->
    message({invalid_args, "start()"}).

% STOP:
% Stops the ECS Process.
%
% Example:
% $> ecs:stop().
stop() ->
    gen_server:call(?MODULE, stop).

% STATUS:
% Requests ECS Elevator Status (i.e. Floors, Directions, Queues,…).
%
% Example:
% $> ecs:status().
% $> ecs:status(1).
status() ->
    gen_server:call(?MODULE, status).

status(ElevatorID) when is_integer(ElevatorID) ->
    gen_server:call(?MODULE, {status, ElevatorID});
status(_) ->
    message({invalid_args, "status()"}).

% REQUEST:
% Requests an elevator to pick-up at Floor "From" and deliver to Floor "To".
%
% Example:
% $> ecs:request(0,1).
request(From, To) when is_integer(From)
                  andalso is_integer(To)
                  andalso From =/= To ->
    gen_server:call(?MODULE, {request, From, To});
request(_, _) ->
    message({invalid_args, "request()"}).

% UPDATE:
% Overwrites an elevator state with new data.
%
% Example:
% -- Args 1 and 2: Moves Elevator with ID 0, to Floor 1.
% -- Arg 3: Sets its direction (+1 = up, 0 = neutral, -1 = down).
% -- Arg 4: Triple of lists representing the queue of floors to visit (Now, NextTrip, TripAfterNext).
% $> ecs:update(0, 1, 1, {[3], [], []}).
%
% QC = Queue Current = List of floors to visit during the current trip (downwards or upwards).
% QN = Queue Next = List of floors to visit after the current trip (i.e. after direction change).
% QAN = Queue After Next = List of floors to visit afte the next trip (i.e. after second direction change).
%
% WARNING:
% Lists (QC, QN, QAN) need not to be specified mindlessly!
% The update() function forcefully applies changes, disregarding the quality of data passed!
% This WILL break desired behavior under certain circumstances:
% E.g. when QC contains values, >= "Floor", with the "Direction" specifying -1.
% I.e. Floors that cannot be reached during a trip as they lie in the opposite direction.
update(ElevatorID, Floor, Direction, {QC, QN, QAN}) when is_integer(ElevatorID)
													andalso is_integer(Direction)
													andalso is_integer(Floor)
									   				andalso is_list(QC)
									   				andalso is_list(QN)
									   				andalso is_list(QAN) ->
    gen_server:call(?MODULE, {update, ElevatorID, Floor, Direction, {QC, QN, QAN}});
update(_,_,_,_) ->
    message({invalid_args, "update()"}).

% STEP:
% Performs N simulation steps.
% A step is defined as an elevators movement from its current floor to one of its immediate neighbors (i.e. 1 -> 2).
%
% Example:
% $> ecs:step(2).
step() ->
    step(1).

step(Size) when is_integer(Size) andalso Size > 0 ->
    gen_server:call(?MODULE, {step, Size});
step(_) ->
    message({invalid_args, "step()"}).

% INTERNAL (GEN_SERVER)

init([NumberOfElevators, {Low, High}]) ->
    process_flag(trap_exit, true),
    {ok, #{ elevator_pids => [ elevator:start(ID, {Low, High}) || ID <- lists:seq(1, NumberOfElevators) ]}}.

handle_call(status, _Caller, State) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), status} || PID <- PIDs ],
    Result = collect(PIDs),
    {reply, Result, State};

handle_call({status, EID}, _Caller, State) ->
    PID = elevator_process_by_id(State, EID),
    case PID =:= void of
        true ->
            {reply, message({invalid_elevator_id, EID}), State};
        false ->
            PID ! {self(), status},
            Result = collect([PID]),
            {reply, Result, State}
    end;

handle_call({step, Size}, _Caller, State) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), {step, Size}} || PID <- PIDs ],
    Result = collect(PIDs),
    {reply, Result, State};

handle_call({request, From, To}, _Caller, State) ->
    PIDs = elevator_processes(State),
    [ PID ! {self(), {suitability, From, To}} || PID <- PIDs ],
    SuitabilityResult = collect(PIDs),
    {ElevatorPID, _} = lists:foldl(fun({NextPID, NextSuitability}, {CurrPID, CurrSuitability}) ->
        case NextSuitability > CurrSuitability of 
            true ->
                {NextPID, NextSuitability};
            false ->
                {CurrPID, CurrSuitability}
        end
    end, {void, 0}, SuitabilityResult),
    case ElevatorPID =:= void of
        true ->
            {reply, message({no_elevator_can_serve, "request()"}), State};
        false ->
            ElevatorPID ! {self(), {request, From, To}},
            Result = collect([ElevatorPID]),
            {reply, Result, State}
    end;

handle_call({update, EID, Floor, Direction, {QC, QN, QAN}}, _Caller, State) ->
    PID = elevator_process_by_id(State, EID),
    case PID =:= void of
        true ->
            {reply, message({invalid_elevator_id, EID}), State};
        false -> 
            PID ! {self(), {update, Floor, Direction, {QC, QN, QAN}}},
            Result = collect([PID]),
            {reply, Result, State}
    end.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% HELPERS

% Collects responses from elevator processes. More fine grained than nece
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
        {PID, {update, Status}} ->
            [ {PID, Status} | collect(PIDs -- [PID])];
        {PID, Response } ->
            [ Response | collect(PIDs -- [PID])];
        _ -> c:flush(), [] % Fail quietly and flush message queue.
    end.

message({invalid_elevator_id, EID}) ->
    "Elevator with ID " ++ integer_to_list(EID) ++ " does not exist.";
message({process_no_response, PID}) ->
    "PID (" ++ pid_to_list(PID) ++ ") did not respond.";
message({invalid_args, FunctionName}) ->
    FunctionName ++ " > Invalid Argument(s).";
message({no_elevator_can_serve, FunctionName}) ->
    FunctionName ++ " > No elevator can serve the desired request (Out of Bounds?).".

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
