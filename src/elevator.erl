-module(elevator).
-export([
    start/2,
    start_loop/2
]).


start(ID, {Low, High}) ->
    spawn(elevator, start_loop, [ID, {Low, High}]).

% IMPRV: Refactor process dictionary usage into state variable, which is then additionally passed into the functions.
% -> Would make most functions trivially testable, as they'd only depend on their input parameters.
start_loop(ID, {Low, High}) ->
    put(id, ID),
    put(current_floor, 0),
    put(current_direction, 0),
    put(queue_current, gb_sets:new()),
    put(queue_next, gb_sets:new()),
    put(queue_after_next, gb_sets:new()),
    put(operating_range, {Low, High}),
    loop().

loop() ->
    receive
        {Sender, status} ->
            Sender ! {self(), {status, description()}};
        {Sender, id} ->
            Sender ! {self(), {id, get(id)}};
        {Sender, {suitability, From, To}} ->
            Suitability = compute_suitability(From, To, get(operating_range)),
            Sender ! {self(), {suitability, Suitability}};
        {Sender, {request, From, To}} ->
            schedule_request(From, To),
            Sender ! {self(), {request, ok}};
        {Sender, {step, Size}} ->
            perform_steps(Size),
            Sender ! {self(), {step, ok}};
        {Sender, {update, Floor, Direction, {QC, QN, QAN}}} ->
            put(current_floor, Floor),
            put(current_direction, Direction),
            put(queue_current, gb_sets:from_list(QC)),
            put(queue_next, gb_sets:from_list(QN)),
            put(queue_after_next, gb_sets:from_list(QAN)),
            Sender ! {self(), {update, ok}}; 
        {Sender, _} ->
            Sender ! {self(), message_not_understood};
        X -> X
    end,
    loop().

% SIMULATION / STEPPING

% Performs N simulation steps.
% 1 step = 1 elevator movement.
perform_steps(N) when N =< 0 -> true;
perform_steps(N) ->
    QueueCurrent = get(queue_current),
    QueueNext = get(queue_next),
    Direction = case get(current_direction) =:= 0 of
        true -> case crypto:rand_uniform(0,2) =:= 0 of true -> 1; false -> -1 end;
        false -> get(current_direction)
    end,
    case gb_sets:size(QueueCurrent) > 0 of
        true -> step(Direction), 
                perform_steps(N - 1);
        false ->
            case gb_sets:size(QueueNext) > 0 of
                true -> reverse_direction(), step(Direction), perform_steps(N - 1);
                false -> idle()
            end
    end.

% Performs 1 steps.
% X > 0 is UP, X =< 0 is DOWN, 0 results in a noop. 
step(Direction) ->
    if 
        Direction > 0 -> step_up();
        Direction < 0 -> step_down();
        Direction =:= 0 -> noop
    end.

% Moves elevator upwards.
step_up() ->
    QueueCurrent = get(queue_current),
    NewFloor = get(current_floor) + 1,
    put(current_floor, NewFloor),
    case NewFloor =:= gb_sets:smallest(QueueCurrent) of
        true -> 
            {_, NewQueueCurrent} = gb_sets:take_smallest(QueueCurrent),
            put(queue_current, NewQueueCurrent),
            case gb_sets:size(NewQueueCurrent) =:= 0 of
                true -> reverse_direction();
                false -> noop
            end;
        false -> noop
    end.

% Moves elevator downwards.
step_down() ->
    QueueCurrent = get(queue_current),
    NewFloor = get(current_floor) - 1,
    put(current_floor, NewFloor),
    case NewFloor =:= gb_sets:largest(QueueCurrent) of
        true ->
            {_, NewQueueCurrent} = gb_sets:take_largest(QueueCurrent),
            put(queue_current, NewQueueCurrent),
            case gb_sets:size(NewQueueCurrent) =:= 0 of
                true -> reverse_direction();
                false -> noop
            end;
        false -> noop
    end.

% Reverses elevator direction, thereby shifting the queues.
reverse_direction() ->
    put(queue_current, get(queue_next)),
    put(queue_next, get(queue_after_next)),
    put(queue_after_next, gb_sets:new()),
    case gb_sets:size(get(queue_current)) > 0 of
        true -> put(current_direction, -get(current_direction));
        false -> put(current_direction, 0)
    end.

% Puts the elevator into an idle state.
idle() -> put(current_direction, 0).

% FLOOR SCHEDULING

% Schedules a request FROM a floor, TO another floor.
schedule_request(From, To) ->
    Current = get(current_floor),
    ReqDirection = case (From - To) > 0 of true -> -1; false -> 1 end,
    case get(current_direction) =:= 0 of
        true -> put(current_direction, ReqDirection);
        false -> noop
    end,
    Type = compute_movement_type(get(current_floor), From, ReqDirection),
    schedule_request(Type, Current, From, To).

schedule_request(towards_caller_same_direction, Current, From, To) when Current =/= From ->
    NewQueueCurrent = gb_sets:add(To, gb_sets:add(From, get(queue_current))),
    put(queue_current, NewQueueCurrent);

schedule_request(towards_caller_same_direction, _Current, _From, To) ->
    NewQueueCurrent = gb_sets:add(To, get(queue_current)),
    put(queue_current, NewQueueCurrent);

schedule_request(towards_caller_opposite_direction, Current, From, To) when Current =/= From ->
    NewQueueCurrent = gb_sets:add(From, get(queue_current)),
    NewQueueNext = gb_sets:add(To, get(queue_next)),
    put(queue_current, NewQueueCurrent),
    put(queue_next, NewQueueNext);

schedule_request(towards_caller_opposite_direction, _Current, _From, To) ->
    NewQueueNext = gb_sets:add(To, get(queue_next)),
    put(queue_next, NewQueueNext);

schedule_request(away_from_caller, _Current, From, To) ->
    NewQueueNext = gb_sets:add(From, get(queue_next)),
    NewQueueAfterNext = gb_sets:add(To, get(queue_after_next)),
    put(queue_next, NewQueueNext),
    put(queue_after_next, NewQueueAfterNext).

% ELEVATOR SUITABILITY COMPUTATION

compute_suitability(From, To, {Low, High}) ->
    case in_operating_range([From, To]) of
        true -> 
            Current = get(current_floor),
            N = High - Low,
            FloorDelta = From - Current,
            Direction = if
                            FloorDelta > 0 -> 1;
                            FloorDelta < 0 -> -1;
                            FloorDelta =:= 0 -> 
                                case get(current_direction) =:= 0 of
                                    true -> case (From - To) > 0 of true -> -1; false -> 1 end;
                                    false -> get(current_direction)
                                end
                        end,
            Type = compute_movement_type(Current, From, Direction),
            Suitability = compute_suitability({Type, N, FloorDelta}),
            Suitability;
        false -> -1
    end.

compute_suitability({towards_caller_same_direction, N, D}) ->
    (N + 2) - abs(D);
compute_suitability({towards_caller_opposite_direction, N, D}) ->
    (N + 1) - abs(D);
compute_suitability({away_from_caller, _, _}) ->
    1.

compute_movement_type(Current, From, ReqDirection) ->
    Delta = From - Current,
    ElevatorDirection = get(current_direction),
    case ElevatorDirection =:= 0 of
        true ->
            towards_caller_same_direction;
        false ->
            case Delta / ElevatorDirection >= 0 of
                true ->
                    case ElevatorDirection =:= ReqDirection of
                        true ->
                            towards_caller_same_direction;
                        false ->
                            towards_caller_opposite_direction
                    end;
                false -> away_from_caller
            end
    end.

% HELPERS

description() ->
    Direction = direction_as_string(get(current_direction)),
    elevator_as_string() ++ ", Floor: " ++ integer_to_list(get(current_floor)) ++ ", Direction: " ++ Direction ++ ", Queue: " ++ next_stops() ++ ".".

next_stops() -> 
    CurrentQueue = gb_sets:to_list(get(queue_current)),
    NextQueue = gb_sets:to_list(get(queue_next)),
    AfterQueue = gb_sets:to_list(get(queue_after_next)),
    utils:string_format("Current Trip: ~p, Next Trip: ~p, After Next Trip: ~p", [CurrentQueue, NextQueue, AfterQueue]).

elevator_as_string() ->
    pid_to_list(self()) ++ " - Elevator ID: " ++ integer_to_list(get(id)).

direction_as_string(1) -> "UP";
direction_as_string(-1) -> "DOWN";
direction_as_string(_) -> "NONE".

in_operating_range([]) -> true;
in_operating_range(Floors) ->
    {Low, High} = get(operating_range),
    lists:foldl(fun(Floor, Acc) ->
        Acc andalso (Floor =< High andalso Floor >= Low)
    end, true, Floors).
