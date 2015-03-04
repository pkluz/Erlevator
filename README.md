# Erlevator
Erlevator is an Elevator Control System (ECS) Simulator in Erlang.

# Setup
- If Necessary: **Install Erlang**.
	- On OS X using Homebrew: Type `brew install erlang` in your Terminal.
	- Alternatively (and on other systems) visit [Erlang Solutions - Downloads](https://www.erlang-solutions.com/downloads/download-erlang-otp) and install it manually.
- To compile the source, execute: `make all`.
- The simulation relies on Erlang's built-in REPL. `cd` into the `bin` folder and execute `erl`.

<br />
# Noteworthy
Once you have started Erlang's interactive REPL, here's what you should know:
- `erl` **Starts the REPL**.
- `CMD + C` followed by `a` – **Closes the REPL**.
- All Erlang commands are terminated using a period "."

<br />

# How-To: Simulation

Here is how to use the simulator in Erlang's REPL.

## 1. Start the ECS Process.
Start the ECS using the default settings:
```
$> ecs:start().
``` 

…alternatively you can customize the settings!


Start the ECS with **2** elevators, and an operating range of **16** floors (-5 to 15):
```
$> ecs:start(2, {-5,10}).
```

## 2. Inquire about the Elevators Statuses.
To find out more about each elevator, find out about their status:
```
$> ecs:status().
```
### Example Status Output:
```
["<0.36.0> - Elevator ID: 1, Floor: 0, Direction: NONE, Queue: Current Trip: [], Next Trip: [], After Next Trip: [].",
 "<0.37.0> - Elevator ID: 2, Floor: 0, Direction: NONE, Queue: Current Trip: [], Next Trip: [], After Next Trip: []."]
```

To find out more about a specific elevator, address it by its ID.

```
$> ecs:status(1).
```

## 3. Request an Elevator.
In order to request an Elevator going from the ground floor (0), to the third floor (3):
```
$> ecs:request(0, 3).
```

## 4. Update an Elevator.
In order to override an Elevator's state, use the update method:
```
-- Args 1 and 2: Move Elevator with ID 0, to Floor 1.
-- Arg 3: Sets its direction (+1 = up, 0 = neutral, -1 = down).
-- Arg 4: Triple of lists representing the queue of floors to visit (Current Queue, Next Queue, After Next Queue).

$> ecs:update(0, 1, 1, {[3], [], []}).
```

## 5. Perform a Simulation Step.
A **step** is defined as an elevators movement from its current floor to one of its immediate neighbors (i.e. 1 -> 2).

**Perform a (1) Step:**
```
$> ecs:step().
```

**Perform 3 Steps:**

```
$> ecs:step(3).
```

<br />

# Why Erlang?

I have written the Erlevator using Erlang due to its powerful concurrency model, which was predestinated for this kind of simulation. It allowed me to not worry about concurrency at all, and let the program simulate even the largest buildings with potentially hundreds of elevators.

Instead of complex threads and synchronization mechanisms, Erlang utilizes **green threads** (called *Processes*) and **message passing** to communicate between these processes.

In Erlevator, each **Elevator in the system is modeled as an entirely self-sufficient process**.

<br />

# Under The Hood.


## Finding a suitable Elevator

Erlevator utilizes the **Nearest Lift** method to calculate a suitable elevator to respond to a request. This is done by computing a **Suitability Value** for each elevator and picking the best one.


```
-- If the elevator moves towards a call, and the destination is in the same direction:
SV = (N + 2) - D

-- If the elevator moves towards a call, and the destination is in the opposite direction:
SV = (N + 1) - D

-- Else:
1

With N = (Number Of Floors - 1) and D = (Number of Floors between Elevator and Caller).
```

## Scheduling a Request

Erlevator implements a variant of the standard **Elevator Algorithm** (aka SCAN) to schedule calls. The algorithm proceeds to travel in one direction, until there is no more requests in that direction or it reached the end – then it reverses direction and does the same thing again.

This is implemented using **three priority queues**:
- **QC** (= Queue Current) consists of all the request that are to be handled while traveling in the current direction. (*E.g.* Elevator at floor 0, going up. Caller at floor 2, wanting to go UP to 4).
- **QN** (= Queue Next) consists of the requests that can only be handled, when the elevator reverses direction. (*E.g.* Elevator at floor 0, going UP to 3. Caller at floor 1 wanting to go DOWN to 0).
- **QAN** (= Queue After Next) consists of the requests that can only be handled, when the elevator reverses direction twice. (*E.g.* Elevator at floor 0, going UP to 7. Caller at floor -2 wanting to go UP to 3).

After each direction reversal (i.e. QC empty), QN becomes QC, QAN becomes QN, and a new priority queue for QAN is created to fill the void.

When traveling in a certain direction, the elevator either consumes the smallest or the largest element of the current queue (**QC**).

This approach is superior to a simple FCFS (First-Come First-Serve), as it requires the elevators to **travel significantly less distance**, thus **reaching destination floors quicker** thereby **minimizing waiting time**.
