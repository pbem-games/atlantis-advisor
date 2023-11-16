function f()
  ShowMessage('Hello from LUA', 'foo', ', bar')
end

-- sample script sctructure

name = 'Script name'

-- will run only once after turn is loaded
function startup(args, unit, region, mem)
    -- args: arguments passed to script
    -- unit: current unit
    -- region: current region
    -- mem: memory table for the current unit (will persist between stages)
end

-- set all flags here and decide what to do later in the turn
function initial(args, unit, region, mem)
end

-- do actions here that are not monthlong
function action(args, unit, region, mem)
end

-- do monthlong actions here
function monthlong(args, unit, region, mem)
end

-- final stage, suitable for different messages and checks
function final(args, unit, region, mem)
end

--[[

You do ns.[turn stage] to define a function that will run on that turn stage
NOTE: script will run BEFORE the client will process order of that state and
      mutate the game state

NOTE: need to make scripts communicate with each other

All turn stages:
  startup (will run only once after turn is loaded)
  initial (before instant orders)
    form
    name
    autotax
    avoid
    behind
    guard
    hold
    noaid
    nocross
    share
    consume
    reveal
    spoils
    claim
    combat
    declare
    describe
    faction
    leave
    enter
    promote
    evict
    attack
    steal
    destroy
  action (before orders that change inventory)
    give
    pillage
    tax
    cast
    sell
    buy
    forget
  monthlong (before monthlong orders)
    sail
    move
    advance
    build
    entertain
    produce
    study
    teach
    work
    transport
    distribute
  final (after all orders)

]]--