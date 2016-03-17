## Propnet Results

We implemented the propnet in PlayjurePropnetStateMachine.  We did it in Java
because it sounded really painful to try to do this in Clojure.  We just
implemented really simple backwards-reasoning without any fancy optimizations.

We made a little test harness in src/main/resources/playjure/propnet.clj to test
things out.  run-test will run both the ProverStateMachine and our propnet state
machine on the entire game tree of a given game and assert that the terminal
values, legal moves, and goal values (when appropriate) match at every node.
For example:

    playjure.propnet=> (run-test "ticTacToe")
    Checked 1000 states
    Checked 2000 states
    Checked 3000 states
    Checked 4000 states
    Checked 5000 states
    9

    playjure.propnet=> (run-test "asteroids")
    Checked 1000 states
    Checked 2000 states
    ...
    Ctrl-C'ed after 200,000+ states checked successfully

    playjure.propnet=> (run-test "connectFour")
    Checked 1000 states
    Checked 2000 states
    ...
    Ctrl-C'ed after 200,000+ states checked successfully

We also made a simple benchmarking function that will run a number of depth
charges from the initial state with each machine and output the milliseconds per
charge:

    playjure.propnet=> (benchmark-game "reversi" 50)
    Benchmarking reversi ...
        Benchmarking Prover ...
        Benchmarking Propnet ...
    Ran 50 depth charges each.
         Prover took: 2915.42 ms per charge.
        Propnet took: 1681.9 ms per charge.

    playjure.propnet=> (benchmark-game "knightsTour" 1000)
    Benchmarking knightsTour ...
        Benchmarking Prover ...
        Benchmarking Propnet ...
    Ran 1000 depth charges each.
         Prover took: 3.589 ms per charge.
        Propnet took: 1.787 ms per charge.

    playjure.propnet=> (benchmark-game "eightPuzzle" 30)
    Benchmarking eightPuzzle ...
        Benchmarking Prover ...
        Benchmarking Propnet ...
    Ran 30 depth charges each.
         Prover took: 101.566666 ms per charge.
        Propnet took: 181.66667 ms per charge.

Sometimes it's faster than the prover, sometimes not.  It depends on the game.
