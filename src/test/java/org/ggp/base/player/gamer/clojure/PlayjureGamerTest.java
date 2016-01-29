package org.ggp.base.player.gamer.clojure;

import java.util.Arrays;
import java.util.List;
import org.ggp.base.player.gamer.Gamer;
import org.ggp.base.player.gamer.clojure.stubs.PlayjureStub;
import org.ggp.base.player.gamer.statemachine.StateMachineGamer;
import org.ggp.base.util.game.LocalGameRepository;
import org.ggp.base.util.gdl.grammar.GdlPool;
import org.ggp.base.util.gdl.grammar.GdlTerm;
import org.ggp.base.util.match.Match;
import org.ggp.base.util.statemachine.MachineState;
import org.ggp.base.util.statemachine.Move;
import org.ggp.base.util.statemachine.StateMachine;
import org.ggp.base.util.statemachine.implementation.prover.ProverStateMachine;
import org.junit.Assert;
import org.junit.Test;

public class PlayjureGamerTest extends Assert {
    @Test
    public void testPlayjureGamer() {
        try {
            Gamer g = new PlayjureStub();

            assertEquals("Playjure", g.getName());

            LocalGameRepository repo = new LocalGameRepository();
            Match m = new Match("", -1, 1000, 1000,
                                repo.getGame("asteroids"), "");

            g.setMatch(m);
            g.setRoleName(GdlPool.getConstant("ship"));
            g.metaGame(1000);

            StateMachine stateMachine;
            MachineState currentState;
            List<Move> previousMoves;

            stateMachine = new ProverStateMachine();
            stateMachine.initialize(m.getGame().getRules());
            currentState = stateMachine.getInitialState();
            previousMoves = null;
            m.appendState(currentState.getContents());

            java.util.List<Move> moves;
            Move move;

            while (!stateMachine.isTerminal(currentState)) {
                move = new Move(g.selectMove(1000));
                System.out.println("Player chose " + move.toString());

                moves = java.util.Arrays.asList(move);
                m.appendMoves2(moves);
                currentState = stateMachine.getNextState(currentState, moves);
                m.appendState(currentState.getContents());
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
    }

}

