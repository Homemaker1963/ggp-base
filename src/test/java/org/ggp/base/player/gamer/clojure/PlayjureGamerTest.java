package org.ggp.base.player.gamer.clojure;

import org.ggp.base.player.gamer.Gamer;
import org.ggp.base.player.gamer.clojure.stubs.PlayjureStub;
import org.ggp.base.util.game.GameRepository;
import org.ggp.base.util.gdl.grammar.GdlPool;
import org.ggp.base.util.match.Match;
import org.junit.Assert;
import org.junit.Test;

public class PlayjureGamerTest extends Assert {
    @Test
    public void testPlayjureGamer() {
        try {
            Gamer g = new PlayjureStub();
            System.out.println("TEST");

            assertEquals("Playjure", g.getName());

            Match m = new Match("", -1, 1000, 1000, GameRepository.getDefaultRepository().getGame("ticTacToe"), "");
            g.setMatch(m);
            g.setRoleName(GdlPool.getConstant("xplayer"));
            g.metaGame(1000);
            assertTrue(g.selectMove(1000) != null);
        } catch(Exception e) {
            e.printStackTrace();
        }
    }
}
