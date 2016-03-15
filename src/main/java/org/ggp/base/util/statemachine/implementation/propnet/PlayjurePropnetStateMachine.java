package org.ggp.base.util.statemachine.implementation.propnet;

// Imports {
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ggp.base.util.gdl.grammar.Gdl;
import org.ggp.base.util.gdl.grammar.GdlConstant;
import org.ggp.base.util.gdl.grammar.GdlRelation;
import org.ggp.base.util.gdl.grammar.GdlSentence;
import org.ggp.base.util.propnet.architecture.Component;
import org.ggp.base.util.propnet.architecture.PropNet;
import org.ggp.base.util.propnet.architecture.components.Proposition;
import org.ggp.base.util.propnet.architecture.components.Transition;
import org.ggp.base.util.propnet.factory.OptimizingPropNetFactory;
import org.ggp.base.util.statemachine.MachineState;
import org.ggp.base.util.statemachine.Move;
import org.ggp.base.util.statemachine.Role;
import org.ggp.base.util.statemachine.StateMachine;
import org.ggp.base.util.statemachine.PropNetMachineState;
import org.ggp.base.util.statemachine.exceptions.GoalDefinitionException;
import org.ggp.base.util.statemachine.exceptions.MoveDefinitionException;
import org.ggp.base.util.statemachine.exceptions.TransitionDefinitionException;
import org.ggp.base.util.statemachine.implementation.prover.query.ProverQueryBuilder;

// }

@SuppressWarnings("unused")
public class PlayjurePropnetStateMachine extends StateMachine {
    // Members {

    /** The underlying proposition network  */
    // TODO: flip this back to private
    public PropNet propNet;

    /** The topological ordering of the propositions */
    private List<Proposition> ordering;

    /** The player roles */
    private List<Role> roles;

    private HashSet<Component> proved;
    private HashSet<Component> proving;

    // }
    // Initialization {

    /**
     * Initializes the PropNetStateMachine. You should compute the topological
     * ordering here. Additionally you may compute the initial state here, at
     * your discretion.
     */
    @Override
    public void initialize(List<Gdl> description) {
        proved = new HashSet();
        proving = new HashSet();

        try {
            propNet = OptimizingPropNetFactory.create(description);
            roles = propNet.getRoles();
            ordering = getOrdering();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    // }
    // Basic Queries {

    /**
     * Computes if the state is terminal. Should return the value
     * of the terminal proposition for the state.
     */
    @Override
    public boolean isTerminal(MachineState state) {
        resetFromState(state);
        Proposition terminal = propNet.getTerminalProposition();
        prove(terminal);
        return terminal.getValue();
    }

    /**
     * Computes the goal for a role in the current state.
     * Should return the value of the goal proposition that
     * is true for that role. If there is not exactly one goal
     * proposition true for that role, then you should throw a
     * GoalDefinitionException because the goal is ill-defined.
     */
    @Override
    public int getGoal(MachineState state, Role role)
            throws GoalDefinitionException {
        resetFromState(state);

        Set<Proposition> goals = propNet.getGoalPropositions().get(role);

        int result = -1;

        for (Proposition g : goals) {
            prove(g);

            if (g.getValue()) {
                if (result != -1) {
                    throw new GoalDefinitionException(state, role);
                } else {
                    result = getGoalValue(g);
                }
            }
        }

        return result;
    }

    public List<Move> getLegalMoves(MachineState state, Role role) throws MoveDefinitionException {
        resetFromState(state);

        Set<Proposition> moves = propNet.getLegalPropositions().get(role);

        ArrayList legalMoves = new ArrayList();

        for (Proposition m : moves) {
            prove(m);

            if (m.getValue()) {
                legalMoves.add(getMoveFromProposition(m));
            }
        }

        return legalMoves;
    }

    // }
    // State Calculation (initial/next) {

    private Set<Proposition> trueBasePropositions() {
        HashSet<Proposition> props = new HashSet();
        for (Proposition base : propNet.getBasePropositions().values()) {
            if (base.getSingleInput().getValue()) {
                props.add(base);
            }
        }
        return props;
    }

    /**
     * Returns the initial state. The initial state can be computed
     * by only setting the truth value of the INIT proposition to true,
     * and then computing the resulting state.
     */
    @Override
    public MachineState getInitialState() {
        reset();

        propNet.getInitProposition().setValue(true);

        return new PropNetMachineState(trueBasePropositions());
    }

    /**
     * Computes the next state given state and the list of moves.
     */
    @Override
    public MachineState getNextState(MachineState state, List<Move> moves) throws TransitionDefinitionException {
        resetFromState(state);

        List<GdlSentence> doeses = toDoes(moves);
        Map<GdlSentence,Proposition> inputs = propNet.getInputPropositions();

        for (GdlSentence does : doeses) {
            inputs.get(does).setValue(true);
        }

        for (Component c : propNet.getComponents()) {
            if (c instanceof Transition) {
                System.out.println();
                System.out.println(c);
                System.out.println(c.getSingleInput());
                prove(c.getSingleInput());
            }
        }

        for (Component c : propNet.getComponents()) {
            if (c instanceof Transition) {
                Proposition p = (Proposition) c.getSingleOutput();
                p.setValue(c.getValue());
            }
        }

        return new PropNetMachineState(trueBasePropositions());
    }

    // }
    // Ordering (unused) {

    /**
     * This should compute the topological ordering of propositions.
     * Each component is either a proposition, logical gate, or transition.
     * Logical gates and transitions only have propositions as inputs.
     *
     * The base propositions and input propositions should always be exempt
     * from this ordering.
     *
     * The base propositions values are set from the MachineState that
     * operations are performed on and the input propositions are set from
     * the Moves that operations are performed on as well (if any).
     *
     * @return The order in which the truth values of propositions need to be set.
     */

    public List<Proposition> getOrdering() {
        // List to contain the topological ordering.
        List<Proposition> order = new LinkedList<Proposition>();

        // All of the components in the PropNet
        List<Component> components = new ArrayList<Component>(propNet.getComponents());

        // All of the propositions in the PropNet.
        List<Proposition> propositions = new ArrayList<Proposition>(propNet.getPropositions());

        // TODO: Compute the topological ordering.

        return order;
    }

    // }
    // Proving {

    public void reset() {
        for (Proposition p : propNet.getPropositions()) {
            p.setValue(false);
        }
    }
    public void resetFromState(MachineState state) {
        PropNetMachineState s = (PropNetMachineState) state;

        reset();

        for (Proposition p : s.getPropositions()) {
            p.setValue(true);
        }

        proving = new HashSet();

        proved = new HashSet(propNet.getBasePropositions().values());
        proved.addAll(propNet.getInputPropositions().values());
        proved.add(propNet.getInitProposition());
    }
    public boolean prove(Component prop) {
        assert !(prop instanceof Transition);

        if (proved.contains(prop)) {
            return prop.getValue();
        }

        if (proving.contains(prop)) {
            int lol = 1 / 0;
        }

        proving.add(prop);

        for (Component input : prop.getInputs()) {
            prove(input);
        }

        if (prop instanceof Proposition) {
            Proposition p = (Proposition)prop;
            System.out.println(p);
            p.setValue(p.getSingleInput().getValue());
        }

        proving.remove(prop);
        proved.add(prop);

        return prop.getValue();
    }

    // }
    // Helper methods {

    @Override
    public List<Role> getRoles() {
        return roles;
    }

    /**
     * The Input propositions are indexed by (does ?player ?action).
     *
     * This translates a list of Moves (backed by a sentence that is simply ?action)
     * into GdlSentences that can be used to get Propositions from inputPropositions.
     * and accordingly set their values etc.  This is a naive implementation when coupled with
     * setting input values, feel free to change this for a more efficient implementation.
     *
     * @param moves
     * @return
     */
    private List<GdlSentence> toDoes(List<Move> moves) {
        List<GdlSentence> doeses = new ArrayList<GdlSentence>(moves.size());
        Map<Role, Integer> roleIndices = getRoleIndices();

        for (int i = 0; i < roles.size(); i++)
        {
            int index = roleIndices.get(roles.get(i));
            doeses.add(ProverQueryBuilder.toDoes(roles.get(i), moves.get(index)));
        }
        return doeses;
    }

    /**
     * Takes in a Legal Proposition and returns the appropriate corresponding Move
     * @param p
     * @return a PropNetMove
     */
    public static Move getMoveFromProposition(Proposition p) {
        return new Move(p.getName().get(1));
    }

    /**
     * Helper method for parsing the value of a goal proposition
     * @param goalProposition
     * @return the integer value of the goal proposition
     */
    private int getGoalValue(Proposition goalProposition) {
        GdlRelation relation = (GdlRelation) goalProposition.getName();
        GdlConstant constant = (GdlConstant) relation.get(1);
        return Integer.parseInt(constant.toString());
    }

    /**
     * A Naive implementation that computes a PropNetMachineState
     * from the true BasePropositions.  This is correct but slower than more advanced implementations
     * You need not use this method!
     * @return PropNetMachineState
     */
    public MachineState getStateFromBase() {
        Set<GdlSentence> contents = new HashSet<GdlSentence>();
        for (Proposition p : propNet.getBasePropositions().values())
        {
            p.setValue(p.getSingleInput().getValue());
            if (p.getValue())
            {
                contents.add(p.getName());
            }

        }
        return new MachineState(contents);
    }

    // }
}
