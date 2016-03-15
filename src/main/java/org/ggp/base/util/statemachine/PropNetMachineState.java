package org.ggp.base.util.statemachine;

import java.util.HashSet;
import java.util.Set;

import org.ggp.base.util.propnet.architecture.components.Proposition;
import org.ggp.base.util.statemachine.MachineState;

public class PropNetMachineState extends MachineState {
    private Set<Proposition> propositions;

    public PropNetMachineState() {
        this.propositions = null;
    }
    public PropNetMachineState(Set<Proposition> propositions) {
        this.propositions = propositions;
    }

    @Override
    public MachineState clone() {
        return new PropNetMachineState(new HashSet<Proposition>(propositions));
    }

    public Set<Proposition> getPropositions() {
        return propositions;
    }

    /* Utility methods */
    @Override
    public int hashCode() {
        return getPropositions().hashCode();
    }

    @Override
    public String toString() {
        Set<Proposition> propositions = getPropositions();

        if(propositions == null)
            return "(PropNetMachineState with null contents)";
        else
            return propositions.toString();
    }

    @Override
    public boolean equals(Object o) {
        if ((o != null) && (o instanceof PropNetMachineState)) {
            PropNetMachineState state = (PropNetMachineState) o;
            return state.getPropositions().equals(getPropositions());
        }

        return false;
    }
}
