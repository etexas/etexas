/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package com.harmonia.apps.exp;

import java.util.HashSet;
import java.util.List;

import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;

/**
 * A utility class used by the spat producer to associate a set of lanes with the list of signal
 * indications those lanes all possess.
 * 
 * @author ablatt
 */
public class IndsMeaning {

    List<ISignalIndication> sis;

    HashSet<Integer> laneIds;

    /**
     * Constructor.
     * 
     * @param sis The list of signal indications associated with the lane.
     * @param laneId The id of the lane in question
     */
    public IndsMeaning(List<ISignalIndication> sis, int laneId) {
        this.sis = sis;
        laneIds = new HashSet<Integer>();
        laneIds.add(laneId);
    }

    /**
     * Determines if this object can (validly) be combined with im.
     * 
     * @param im The other IndsMeaning to compare if its combinable.
     * @return True if the objects can be combined, False otherwise.
     */
    boolean canCombineWith(IndsMeaning im) {
        if (im.sis.size() != sis.size()) {
            return false;
        }

        boolean b = true;
        for (ISignalIndication si : im.sis) {
            b &= containsSpecial(sis, si);
            if (!b) {
                break;
            }
        }

        return b;
    }

    /**
     * A utility method for determining if si is in ls
     * 
     * @param ls The list of signal indications.
     * @param si The signal indication to check if its in the list.
     * @return Utility method for determining if the signal indication is in the list.
     */
    boolean containsSpecial(List<ISignalIndication> ls, ISignalIndication si) {
        for (ISignalIndication s : ls) {
            boolean b = s.getColorIndication() == si.getColorIndication();
            b &= s.getStateIndication() == si.getStateIndication();
            b &= s.getTypeIndication() == si.getTypeIndication();
            b &= closeEnough(s.getTimeToChange(), si.getTimeToChange());
            if (b) {
                return true;
            }
        }

        return false;
    }

    /**
     * A method which merges im with the current object (does not preserve the state of this object)
     * 
     * @param im Combines to IndsMeanings.
     */
    void combine(IndsMeaning im) {
        laneIds.addAll(im.laneIds);
    }

    /**
     * determines if d1 and d2 are close enough to be considered equal
     * 
     * @param d1 First double value.
     * @param d2 Second double value.
     * @return True if both doubles are within a .05 threshold of each other, False otherwise.
     */
    boolean closeEnough(double d1, double d2) {
        return (d1 - d2) < 0.05 && (d2 - d1) < 0.05;
    }
}
