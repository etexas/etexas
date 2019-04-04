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

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735.exp.GIDExp;
import org.etexascode.j2735.exp.SPATExp;
import org.etexascode.j2735.util.SizeCalculators;
import org.junit.Before;
import org.junit.Test;

public class SizeCalculatorsTest {

    ILaneManager lmi = null;

    ISignalManager smi = null;

    double simTime = 1.5;

    @Before
    public void setup() {
        lmi = getLaneManager();
        smi = getSignalManager();
    }

    @Test
    public void testGetSizeGID() {
        GIDExp msg = GIDProducerApp.getGIDFromLaneMan(lmi);
        assertEquals(318, SizeCalculators.getSize(msg));
    }

    @Test
    public void testGetSizeSPAT() {
        SPATExp msg = SPATProducerAppExp.getCurrentSPATMessage(smi, lmi, simTime);
        assertEquals(65, SizeCalculators.getSize(msg));
    }

    private SignalManager getSignalManager() {
        SignalManager sm = new SignalManager();
        sm.addSignal(getSignalIndication(1, SignalIndication.Color.RED, 28.5));
        sm.addSignal(getSignalIndication(2, SignalIndication.Color.RED, 28.5));
        sm.addSignal(getSignalIndication(9, SignalIndication.Color.RED, 28.5));
        sm.addSignal(getSignalIndication(10, SignalIndication.Color.RED, 28.5));

        sm.addSignal(getSignalIndication(5, SignalIndication.Color.GREEN, 28.5));
        sm.addSignal(getSignalIndication(6, SignalIndication.Color.GREEN, 28.5));
        sm.addSignal(getSignalIndication(13, SignalIndication.Color.GREEN, 28.5));
        sm.addSignal(getSignalIndication(14, SignalIndication.Color.GREEN, 28.5));
        return sm;
    }

    private SignalIndication getSignalIndication(int laneID, SignalIndication.Color color, double timeToChange) {
        SignalIndication si = new SignalIndication();
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(laneID);
        si.setTimeToChange(timeToChange);
        si.setColorIndication(color);

        return si;
    }

    private LaneManager getLaneManager() {
        LaneManager lm = new LaneManager();
        lm.setElevation(0.0);
        lm.setLatitude(0.0);
        lm.setLongitude(0.0);

        Map<Integer, Lane> lmap = new HashMap<Integer, Lane>();

        lmap.put(1, getLane(1, Lane.INBOUND, Movement.LEFT_TURN, Movement.STRAIGHT));
        lmap.put(2, getLane(2, Lane.INBOUND, Movement.RIGHT_TURN, Movement.STRAIGHT));
        lmap.put(3, getLane(3, Lane.OUTBOUND, Movement.STRAIGHT, null));
        lmap.put(4, getLane(4, Lane.OUTBOUND, Movement.STRAIGHT, null));

        lmap.put(5, getLane(5, Lane.INBOUND, Movement.LEFT_TURN, Movement.STRAIGHT));
        lmap.put(6, getLane(6, Lane.INBOUND, Movement.RIGHT_TURN, Movement.STRAIGHT));
        lmap.put(7, getLane(7, Lane.OUTBOUND, Movement.STRAIGHT, null));
        lmap.put(8, getLane(8, Lane.OUTBOUND, Movement.STRAIGHT, null));

        lmap.put(9, getLane(9, Lane.INBOUND, Movement.LEFT_TURN, Movement.STRAIGHT));
        lmap.put(10, getLane(10, Lane.INBOUND, Movement.RIGHT_TURN, Movement.STRAIGHT));
        lmap.put(11, getLane(11, Lane.OUTBOUND, Movement.STRAIGHT, null));
        lmap.put(12, getLane(12, Lane.OUTBOUND, Movement.STRAIGHT, null));

        lmap.put(13, getLane(13, Lane.INBOUND, Movement.LEFT_TURN, Movement.STRAIGHT));
        lmap.put(14, getLane(14, Lane.INBOUND, Movement.RIGHT_TURN, Movement.STRAIGHT));
        lmap.put(15, getLane(15, Lane.OUTBOUND, Movement.STRAIGHT, null));
        lmap.put(16, getLane(16, Lane.OUTBOUND, Movement.STRAIGHT, null));

        lm.setLanes(lmap);
        return lm;
    }

    private Lane getLane(int id, String type, LaneMovement.Movement move1, LaneMovement.Movement move2) {
        Lane l = new Lane();
        l.addLaneNode(new LaneNode(0.0, 0.0));
        l.addLaneNode(new LaneNode(0.0, 0.0));

        l.setLaneId(id);
        l.setType(type);

        LaneMovement lmov1 = new LaneMovement();
        lmov1.setMovement(move1);
        lmov1.setMovementId(1000 * id + 0);
        l.getLaneMovements().put(lmov1.getMovementId(), lmov1);

        if (move2 != null) {
            LaneMovement lmov2 = new LaneMovement();
            lmov2.setMovement(move2);
            lmov2.setMovementId(1000 * id + 1);
            l.getLaneMovements().put(lmov2.getMovementId(), lmov2);
        }
        return l;
    }
}
