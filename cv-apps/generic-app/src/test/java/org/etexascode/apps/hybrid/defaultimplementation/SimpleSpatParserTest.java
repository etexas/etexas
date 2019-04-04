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
package org.etexascode.apps.hybrid.defaultimplementation;

import static org.junit.Assert.assertEquals;

import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.j2735.IntersectionState;
import org.etexascode.j2735.IntersectionState.States;
import org.etexascode.j2735.MovementState;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.SPAT.IntersectionStates;
import org.etexascode.j2735.util.SignalLightState;
import org.junit.Before;
import org.junit.Test;

/**
 * Simple SPAT parser test.
 * 
 * @author jrutherford
 */
public class SimpleSpatParserTest {

    public SPAT spat;

    @Before
    public void setup() {
        spat = new SPAT();
        MovementState ms = new MovementState();
        ms.setTimeToChange(30);
        ms.setCurrState(SignalLightState.BALL_GREEN);
        ms.setLaneSet(new byte[] { 1 });
        States states = new States();
        states.getMovementState().add(ms);
        IntersectionState state = new IntersectionState();
        state.setId(new byte[] { 0, 0 });
        state.setLanesCnt((short)1);
        state.setName("Test");
        state.setPreempt(new byte[] { 0, 0 });
        state.setPriority(new byte[] { 0, 0 });
        state.setStates(states);
        state.setStatus(new byte[] { 0, 0 });
        state.setTimeStamp(100);
        IntersectionStates istates = new IntersectionStates();
        istates.getIntersectionState().add(state);
        spat.setIntersectionStates(istates);
    }

    @Test
    public void testParseSignalManager1() {
        SimpleSpatParser svp = new SimpleSpatParser();
        Object mes[] = { 0, 0 };
        assertEquals(null, svp.parseSignalManager(mes).getSignalsByLaneId(1));
    }

    @Test
    public void testParseSignalManager2() {
        SimpleSpatParser svp = new SimpleSpatParser();
        Object mes[] = { spat };
        assertEquals(1, svp.parseSignalManager(mes).getSignalsByLaneId(1).size());
        assertEquals(Color.GREEN, svp.parseSignalManager(mes).getSignalsByLaneId(1).get(0).getColorIndication());
        assertEquals(Type.BALL, svp.parseSignalManager(mes).getSignalsByLaneId(1).get(0).getTypeIndication());
        assertEquals(State.STEADY, svp.parseSignalManager(mes).getSignalsByLaneId(1).get(0).getStateIndication());
    }
}