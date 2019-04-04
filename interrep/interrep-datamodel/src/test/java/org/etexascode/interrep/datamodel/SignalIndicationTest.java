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

package org.etexascode.interrep.datamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 */
public class SignalIndicationTest {

    /** Read in signal manager from xml file, create signal indication */
    SignalManager sm = JaxbRead.readJaxManager("/signalmanager.xml", SignalManager.class);

    List<SignalIndication> siList = new ArrayList<SignalIndication>();

    SignalIndication si = new SignalIndication();

    int laneid = 200;

    double time = 105.00;

    double delta = 0.00001;

    @Before
    public void setUp() throws Exception {
        siList = sm.getSignalsByLaneId(102);
        si = siList.get(0);
        si.setLaneId(laneid);
    }

    @After
    public void tearDown() throws Exception {
        siList.clear();
    }

    @Test
    public void testEqualsSignalIndication() {
        assertEquals(si, si);
        assertFalse(si.equals(new String()));
    }

    @Test
    public void testGetColorIndication() {
        si.setColorIndication(Color.RED);
        assertEquals(Color.RED, si.getColorIndication());
    }

    @Test
    public void testGetLaneId() {
        assertEquals(laneid, si.getLaneId());
    }

    @Test
    public void testGetStateIndication() {
        si.setStateIndication(State.SOFT);
        assertEquals(State.SOFT, si.getStateIndication());
    }

    @Test
    public void testGetTimeToChange() {
        si.setTimeToChange(time);
        assertEquals(time, si.getTimeToChange(), delta);
    }

    @Test
    public void testGetTypeIndication() {
        si.setTypeIndication(Type.STOP_SIGN);
        assertEquals(Type.STOP_SIGN, si.getTypeIndication());
    }

    @Test
    public void testSetColorIndication() {
        si.setColorIndication(Color.RED);
        assertEquals(Color.RED, si.getColorIndication());
    }

    @Test
    public void testSetLaneId() {
        si.setLaneId(205);
        assertEquals(205, si.getLaneId());
    }

    @Test
    public void testSetStateIndication() {
        si.setStateIndication(State.SOFT);
        assertEquals(State.SOFT, si.getStateIndication());
    }

    @Test
    public void testSetTimeToChange() {
        si.setTimeToChange(time);
        assertEquals(time, si.getTimeToChange(), delta);
    }

    @Test
    public void testSetTypeIndication() {
        si.setTypeIndication(Type.STOP_SIGN);
        assertEquals(Type.STOP_SIGN, si.getTypeIndication());
    }

    @Test
    public void testToString() {
        assertEquals(si.toString(), si.toString());
        si.setColorIndication(null);
        si.setTypeIndication(null);
        si.setStateIndication(null);
        assertEquals(si.toString(), si.toString());

    }

}
