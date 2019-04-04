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
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test Signal manager class. Uses signal manager in src/main/resources/signalmanager.xml
 *
 * @author bmauldon
 */
public class SignalManagerTest {

    SignalManager sm = JaxbRead.readJaxManager("/signalmanager.xml", SignalManager.class);

    List<SignalIndication> siList = new ArrayList<SignalIndication>();

    SignalIndication si = new SignalIndication();

    @Before
    public void setUp() throws Exception {
        /**
         * Get list with one signal indication, laneId=102
         */
        siList = sm.getSignalsByLaneId(102);
        si = siList.get(0);
        si.setLaneId(200);

    }

    @After
    public void tearDown() throws Exception {
        JAXBContext jc = JAXBContext.newInstance(SignalManager.class);
        Marshaller m = jc.createMarshaller();
        SignalIndication signalIndication = new SignalIndication();
        signalIndication.setLaneId(1);
        sm.addSignal(signalIndication);
        m.marshal(sm, System.out);

        siList.clear();
    }

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testAddSignal() {
        sm.addSignal(si);
        assertEquals(si, sm.getSignalsByLaneId(200).get(0));
        SignalIndication si2 = new SignalIndication();
        si2.setLaneId(50);
        sm.addSignal(si2);
        assertEquals(si2, sm.getSignalsByLaneId(50).get(0));
    }

    @Test
    public void testEqualsSignalManager() {
        assertEquals(sm, sm);
        assertFalse(sm.equals(new String()));
    }

    @Test
    public void testGetSignalsByLaneId() {
        sm.addSignal(si);
        assertEquals(si, sm.getSignalsByLaneId(200).get(0));

    }

    @Test
    public void testAddSignals() {
        sm.addSignals(siList);

        for (SignalIndication si : siList) {

            assertTrue(siList.contains(si));
        }
    }

    @Test
    public void testSigManIterator() {
        SignalManager sm = new SignalManager();
        sm.addSignal(si);
        Iterable<SignalIndication> iterable = sm.getIterable();
        Iterator<SignalIndication> iterator = iterable.iterator();
        while (iterator.hasNext()) {
            SignalIndication signal = iterator.next();
            assertTrue(si.equals(signal));
            try {
                iterator.remove();
                thrown.expect(UnsupportedOperationException.class);
            }
            catch (UnsupportedOperationException uoe) {}
        }
    }
}
