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

import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class StaticDataTest {

    SignalManager sm = JaxbRead.readJaxManager("/signalmanager.xml", SignalManager.class);

    /** Read DetectorManager from xml file */
    DetectorManager dm = JaxbRead.readJaxManager("/detectormanager.xml", DetectorManager.class);

    /** read lane manager from xmlfile */
    LaneManager lm = JaxbRead.readJaxManager("/lanemanager.xml", LaneManager.class);

    SimMetaData metaData = new SimMetaData();

    StaticData sd = new StaticData();

    @Before
    public void setUp() throws Exception {
        sd.setSignalManager(sm);
        sd.setDetectorManager(dm);
        sd.setLaneManager(lm);
        sd.setMetaData(metaData);
    }

    @After
    public void tearDown() throws Exception {
        sd = null;
    }

    @Test
    public void testGetDetectorManager() {
        assertEquals(dm, sd.getDetectorManager());
    }

    @Test
    public void testGetLaneManager() {
        assertEquals(lm, sd.getLaneManager());
    }

    @Test
    public void testGetMetaData() {
        assertEquals(metaData, sd.getMetaData());
    }

    @Test
    public void testGetSignalManager() {
        assertEquals(sm, sd.getSignalManager());
    }

    @Test
    public void testSetDetectorManager() {
        assertEquals(dm, sd.getDetectorManager());
    }

    @Test
    public void testSetLaneManager() {
        assertEquals(lm, sd.getLaneManager());
    }

    @Test
    public void testSetMetaData() {
        assertEquals(metaData, sd.getMetaData());
    }

    @Test
    public void testSetSignalManager() {
        assertEquals(sm, sd.getSignalManager());
    }

}
