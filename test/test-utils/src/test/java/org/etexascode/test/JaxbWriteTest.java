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
package org.etexascode.test;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.Iterator;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test class for writing (marshalling) Jaxb objects. Test for simple functionality now. After
 * reading (unmarshalling) in interrep.datamodels will test equality of objects
 * 
 * @author bmauldon
 */
public class JaxbWriteTest {

    /** Create TestHarnessCase using playback data, get data model Managers */
    TestHarnessCase thc = new TestHarnessCase();

    LaneManager lm = thc.getLaneManager();

    VehicleManager vm = thc.getVehicleManager();

    DetectorManager dm = thc.getDetectorManager();

    SignalManager sm = thc.getSignalManager();

    /** Xml file names */
    String laneManagerXml = "lanemanager.xml";

    String detectorManagerXml = "detectormanager.xml";

    String vehicleManagerXml = "vehiclemanager.xml";

    String signalManagerXml = "signalmanager.xml";

    File file = null;

    @Before
    public void setUp() throws Exception {
        /** Write managers to files */
        JaxbWrite.writeJaxbManager(dm, detectorManagerXml, DetectorManager.class);
        JaxbWrite.writeJaxbManager(vm, vehicleManagerXml, VehicleManager.class);
        JaxbWrite.writeJaxbManager(sm, signalManagerXml, SignalManager.class);
        JaxbWrite.writeJaxbManager(lm, laneManagerXml, LaneManager.class);
        /** Read managers back in from the xmlfiles in the test methods */
    }

    @After
    public void tearDown() throws Exception {
        file = null;
    }

    @Test
    public void testWriteDetectorManager() {
        file = new File(detectorManagerXml);
        DetectorManager dmx = JaxbRead.readJaxManager(file, DetectorManager.class);

        assertEquals(dm, dmx);
    }

    @Test
    public void testWriteJaxbSignallManager() {
        file = new File(signalManagerXml);
        SignalManager smx = JaxbRead.readJaxManager(file, SignalManager.class);
        assertEquals(sm, smx);
    }

    @Test
    public void testWriteLaneManager() {
        file = new File(laneManagerXml);
        LaneManager lmx = JaxbRead.readJaxManager(file, LaneManager.class);
        assertEquals(lm, lmx);
    }

    @Test
    public void testWriteVehicleManager() {
        file = new File(vehicleManagerXml);
        VehicleManager vmx = JaxbRead.readJaxManager(file, VehicleManager.class);
        assertEquals(vm, vmx);
    }
}
