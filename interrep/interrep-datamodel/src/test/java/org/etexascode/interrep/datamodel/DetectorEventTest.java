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

import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 * @author ablatt
 */
public class DetectorEventTest {

    /** Read DetectorManager from xml file */
    DetectorManager dm = JaxbRead.readJaxManager("/detectormanager.xml", DetectorManager.class);

    /** detector event -hardcoded */
    Detector detector = dm.getDetector(1);

    DetectorEvent dv = new DetectorEvent();

    double length = 80;

    int dvId = 1001;

    int pulse = 3;

    boolean presence = true;

    double speed = 15.64;

    double delta = 0.00001;

    @Before
    public void setUp() throws Exception {
        /** use sets to fill out detector event - set tests can compare gets */
        dv.setDetectorId(dvId);
        dv.setLength(length);
        dv.setPresence(presence);
        dv.setPulse(pulse);
        dv.setSpeed(speed);

    }

    @After
    public void tearDown() throws Exception {
        dv = null;
    }

    @Test
    public void testEqualsDetectorEvent1() {
        DetectorEvent dvtoo = dv;
        assertEquals(dv, dvtoo);
        Assert.assertNotEquals(dv, new String());
    }

    @Test
    public void testEqualsDetectorEvent2() {
        DetectorEvent dvtoo = new DetectorEvent();
        dvtoo.setDetectorId(dv.detectorId + 1);
        dvtoo.setLength(dv.length);
        dvtoo.setPresence(dv.presence);
        dvtoo.setPulse(dv.pulse);
        dvtoo.setSpeed(dv.speed);
        Assert.assertNotEquals(dv, dvtoo);
    }

    @Test
    public void testEqualsDetectorEvent3() {
        DetectorEvent dvtoo = new DetectorEvent();
        dvtoo.setDetectorId(dv.detectorId);
        dvtoo.setLength(dv.length * 4);
        dvtoo.setPresence(dv.presence);
        dvtoo.setPulse(dv.pulse);
        dvtoo.setSpeed(dv.speed);
        Assert.assertNotEquals(dv, dvtoo);
    }

    @Test
    public void testEqualsDetectorEvent4() {
        DetectorEvent dvtoo = new DetectorEvent();
        dvtoo.setDetectorId(dv.detectorId);
        dvtoo.setLength(dv.length);
        dvtoo.setPresence(!dv.presence);
        dvtoo.setPulse(dv.pulse);
        dvtoo.setSpeed(dv.speed);
        Assert.assertNotEquals(dv, dvtoo);
    }

    @Test
    public void testEqualsDetectorEvent5() {
        DetectorEvent dvtoo = new DetectorEvent();
        dvtoo.setDetectorId(dv.detectorId);
        dvtoo.setLength(dv.length);
        dvtoo.setPresence(dv.presence);
        dvtoo.setPulse(dv.pulse * 4);
        dvtoo.setSpeed(dv.speed);
        Assert.assertNotEquals(dv, dvtoo);
    }

    @Test
    public void testEqualsDetectorEvent6() {
        DetectorEvent dvtoo = new DetectorEvent();
        dvtoo.setDetectorId(dv.detectorId);
        dvtoo.setLength(dv.length);
        dvtoo.setPresence(dv.presence);
        dvtoo.setPulse(dv.pulse);
        dvtoo.setSpeed(dv.speed * 4);
        Assert.assertNotEquals(dv, dvtoo);
    }

    @Test
    public void testGetDetectorId() {
        assertEquals(dvId, dv.getDetectorId());
    }

    @Test
    public void testGetLength() {
        assertEquals(length, dv.getLength(), delta);
    }

    @Test
    public void testGetPulse() {
        assertEquals(pulse, dv.getPulse());
    }

    @Test
    public void testGetSpeed() {
        assertEquals(speed, dv.getSpeed(), delta);
    }

    @Test
    public void testIsPresence() {
        assertEquals(presence, dv.isPresence());
    }

    @Test
    public void testSetDetectorId() {
        assertEquals(dvId, dv.getDetectorId());
    }

    @Test
    public void testSetLength() {
        assertEquals(length, dv.getLength(), delta);
    }

    @Test
    public void testSetPresence() {
        assertEquals(presence, dv.isPresence());
    }

    @Test
    public void testSetPulse() {
        assertEquals(pulse, dv.getPulse());
    }

    @Test
    public void testSetSpeed() {
        assertEquals(speed, dv.getSpeed(), delta);
    }

    @Test
    public void testToString() {
        assertEquals(dv.toString(), dv.toString());
    }

    @Test
    public void testEqualsId1() {
        assertTrue(dv.equalsId(dv));
    }

    @Test
    public void testEqualsId2() {
        assertFalse(dv.equalsId(new IDable() {

            @Override
            public boolean equalsId(IDable entity) {
                return false;
            }

            @Override
            public String getProperId() {
                return null;
            }
        }));
    }

    @Test
    public void testEqualsId3() {
        DetectorEvent testDv = new DetectorEvent();
        testDv.setDetectorId(dv.detectorId + 1);
        assertFalse(dv.equalsId(testDv));
    }

    @Test
    public void testGetProperId() {
        assertEquals("DetectorEvent:1001", dv.getProperId());
    }
}
