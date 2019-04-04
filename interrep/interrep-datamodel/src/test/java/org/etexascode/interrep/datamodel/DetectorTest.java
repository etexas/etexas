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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.awt.Polygon;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tested using these values for detector one
 * 
 * @author bmauldon
 *         <value xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type ="detector">
 *         <detectorManager> <detectors> <entry> <key>6</key>
 *         <value xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type ="detector">
 *         <detectorID>6</detectorID> <laneIDs>6</laneIDs>
 *         <presenceDetectCap>true</presenceDetectCap> <pulseDetectCap>false</pulseDetectCap>
 *         <speedDetectCap>true</speedDetectCap> <lengthDetectCap>true</lengthDetectCap> <area>
 *         <npoints>4</npoints> <xpoints>6642</xpoints> <xpoints>6642</xpoints>
 *         <xpoints>6947</xpoints> <xpoints>6947</xpoints> <ypoints>1988</ypoints>
 *         <ypoints>6947</ypoints> <ypoints>6947</ypoints> <ypoints>1988</ypoints> </area> </value>
 *         </entry> </detectors> </detectorManager>
 */
public class DetectorTest {

    /** Read DetectorManager from xml file */
    File file = FileUtils.toFile(this.getClass().getResource("/detectormanager.xml"));

    DetectorManager dm = JaxbRead.readJaxManager(file, DetectorManager.class);

    /** detector from map */
    Detector detector = dm.getDetector(6);

    Detector detector3 = dm.getDetector(3);

    Detector condDetector = dm.getDetector(6);

    /** Detector event and attributes */
    DetectorEvent de = new DetectorEvent();

    double length = 80;

    int dvId = 1001;

    int pulse = 3;

    boolean presence = true;

    double speed = 15.64;

    double delta = 0.00001;

    /** Lane List in detector one */
    List<Integer> lanesOne = new ArrayList<Integer>();

    /** x,y npoints for polygon detector 3 */
    int x[] = { 606, 606, 911, 911 };

    int y[] = { 5212, 5516, 5516, 5212 };

    int npoints = 4;

    int xnew[] = { 5000, 5200, 4800 };

    int ynew[] = { 4000, 5000, 4000 };

    Polygon area = new Polygon(x, y, npoints);

    Polygon areaNew = new Polygon(xnew, ynew, 3);

    Detector getXYDet = null;

    @Before
    public void setUp() throws Exception {
        lanesOne.add(6);

        getXYDet = new Detector();
        Polygon p = new Polygon();

        for (int i = 0; i < npoints; i++) {
            p.addPoint(x[i], y[i]);
        }

        getXYDet.setArea(p);
        // detector event for detector 6
        de.setDetectorId(6);
        de.setPulse(1);
        de.setPresence(true);
        de.setSpeed(0);
        de.setLength(426.72);

    }

    @After
    public void tearDown() throws Exception {
        lanesOne.clear();
        getXYDet = null;
    }

    @Test
    public void testCloneArea() {
        Polygon clone = detector.cloneArea();
        assertTrue(UtilsSpecialEquals.equals(detector.getArea(), clone));

        clone.addPoint(0, 1);
        assertFalse(UtilsSpecialEquals.equals(detector.getArea(), clone));
    }

    @Test
    public void testEqualsDetectorCond1() {
        condDetector = dm.getDetector(3);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond2() {
        condDetector = dm.getDetector(3);
        condDetector.setDetectorID(6);
        condDetector.setPresenceDetectCap(false);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond3() {
        condDetector = dm.getDetector(3);
        condDetector.setDetectorID(6);
        condDetector.setPulseDetectCap(true);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond4() {
        condDetector = dm.getDetector(3);
        condDetector.setDetectorID(6);
        condDetector.setSpeedDetectCap(false);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond5() {
        condDetector = dm.getDetector(3);
        condDetector.setDetectorID(6);
        condDetector.setLengthDetectCap(false);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond6() {
        condDetector = dm.getDetector(3);
        condDetector.getLaneIDs().add(-1);
        condDetector.setDetectorID(6);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond7() {
        condDetector = dm.getDetector(3);
        condDetector.setDetectorID(6);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond8() {
        condDetector = dm.getDetector(3);
        detector.getLaneIDs().clear();
        condDetector.getLaneIDs().clear();
        condDetector.setDetectorID(6);
        condDetector.setArea(areaNew);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond10() {
        condDetector = dm.getDetector(3);
        detector = dm.getDetector(6);
        detector.getLaneIDs().clear();
        condDetector.getLaneIDs().clear();
        detector.getLaneIDs().add(1);
        condDetector.getLaneIDs().add(1);
        condDetector.setDetectorID(6);
        condDetector.setDetEvent(null);
        detector.setDetEvent(de);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond11() {
        condDetector = dm.getDetector(3);
        detector = dm.getDetector(6);
        detector.getLaneIDs().clear();
        condDetector.getLaneIDs().clear();
        detector.getLaneIDs().add(1);
        condDetector.getLaneIDs().add(1);
        condDetector.setDetectorID(6);
        condDetector.setDetEvent(de);
        detector.setDetEvent(null);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetectorCond12() {
        condDetector = dm.getDetector(3);
        detector = dm.getDetector(6);
        detector.getLaneIDs().clear();
        condDetector.getLaneIDs().clear();
        detector.getLaneIDs().add(1);
        condDetector.getLaneIDs().add(1);
        condDetector.setDetectorID(6);
        condDetector.setDetEvent(new DetectorEvent());
        de.setDetectorId(12);
        detector.setDetEvent(de);
        assertNotEquals(detector, condDetector);
    }

    @Test
    public void testEqualsDetector() {
        assertEquals(detector, condDetector);
        assertNotEquals(detector, new String());
    }

    @Test
    public void testGetArea() {
        assertEquals(area.npoints, detector3.getArea().npoints);
        assertArrayEquals(area.xpoints, detector3.getArea().xpoints);
        assertArrayEquals(area.ypoints, detector3.getArea().ypoints);
    }

    @Test
    public void testGetDetectorID() {
        assertEquals(detector.getDetectorID(), 6);
    }

    @Test
    public void testGetDetEvent() {
        assertEquals(detector.getDetEvent(), de);
    }

    @Test
    public void testGetLaneIDs() {
        assertEquals(detector.getLaneIDs().get(0), lanesOne.get(0));
    }

    @Test
    public void testIsLengthDetectCap() {
        assertEquals(detector.isLengthDetectCap(), true);
    }

    @Test
    public void testIsPresenceDetectCap() {
        assertEquals(detector.isPresenceDetectCap(), true);
    }

    @Test
    public void testIsPulseDetectCap() {
        assertEquals(detector.isPulseDetectCap(), false);
    }

    @Test
    public void testIsSpeedDetectCap() {
        assertEquals(detector.isSpeedDetectCap(), true);
    }

    @Test
    public void testSetArea() {
        detector.setArea(areaNew);
        assertEquals(detector.getArea(), areaNew);
    }

    @Test
    public void testSetDetectorID() {
        detector.setDetectorID(2);
        assertEquals(detector.getDetectorID(), 2);
    }

    @Test
    public void testSetDetEvent() {
        de.setDetectorId(dvId);
        de.setLength(length);
        de.setPresence(presence);
        de.setPulse(pulse);
        de.setSpeed(speed);
        detector.setDetEvent(de);
        assertEquals(detector.getDetEvent(), de);
    }

    @Test
    public void testSetLaneIDs() {
        lanesOne.add(22);
        detector.setLaneIDs(lanesOne);
        assertEquals(detector.getLaneIDs(), lanesOne);
    }

    @Test
    public void testSetLengthDetectCap() {
        detector.setLengthDetectCap(false);
        assertEquals(detector.isLengthDetectCap(), false);
    }

    @Test
    public void testSetPresenceDetectCap() {
        detector.setPresenceDetectCap(false);
        assertEquals(detector.isPresenceDetectCap(), false);
    }

    @Test
    public void testSetPulseDetectCap() {
        detector.setPulseDetectCap(true);
        assertEquals(detector.isPulseDetectCap(), true);
    }

    @Test
    public void testSetSpeedDetectCap() {
        detector.setSpeedDetectCap(false);
        assertEquals(detector.isSpeedDetectCap(), false);
    }

    @Test
    public void testToString() {
        assertEquals(detector.toString(), detector.toString());
    }

    @Test
    public void testGetX() {
        double expected = 0.0;
        for (int i = 0; i < npoints; i++) {
            expected += x[i];
        }

        expected /= npoints;

        assertEquals(expected, getXYDet.getX(), 0.05);
    }

    @Test
    public void testGetY() {
        double expected = 0.0;
        for (int i = 0; i < npoints; i++) {
            expected += y[i];
        }

        expected /= npoints;

        assertEquals(expected, getXYDet.getY(), 0.05);
    }

    @Test
    public void testEqualsId1() {
        Detector d = new Detector();
        d.setDetectorID(56);
        assertTrue(d.equalsId(d));
    }

    @Test
    public void testEqualsId2() {
        Detector d = new Detector();
        d.setDetectorID(56);
        assertFalse(d.equalsId(new IDable() {

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
        Detector d = new Detector();
        Detector testD = new Detector();
        d.setDetectorID(23);
        testD.setDetectorID(5);
        assertFalse(d.equalsId(testD));
    }

    @Test
    public void testGetProperId() {
        Detector d = new Detector();
        d.setDetectorID(56);
        assertEquals("Detector:56", d.getProperId());
    }

    @Test
    public void testCloneDetector1() {
        Detector d = Detector.cloneDetector(detector);
        assertTrue(detector.equals(d));
    }

    @Test
    public void testCloneDetector2() {
        Detector d = Detector.cloneDetector(detector);

        Polygon poly = detector.getArea();
        Polygon testPoly = d.getArea();
        assertTrue(poly.npoints == testPoly.npoints);
        assertTrue(Arrays.equals(poly.xpoints, testPoly.xpoints));
        assertTrue(Arrays.equals(poly.ypoints, testPoly.ypoints));

        assertTrue(detector.getDetectorID() == d.getDetectorID());
        assertTrue(detector.getDetEvent().equals(d.getDetEvent()));

        List<Integer> lanes = detector.getLaneIDs();
        List<Integer> testLanes = d.getLaneIDs();
        if (lanes.size() != testLanes.size()) {
            fail("The cloned detector doesn't have the same amount of lanes");
        }
        for (int i = 0; i < lanes.size(); i++) {
            assertTrue(lanes.get(i).intValue() == testLanes.get(0).intValue());
        }

        assertTrue(detector.getProperId().equals(d.getProperId()));
        assertTrue(detector.getX() == d.getX());
        assertTrue(detector.getY() == d.getY());
        assertTrue(detector.getZ() == d.getZ());
    }

    @Test
    public void testCloneDetector3() {
        Detector d = Detector.cloneDetector(null);
        assertTrue(d == null);
    }
}
