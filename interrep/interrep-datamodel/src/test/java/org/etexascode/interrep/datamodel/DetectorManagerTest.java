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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * test uses detectormanager.xml found in /src/main/resources
 * 
 * @author bmauldon uses
 */
public class DetectorManagerTest {

    /** Read DetectorManager from xml file */
    DetectorManager dm = JaxbRead.readJaxManager("/detectormanager.xml", DetectorManager.class);

    DetectorManager dmtest = JaxbRead.readJaxManager("/detectormanager.xml", DetectorManager.class);

    /** Get last detector in manager, number 22 create 23 */
    Detector twentytwo = dm.getDetector(6);

    Detector twentythree = dm.getDetector(3);

    /** Map of detectors */
    Map<Integer, Detector> detectors = new HashMap<Integer, Detector>();

    /** Keys */
    Set<Integer> keys = new TreeSet<Integer>();

    Integer key[] = { 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 16, 19, 18, 21, 20, 22 };

    /** Empty map */
    Map<Integer, Detector> empty = new HashMap<Integer, Detector>();

    @Before
    public void setUp() throws Exception {
        twentythree.setDetectorID(23);
        keys.addAll(Arrays.asList(key));
    }

    @After
    public void tearDown() throws Exception {
        dmtest = null;
    }

    @Test
    public void testAddDetector() {
        dmtest.addDetector(23, twentythree);
        assertEquals(twentythree, dmtest.getDetector(23));
    }

    @Test
    public void testClearDetectors() {
        dm.clearDetectors();
        assertTrue(dm.getKeys().isEmpty());
    }

    @Test
    public void testEqualsDetectorManager() {
        assertNotEquals(dm, dmtest);
        dmtest.clearDetectors();
        dm.clearDetectors();
        dmtest.addDetector(-1, twentythree);
        dm.addDetector(1, twentythree);
        assertNotEquals(dm, dmtest);
        assertFalse(dm.equals(new String()));

    }

    @Test
    public void testGetDetector() {
        dmtest.addDetector(23, twentythree);
        assertEquals(twentythree, dmtest.getDetector(23));
    }

    @Test
    public void testGetDetectorCollection() {
        DetectorManager man = new DetectorManager();
        man.addDetector(22, twentytwo);
        man.addDetector(23, twentythree);
        Collection<Detector> collect = man.getDetectorCollection();
        assertEquals(2, collect.size());
        assertTrue(collect.contains(twentytwo));
        assertTrue(collect.contains(twentythree));
    }

    @Test
    public void testGetKeys() {
        assertEquals(keys, dm.getKeys());
    }

    @Test
    public void testToString() {
        assertEquals(dm.toString(), dm.toString());
    }

    @Test
    public void testIterator() {
        DetectorManager man = new DetectorManager();
        man.addDetector(22, twentytwo);
        man.addDetector(23, twentythree);
        Set<Detector> exp = new HashSet<Detector>();
        exp.add(twentytwo);
        exp.add(twentythree);

        for (Detector d : man.getIterable()) {
            assertTrue(exp.contains(d));
            exp.remove(d);
        }

        assertEquals(0, exp.size());
    }

    @Test
    public void testGetDetectorFromLaneId() {
        DetectorManager testDm = new DetectorManager();

        Detector d = new Detector();
        List<Integer> laneIds = new ArrayList<Integer>(1);
        int laneId = 8;
        laneIds.add(laneId);
        d.setDetectorID(1);
        d.setLaneIDs(laneIds);

        testDm.addDetector(d.getDetectorID(), d);

        Detector diffLaneD = new Detector();
        List<Integer> diffLaneIds = new ArrayList<Integer>(1);
        int diffLaneId = 5;
        diffLaneIds.add(diffLaneId);
        diffLaneD.setDetectorID(2);
        diffLaneD.setLaneIDs(diffLaneIds);

        testDm.addDetector(diffLaneD.getDetectorID(), diffLaneD);

        @SuppressWarnings("unchecked")
        List<IDetector> detectors = (List<IDetector>)testDm.getDetectorFromLaneId(laneId);

        assertTrue(detectors.size() == 1);
        assertTrue(d.equals(detectors.get(0)));
    }
}
