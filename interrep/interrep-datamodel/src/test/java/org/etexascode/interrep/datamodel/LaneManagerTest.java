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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Uses /src/main/resources/lanemanager.xml
 * 
 * @author bmauldon
 * @author ablatt
 */
public class LaneManagerTest {

    /** read lane manager from xmlfile */
    LaneManager lm = JaxbRead.readJaxManager("/lanemanager.xml", LaneManager.class);

    LaneManager lmtest = JaxbRead.readJaxManager("/lanemanager.xml", LaneManager.class);

    double delta = 0.00001;

    double latitude = 37.89760;

    double longitude = 10.00876;

    double elevation = 200.0;

    /** New Lane map */
    Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();

    Lane value = lm.getLanes().get(4);

    @Before
    public void setUp() throws Exception {
        lanes.put(4, value);
    }

    @After
    public void tearDown() throws Exception {
        lanes.clear();
    }

    @Test
    public void testToString() {
        LaneManager manager = new LaneManager();
        assertEquals("latitude = 0.0\nlongitude = 0.0\nelevation = 0.0\n", manager.toString());
    }

    @Test
    public void testEqualsLaneManager1() {
        assertEquals(lm, lmtest);
        lmtest.setLatitude(.1);
        assertNotEquals(lm, lmtest);
        lmtest.setLatitude(0);
        lmtest.setLongitude(.1);
        assertNotEquals(lm, lmtest);
        lmtest.setLongitude(0);
        lmtest.setElevation(.1);
        assertNotEquals(lm, lmtest);
        lmtest.getLanes().clear();
        lmtest.setElevation(0);
        assertNotEquals(lm, lmtest);
        lm.getLanes().clear();
        lm.getLanes().put(1, new Lane());
        assertNotEquals(lm, lmtest);
        assertFalse(lm.equals(new String()));
    }

    @Test
    public void testEqualsLaneManager2() {
        lm.getLanes().put(1, new Lane());
        Lane diffL = new Lane();
        diffL.setType("differentLane");
        lmtest.getLanes().put(1, diffL);
        assertFalse(lm.equals(lmtest));
        lmtest.getLanes().clear();
        lmtest.getLanes().put(2, diffL);
        assertFalse(lm.equals(lmtest));
    }

    @Test
    public void testGetLatitude() {
        assertEquals(0.0, lm.getLatitude(), delta);

    }

    @Test
    public void testSetLatitude() {
        lm.setLatitude(latitude);
        assertEquals(latitude, lm.getLatitude(), delta);
    }

    @Test
    public void testGetLongitude() {
        assertEquals(0.0, lm.getLongitude(), delta);
    }

    @Test
    public void testSetLongitude() {
        lm.setLongitude(longitude);
        assertEquals(longitude, lm.getLongitude(), delta);
    }

    @Test
    public void testGetElevation() {
        assertEquals(0.0, lm.getElevation(), delta);
    }

    @Test
    public void testSetElevation() {
        lm.setElevation(elevation);
        assertEquals(elevation, lm.getElevation(), delta);

    }

    @Test
    public void testGetLanes() {
        lm.setLanes(lanes);
        assertEquals(lanes, lm.getLanes());
    }

    @Test
    public void testSetLanes() {
        lm.setLanes(lanes);
        assertEquals(lanes, lm.getLanes());
    }

    @Test
    public void testGetLaneById() {
        assertEquals(value, lm.getLaneById(4));
    }

    @Test
    public void testGetGeoCalculatorType() {
        LaneManager lm = new LaneManager();
        lm.setGeoCalculatorType(2);
        assertEquals(2, lm.getGeoCalculatorType());
    }

    @Test
    public void testIterator() {
        Collection<Lane> tmp = lm.getLanes().values();
        int i = 0;
        for (Lane l : lm.getIterable()) {
            assertTrue(tmp.contains(l));
            i++;
        }
        assertEquals(i, tmp.size());
    }

    @Test
    public void testEqualsId1() {
        LaneManager lm = new LaneManager();
        lm.setIntersectionId(1729);
        LaneManager lm2 = new LaneManager();
        lm2.setIntersectionId(1729);
        assertTrue(lm.equalsId(lm2));
    }

    @Test
    public void testEqualsId2() {
        LaneManager lm = new LaneManager();
        lm.setIntersectionId(1729);
        assertFalse(lm.equalsId(new IDable() {

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
        LaneManager lm = new LaneManager();
        lm.setIntersectionId(0);

        LaneManager diffLm = new LaneManager();
        diffLm.setIntersectionId(1);

        assertFalse(lm.equalsId(diffLm));
    }

    @Test
    public void testGetProperId() {
        LaneManager lm = new LaneManager();
        lm.setIntersectionId(1729);
        assertEquals("LaneManager:1729", lm.getProperId());
    }
}
