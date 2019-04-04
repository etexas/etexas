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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 */
public class LaneTest {

    /** Read LaneManager from xml file, get Lane 7 */
    LaneManager lm = JaxbRead.readJaxManager("/lanemanager.xml", LaneManager.class);

    Map<Integer, LaneMovement> lmMap = new HashMap<Integer, LaneMovement>();

    Lane lane7 = null;

    LaneMovement lmove = new LaneMovement();

    /** get test lane node and lane movement */
    double x = 37.869;

    double y = 65.789;

    double speed = 15.64;

    double delta = 0.00001;

    String type = "Unset";

    /** empty lane objects */
    Lane lane7too = null;

    Lane testLane = null;

    LaneNode node = new LaneNode(x, y);

    List<LaneNode> nodelist = new ArrayList<LaneNode>();

    LaneMovement laneM = new LaneMovement();

    Set<Lane> iterExp = null;

    @Before
    public void setUp() throws Exception {
        /** set lane and lane movement */
        lane7 = lm.getLaneById(7);
        lane7too = lm.getLaneById(7);
        testLane = new Lane(lm.getLaneById(1));
        nodelist.add(node);
        laneM.setMovementId(3);
        lmMap.put(999, laneM);
        iterExp = new HashSet<Lane>();
    }

    @After
    public void tearDown() throws Exception {
        lane7 = null;
        nodelist.clear();

    }

    @Test
    public void testAddLaneNode() {
        lane7.addLaneNode(node);
        assertTrue(lane7.getLaneGeomList().contains(node));
    }

    @Test
    public void testEqualsLane1() {
        assertEquals(lane7, lane7too);
        assertNotEquals(lane7, testLane);
        testLane.setLaneId(7);
        testLane.addLaneNode(node);
        assertNotEquals(lane7, testLane);
        lane7.addLaneNode(node);
        lane7.getLaneMovements().keySet().clear();
        testLane.getLaneMovements().keySet().clear();
        testLane.getLaneMovements().put(1, new LaneMovement());
        assertNotEquals(lane7, testLane);
        lane7.laneGeomList.add(new LaneNode());
        testLane.getLaneMovements().put(1, new LaneMovement());
        assertNotEquals(lane7, testLane);
        assertFalse(lane7.equals(new String()));
    }

    @Test
    public void testEqualsLane2() {
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        l.setIntersectionId(0);

        Lane diffL = new Lane();
        diffL.setLaneId(10);
        diffL.setApproachId(12);
        diffL.setIntersectionId(1);
        assertFalse(l.equals(diffL));

        diffL.setType("DifferentType");
        assertFalse(l.equals(diffL));

        diffL.setApproachId(24);
        assertFalse(l.equals(diffL));

        diffL.setLaneId(10);
        assertFalse(l.equals(diffL));
    }

    @Test
    public void testEqualsLane3() {
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        l.setIntersectionId(0);

        Lane diffL = new Lane();
        diffL.setLaneId(10);
        diffL.setApproachId(12);
        diffL.setIntersectionId(0);

        l.addLaneNode(new LaneNode(1, 2));
        assertFalse(l.equals(diffL));

        diffL.addLaneNode(new LaneNode(2, 4));
        assertFalse(l.equals(diffL));
    }

    @Test
    public void testEqualsLane4() {
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        l.setIntersectionId(0);

        Lane diffL = new Lane();
        diffL.setLaneId(10);
        diffL.setApproachId(12);
        diffL.setIntersectionId(0);
        Map<Integer, LaneMovement> laneMoveMap = new HashMap<Integer, LaneMovement>(1);
        Map<Integer, LaneMovement> laneMoveMap2 = new HashMap<Integer, LaneMovement>(1);
        LaneMovement laneMove = new LaneMovement();
        laneMove.setMovement(Movement.LEFT_TURN);
        LaneMovement diffLaneMove = new LaneMovement();
        diffLaneMove.setMovement(Movement.STRAIGHT);

        laneMoveMap.put(1, laneMove);
        laneMoveMap2.put(1, diffLaneMove);
        l.setLaneMovements(laneMoveMap);
        diffL.setLaneMovements(laneMoveMap2);
        assertFalse(l.equals(diffL));

        laneMoveMap2.clear();
        laneMoveMap2.put(2, diffLaneMove);
        diffL.setLaneMovements(laneMoveMap2);
        assertFalse(l.equals(diffL));
    }

    @Test
    public void testGetApproachId() {
        assertEquals(7, lane7.getApproachId());
    }

    @Test
    public void testGetIntersectionId() {
        assertEquals(0, lane7.getIntersectionId());
    }

    @Test
    public void testGetLaneGeomList() {
        lane7.setLaneGeomList(nodelist);
        assertEquals(nodelist, lane7.getLaneGeomList());
    }

    @Test
    public void testGetLaneId() {
        assertEquals(7, lane7.getLaneId());
    }

    @Test
    public void testGetLaneMovements() {
        lane7.setLaneMovements(lmMap);
        assertEquals(lmMap, lane7.getLaneMovements());

    }

    @Test
    public void testGetSpeedLimitInMetersPerSecond() {
        lane7.setSpeedLimitInMetersPerSecond(speed);
        assertEquals(speed, lane7.getSpeedLimitInMetersPerSecond(), delta);

    }

    @Test
    public void testGetType() {
        lane7.setType(type);
        assertEquals(type, lane7.getType());
    }

    @Test
    public void testSetApproachId() {
        lane7.setApproachId(22);
        assertEquals(22, lane7.getApproachId());
    }

    @Test
    public void testSetIntersectionId() {
        lane7.setIntersectionId(1);
        assertEquals(1, lane7.getIntersectionId());
    }

    @Test
    public void testSetLaneGeomList() {
        lane7.setLaneGeomList(nodelist);
        assertEquals(nodelist, lane7.getLaneGeomList());
    }

    @Test
    public void testSetLaneId() {
        lane7.setLaneId(9);
        assertEquals(9, lane7.getLaneId());
    }

    @Test
    public void testSetLaneMovements() {
        lane7.setLaneMovements(lmMap);
        assertEquals(lmMap, lane7.getLaneMovements());
    }

    @Test
    public void testSetSpeedLimitInMetersPerSecond() {
        lane7.setSpeedLimitInMetersPerSecond(speed);
        assertEquals(speed, lane7.getSpeedLimitInMetersPerSecond(), delta);
    }

    @Test
    public void testSetType() {
        lane7.setType(type);
        assertEquals(type, lane7.getType());
    }

    @Test
    public void testToString() {
        assertEquals(lane7.toString(), lane7.toString());
    }

    @Test
    public void testIterator() {
        Lane l = lm.getLanes().values().iterator().next();
        int i = 0;
        for (LaneNode ln : l.getIterable()) {
            assertTrue(l.laneGeomList.contains(ln));
            i++;
        }

        assertEquals(i, l.laneGeomList.size());
    }

    /*
     * @Test public void testIterator() { Collection<Lane> tmp = lm.getLanes().values(); for(Lane
     * l:lm) { assertTrue(tmp.contains(l)); } }
     */

    @Test
    public void testLanMovIter() {
        Lane l = lm.getLanes().values().iterator().next();
        Collection<LaneMovement> coll = l.getLaneMovements().values();
        int i = 0;
        for (LaneMovement ln : l.lanMovIterator()) {
            assertTrue(coll.contains(ln));
            i++;
        }

        assertEquals(i, coll.size());
    }

    @Test
    public void testEqualsId1() {
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        l.setIntersectionId(0);
        assertTrue(l.equalsId(l));
    }

    @Test
    public void testEqualsId2() {
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        assertFalse(l.equalsId(new IDable() {

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
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        l.setIntersectionId(0);

        Lane diffL = new Lane();
        diffL.setLaneId(10);
        diffL.setApproachId(12);
        diffL.setIntersectionId(1);

        assertFalse(l.equalsId(diffL));

        diffL.setApproachId(24);
        assertFalse(l.equalsId(diffL));

        diffL.setLaneId(20);
        assertFalse(l.equalsId(diffL));
    }

    @Test
    public void testGetProperId() {
        Lane l = new Lane();
        l.setLaneId(10);
        l.setApproachId(12);
        assertEquals("Lane:10:12:0", l.getProperId());
    }

    @Test
    public void testGetCoordinates() {
        Lane l = new Lane();
        double x = 5;
        double y = 12;
        double z = 20;
        double width = 100;
        l.addLaneNode(new LaneNode(x, y, z, width));
        assertTrue(x == l.getX());
        assertTrue(y == l.getY());
        assertTrue(z == l.getZ());
    }
}
