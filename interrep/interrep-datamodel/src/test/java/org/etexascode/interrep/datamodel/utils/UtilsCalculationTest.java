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
package org.etexascode.interrep.datamodel.utils;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.awt.Polygon;
import java.awt.geom.Area;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.Test;

public class UtilsCalculationTest {

    private final double TOLERANCE = 0.005;

    public UtilsCalculationTest() {
        super();
    }

    private boolean closeEnough(double x, double y) {
        return x - y <= 0.05 && y - x <= 0.05;
    }

    public void testCalcDistToStopLine() {
        Lane l = new Lane();
        LaneNode node = new LaneNode();

        node.setX(50);
        node.setY(55);
        node.setZ(35);
        node.setWidth(15511523);

        l.addLaneNode(node);
        ILane info = l;

        Vehicle v = new Vehicle(2, 180, 80, 61, 55, 35);
        v.setLaneID(4);

        IVehicle vInfo = v;

        assertTrue(closeEnough(-1.0,
                UtilsCalculations.calcDistToStopLine(null, null)));

        assertTrue(closeEnough(-1.0,
                UtilsCalculations.calcDistToStopLine(null, info)));

        assertTrue(closeEnough(-1.0,
                UtilsCalculations.calcDistToStopLine(vInfo, null)));

        assertTrue(closeEnough(11.0,
                UtilsCalculations.calcDistToStopLine(vInfo, info)));
    }

    public void testGetBaseNode() {
        Lane l = new Lane();
        try {
            UtilsCalculations.getBaseNode(l);
            fail();
        }
        catch (RuntimeException re) {
            assertTrue(true);
        }

        LaneNode node = new LaneNode();

        node.setX(50);
        node.setY(55);
        node.setZ(35);
        node.setWidth(15511523);

        Lane l2 = new Lane();

        l2.addLaneNode(node);

        assertTrue(node.equals(UtilsCalculations.getBaseNode(l2)));
    }

    @Test
    public void testAreaIsOverDetector() {
        Polygon p = new Polygon();
        p.addPoint(0, 0);
        p.addPoint(10, 0);
        p.addPoint(10, 10);
        p.addPoint(0, 10);
        Detector d = new Detector();
        d.setArea(p);

        Polygon overlaps = new Polygon();
        overlaps.addPoint(5, 0);
        overlaps.addPoint(10, 0);
        overlaps.addPoint(10, 10);
        overlaps.addPoint(5, 10);

        Polygon disjoint = new Polygon();
        disjoint.addPoint(11, 11);
        disjoint.addPoint(21, 11);
        disjoint.addPoint(21, 21);
        disjoint.addPoint(11, 21);

        assertTrue(UtilsCalculations.areaIsOverDetector(new Area(overlaps), d));
        assertFalse(UtilsCalculations.areaIsOverDetector(new Area(disjoint), d));
    }

    @Test
    public void testGetDistToStopLine() {
        Lane l1 = new Lane();
        l1.getLaneGeomList().add(new LaneNode(0, 0));

        Vehicle v1 = new Vehicle(3, 180, 80, 3.0, 4.0, 0.0);

        Lane l2 = new Lane();
        l2.getLaneGeomList().add(new LaneNode(237.5, 122.7));

        Vehicle v2 = new Vehicle(4, 180, 80, 456.7, 567.8, 0.0);

        assertEquals(-1, UtilsCalculations.getDistance(v1, null), TOLERANCE);
        assertEquals(5, UtilsCalculations.getDistance(v1, l1), TOLERANCE);
        assertEquals(496.147, UtilsCalculations.getDistance(v2, l2), TOLERANCE);
    }

    @Test
    public void testGenPolygonFromVehicle() {
        // 10x10 car heading west with front center at the origin.
        Vehicle v1 = new Vehicle(3, 10.0, 10.0, 0.0, 0.0, 0.0);
        double[] heading1 = { 0.0, -10.0 };

        Polygon actual1 = UtilsCalculations.genPolygonFromVehicle(v1, heading1);
        int[] expectedx1 = { 0, 0, 10, 10 };
        int[] expectedy1 = { -5, 5, 5, -5 };

        // 8sqrt(2) x 40 car heading exactly northeast with front center at (50,50)
        Vehicle v2 = new Vehicle(4, (40 * Math.sqrt(2)), (8 * Math.sqrt(2)), 50.0, 50.0, 0.5);

        Vehicle prev2 = new Vehicle(4, (40 * Math.sqrt(2)), (8 * Math.sqrt(2)), 10.0, 10.0, 0.5);

        double[] heading2 = { 40.0, 40.0 };

        Polygon actual2 = UtilsCalculations.genPolygonFromVehicle(v2, heading2);
        int[] expectedx2 = { 46, 54, 14, 6 };
        int[] expectedy2 = { 54, 46, 6, 14 };

        assertArrayEquals(expectedx1, actual1.xpoints);
        assertArrayEquals(expectedy1, actual1.ypoints);
        assertArrayEquals(expectedx2, actual2.xpoints);
        assertArrayEquals(expectedy2, actual2.ypoints);
    }

    @Test
    public void testMoveAlongAxis() {
        double[] origin = { 5.5, 4.4 };
        assertArrayEquals(origin, UtilsCalculations.moveAlongAxis(origin, 0, 0, 20), TOLERANCE);
    }

    @Test
    public void testGenVehicleSpeed() {
        Vehicle v1 = new Vehicle(1, 10, 10, 35, 35, 0.0);

        Vehicle v2 = new Vehicle(2, 10, 10, 15, 15, 0.0);

        double timeStepInterval = 12;
        double speedtest = (UtilsCalculations.getDist(35, 35, 15, 15) / timeStepInterval);
        v1.setSpeed(UtilsCalculations.genSpeed(v1, v2, timeStepInterval));
        assertTrue(speedtest == v1.getSpeed());
    }

}
