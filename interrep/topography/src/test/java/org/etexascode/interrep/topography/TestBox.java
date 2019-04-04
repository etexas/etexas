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
package org.etexascode.interrep.topography;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class TestBox {

    Box box;

    double xMin = 0;

    double xMax = 15;

    double yMin = 5;

    double yMax = 20;

    double zMin = 10;

    double zMax = 25;

    double xMiddle = (xMin + xMax) / 2;

    double yMiddle = (yMin + yMax) / 2;

    double zMiddle = (zMin + zMax) / 2;

    @Before
    public void setup() {

        box = new Box(xMin, yMin, zMin, xMax, yMax, zMax);
    }

    @After
    public void teardown() {

        box = null;
    }

    @Test
    public void testBoxConstructor1() {

        Box box = new Box(xMin, yMin, zMin, xMax, yMax, zMax);

        assertTrue(box.getXMin() == xMin);
        assertTrue(box.getXMax() == xMax);
        assertTrue(box.getYMin() == yMin);
        assertTrue(box.getYMax() == yMax);
        assertTrue(box.getZMin() == zMin);
        assertTrue(box.getZMax() == zMax);
    }

    @Test
    public void testBoxConstructor2() {

        Box box = new Box(this.box);

        assertTrue(box.getXMin() == xMin);
        assertTrue(box.getXMax() == xMax);
        assertTrue(box.getYMin() == yMin);
        assertTrue(box.getYMax() == yMax);
        assertTrue(box.getZMin() == zMin);
        assertTrue(box.getZMax() == zMax);
    }

    @Test
    public void testLineIntersection() {
        Vector3 source = new Vector3(xMin - 5, yMiddle, zMiddle);
        Vector3 destination = new Vector3(xMax + 5, yMiddle, zMiddle);

        // Intersects with box
        assertEquals(box.lineIntersection(source, destination), new Vector3(xMin, yMiddle, zMiddle));

        source = new Vector3(xMin - 5, yMin - 10, zMiddle);
        destination = new Vector3(xMax + 5, yMin - 10, zMiddle);

        // Does not intersect with box
        assertTrue(box.lineIntersection(source, destination) == null);
    }

    @Test
    public void testContainsCorners() {
        Vector3 vector = new Vector3(xMin, yMin, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMin, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMax, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMax, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMin, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMin, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMax, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMax, zMax);
        assertTrue(box.contains(vector));
    }

    @Test
    public void testContainsEdges() {

        Vector3 vector = new Vector3(xMiddle, yMin, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMin, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMax, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMax, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMiddle, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMiddle, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMiddle, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMiddle, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMin, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMax, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMin, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMax, zMiddle);
        assertTrue(box.contains(vector));
    }

    @Test
    public void testContainsPlanes() {
        Vector3 vector = new Vector3(xMiddle, yMiddle, zMin);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMiddle, zMax);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMin, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMax, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin, yMiddle, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax, yMiddle, zMiddle);
        assertTrue(box.contains(vector));

    }

    @Test
    public void testContainsInBox() {
        Vector3 vector = new Vector3(xMiddle, yMiddle, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMiddle, zMin + 0.0001);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMiddle, zMax - 0.0001);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMin + 0.0001, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMiddle, yMax - 0.0001, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMin + 0.0001, yMiddle, zMiddle);
        assertTrue(box.contains(vector));

        vector = new Vector3(xMax - 0.0001, yMiddle, zMiddle);
        assertTrue(box.contains(vector));
    }

    @Test
    public void testContainsOutsideBox() {

        Vector3 vector = new Vector3(xMiddle, yMiddle, zMin - 0.0001);
        assertFalse(box.contains(vector));

        vector = new Vector3(xMiddle, yMiddle, zMax + 0.0001);
        assertFalse(box.contains(vector));

        vector = new Vector3(xMiddle, yMin - 0.0001, zMiddle);
        assertFalse(box.contains(vector));

        vector = new Vector3(xMiddle, yMax + 0.0001, zMiddle);
        assertFalse(box.contains(vector));

        vector = new Vector3(xMin - 0.0001, yMiddle, zMiddle);
        assertFalse(box.contains(vector));

        vector = new Vector3(xMax + 0.0001, yMiddle, zMiddle);
        assertFalse(box.contains(vector));
    }
}
