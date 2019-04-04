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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class TestBoundingBoxCalculation {

    BoundingBoxCalculation calc;

    Box box;

    Vector3 centerVector;

    @Before
    public void setup() {

        calc = new BoundingBoxCalculation();

        box = new Box(0, 0, 0, 10, 10, 5);

        centerVector = new Vector3((box.getXMin() + box.getXMax()) / 2, (box.getYMin() + box.getYMax()) / 2, (box.getZMin() + box.getZMax()) / 2);
    }

    @After
    public void teardown() {

        box = null;
        centerVector = null;
    }

    private void cornerTestCase(Vector3 offsetVector, Vector3 intersectionVector, boolean xFlip, boolean yFlip, boolean zFlip) {

        double startX = xFlip ? centerVector.getX() + offsetVector.getX() : centerVector.getX() - offsetVector.getX();
        double endX = xFlip ? centerVector.getX() - offsetVector.getX() : centerVector.getX() + offsetVector.getX();

        double startY = yFlip ? centerVector.getY() + offsetVector.getY() : centerVector.getY() - offsetVector.getY();
        double endY = yFlip ? centerVector.getY() - offsetVector.getY() : centerVector.getY() + offsetVector.getY();

        double startZ = zFlip ? centerVector.getZ() + offsetVector.getZ() : centerVector.getZ() - offsetVector.getZ();
        double endZ = zFlip ? centerVector.getZ() - offsetVector.getZ() : centerVector.getZ() + offsetVector.getZ();

        Vector3 startVector = new Vector3(startX, startY, startZ);
        Vector3 endVector = new Vector3(endX, endY, endZ);

        Vector3 intersection = calc.lineIntersection(box, startVector, endVector);
        assertNotNull(intersection);
        assertTrue(intersection.equals(intersectionVector));
    }

    @Test
    public void testLineIntersectionOutsideCorner() {

        // offset to the outside of the box.
        Vector3 offsetVector = new Vector3(box.getXMax() - box.getXMin(), box.getYMax() - box.getYMin(), box.getZMax() - box.getZMin());

        // bottom = zMin, top = zMax, left = xMin, right = xMax, front = yMin, back = yMax

        // bottom left-front corner to top right-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMin(), box.getZMin()), false, false, false);

        // bottom right-front corner to top left-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMin(), box.getZMin()), true, false, false);

        // bottom left-back corner to top right-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMax(), box.getZMin()), false, true, false);

        // top left-front corner to bottom right-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMin(), box.getZMax()), false, false, true);

        // bottom right-back corner to top left-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMax(), box.getZMin()), true, true, false);

        // top right-front corner to bottom left-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMin(), box.getZMax()), true, false, true);

        // top left-back corner to bottom right-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMax(), box.getZMax()), false, true, true);

        // top right-back corner to bottom left-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMax(), box.getZMax()), true, true, true);
    }

    @Test
    public void testLineIntersectionOnCorner() {

        // offset onto the lines of the box.
        Vector3 offsetVector = new Vector3((box.getXMax() - box.getXMin()) / 2, (box.getYMax() - box.getYMin()) / 2, (box.getZMax() - box.getZMin()) / 2);

        // bottom = zMin, top = zMax, left = xMin, right = xMax, front = yMin, back = yMax

        // bottom left-front corner to top right-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMin(), box.getZMin()), false, false, false);

        // bottom right-front corner to top left-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMin(), box.getZMin()), true, false, false);

        // bottom left-back corner to top right-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMax(), box.getZMin()), false, true, false);

        // top left-front corner to bottom right-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMin(), box.getZMax()), false, false, true);

        // bottom right-back corner to top left-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMax(), box.getZMin()), true, true, false);

        // top right-front corner to bottom left-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMin(), box.getZMax()), true, false, true);

        // top left-back corner to bottom right-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin(), box.getYMax(), box.getZMax()), false, true, true);

        // top right-back corner to bottom left-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax(), box.getYMax(), box.getZMax()), true, true, true);
    }

    @Test
    public void testLineIntersectionInsideCorner() {

        // offset to the inside of the box.
        Vector3 offsetVector = new Vector3((box.getXMax() - box.getXMin()) / 4, (box.getYMax() - box.getYMin()) / 4, (box.getZMax() - box.getZMin()) / 4);

        // bottom = zMin, top = zMax, left = xMin, right = xMax, front = yMin, back = yMax

        // bottom left-front corner to top right-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin() + offsetVector.getX(), box.getYMin() + offsetVector.getY(), box.getZMin() + offsetVector.getZ()), false, false, false);

        // bottom right-front corner to top left-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax() - offsetVector.getX(), box.getYMin() + offsetVector.getY(), box.getZMin() + offsetVector.getZ()), true, false, false);

        // bottom left-back corner to top right-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin() + offsetVector.getX(), box.getYMax() - offsetVector.getY(), box.getZMin() + offsetVector.getZ()), false, true, false);

        // top left-front corner to bottom right-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin() + offsetVector.getX(), box.getYMin() + offsetVector.getY(), box.getZMax() - offsetVector.getZ()), false, false, true);

        // bottom right-back corner to top left-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax() - offsetVector.getX(), box.getYMax() - offsetVector.getY(), box.getZMin() + offsetVector.getZ()), true, true, false);

        // top right-front corner to bottom left-back corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax() - offsetVector.getX(), box.getYMin() + offsetVector.getY(), box.getZMax() - offsetVector.getZ()), true, false, true);

        // top left-back corner to bottom right-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMin() + offsetVector.getX(), box.getYMax() - offsetVector.getY(), box.getZMax() - offsetVector.getZ()), false, true, true);

        // top right-back corner to bottom left-front corner
        this.cornerTestCase(offsetVector, new Vector3(box.getXMax() - offsetVector.getX(), box.getYMax() - offsetVector.getY(), box.getZMax() - offsetVector.getZ()), true, true, true);
    }

    @Test
    public void testLineIntersectionNode1InsideBox() {
        Vector3 vector = new Vector3(0, 0, 0);
        Vector3 vector2 = new Vector3(10, 0, 0);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(0, 0, 0));

        vector = new Vector3(0, 0, -1);
        vector2 = new Vector3(0, 0, 15);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(0, 0, 0));

        vector = new Vector3(0, 0, 15);
        vector2 = new Vector3(0, 0, -1);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(0, 0, 5));

        vector = new Vector3(0, -15, 0);
        vector2 = new Vector3(0, 20, 0);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(0, 0, 0));

        vector = new Vector3(0, 20, 0);
        vector2 = new Vector3(0, -15, 0);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(0, 10, 0));

        vector = new Vector3(-182, 0, 0);
        vector2 = new Vector3(9000, 0, 0);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(0, 0, 0));

        vector = new Vector3(9000, 0, 0);
        vector2 = new Vector3(-182, 0, 0);
        assertEquals(calc.lineIntersection(box, vector, vector2), new Vector3(10, 0, 0));
    }

    @Test
    public void testLineIntersectionAcrossPlane() {
        // goes across the plane being calculated but does not actually intersect with the box

        // xMin
        Vector3 vector = new Vector3(-5, 5, 2.5);
        Vector3 vector2 = new Vector3(5, 5, 15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(-5, 5, 2.5);
        vector2 = new Vector3(5, 5, -15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(-5, 5, 2.5);
        vector2 = new Vector3(5, 30, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(-5, 5, 2.5);
        vector2 = new Vector3(5, -30, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        // xMax
        vector = new Vector3(15, 5, 2.5);
        vector2 = new Vector3(5, 5, 15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(15, 5, 2.5);
        vector2 = new Vector3(5, 5, -15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(15, 5, 2.5);
        vector2 = new Vector3(5, 30, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(15, 5, 2.5);
        vector2 = new Vector3(5, -30, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        // yMin
        vector = new Vector3(5, -5, 2.5);
        vector2 = new Vector3(5, 5, 15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, -5, 2.5);
        vector2 = new Vector3(5, 5, -15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, -5, 2.5);
        vector2 = new Vector3(30, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, -5, 2.5);
        vector2 = new Vector3(-30, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        // yMax
        vector = new Vector3(5, 15, 2.5);
        vector2 = new Vector3(5, 5, 15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 15, 2.5);
        vector2 = new Vector3(5, 5, -15);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 15, 2.5);
        vector2 = new Vector3(30, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 15, 2.5);
        vector2 = new Vector3(-30, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        // zMin
        vector = new Vector3(5, 5, -10);
        vector2 = new Vector3(15, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 5, -10);
        vector2 = new Vector3(5, 15, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 5, -10);
        vector2 = new Vector3(-15, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 5, -10);
        vector2 = new Vector3(5, -15, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        // zMax
        vector = new Vector3(5, 5, -10);
        vector2 = new Vector3(15, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 5, 10);
        vector2 = new Vector3(5, 15, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 5, 10);
        vector2 = new Vector3(-15, 5, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));

        vector = new Vector3(5, 5, 10);
        vector2 = new Vector3(5, -15, 2.5);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }

    @Test
    public void testLineIntersectionLeft() {
        Vector3 vector = new Vector3(box.getXMin() - 1, box.getYMin() + 1, box.getZMin() + 1);
        Vector3 vector2 = new Vector3(box.getXMin() - 1, box.getYMax() - 1, box.getZMax() - 1);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }

    @Test
    public void testLineIntersectionRight() {
        Vector3 vector = new Vector3(box.getXMax() + 1, box.getYMin() + 1, box.getZMin() + 1);
        Vector3 vector2 = new Vector3(box.getXMax() + 1, box.getYMax() - 1, box.getZMax() - 1);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }

    @Test
    public void testLineIntersectionInFront() {
        Vector3 vector = new Vector3(box.getXMin() + 1, box.getYMin() - 1, box.getZMin() + 1);
        Vector3 vector2 = new Vector3(box.getXMax() - 1, box.getYMin() - 1, box.getZMax() - 1);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }

    @Test
    public void testLineIntersectionBehind() {
        Vector3 vector = new Vector3(box.getXMin() + 1, box.getYMax() + 1, box.getZMin() + 1);
        Vector3 vector2 = new Vector3(box.getXMax() - 1, box.getYMax() + 1, box.getZMax() - 1);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }

    @Test
    public void testLineIntersectionAbove() {
        Vector3 vector = new Vector3(box.getXMin() + 1, box.getYMin() + 1, box.getZMin() - 1);
        Vector3 vector2 = new Vector3(box.getXMax() - 1, box.getYMax() - 1, box.getZMin() - 1);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }

    @Test
    public void testLineIntersectionBelow() {
        Vector3 vector = new Vector3(box.getXMin() + 1, box.getYMin() + 1, box.getZMax() + 1);
        Vector3 vector2 = new Vector3(box.getXMax() - 1, box.getYMax() - 1, box.getZMax() + 1);
        assertNull(calc.lineIntersection(box, vector, vector2));
    }
}
