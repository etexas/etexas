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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class TestVector3 {

    @Before
    public void setup() {}

    @After
    public void teardown() {}

    @Test
    public void testVector3Constructor1() {
        Vector3 vector = new Vector3();
        assertTrue(vector.getX() == 0);
        assertTrue(vector.getY() == 0);
        assertTrue(vector.getZ() == 0);
    }

    @Test
    public void testVector3Constructor2() {
        Vector3 vector = new Vector3(50, 68, 999);
        assertTrue(vector.getX() == 50);
        assertTrue(vector.getY() == 68);
        assertTrue(vector.getZ() == 999);
    }

    @Test
    public void testVector3Constructor3() {
        Vector3 vector = new Vector3(2561, 878, 5545);
        Vector3 vector2 = new Vector3(vector);
        assertTrue(vector2.getX() == 2561);
        assertTrue(vector2.getY() == 878);
        assertTrue(vector2.getZ() == 5545);
    }

    @Test
    public void testAdd() {
        Vector3 vector = new Vector3();
        vector.add(new Vector3(15, 66, 78));

        assertEquals(vector, new Vector3(15, 66, 78));

        vector.add(new Vector3(89, 45, 45));

        assertEquals(vector, new Vector3(104, 111, 123));
    }

    @Test
    public void testAddScalar() {
        Vector3 vector = new Vector3();
        vector.addScalar(50);

        assertEquals(vector, new Vector3(50, 50, 50));

        Vector3 vector2 = new Vector3(15, 66, 78);
        vector2.addScalar(23);

        assertEquals(vector2, new Vector3(38, 89, 101));
    }

    @Test
    public void testAddVectors() {
        Vector3 vector = Vector3.addVectors(new Vector3(15, 66, 78), new Vector3(5, 36, 68));

        assertEquals(vector, new Vector3(20, 102, 146));
    }

    @Test
    public void testSub() {
        Vector3 vector = new Vector3();
        vector.sub(new Vector3(23, 455, 28));

        assertEquals(vector, new Vector3(-23, -455, -28));

        vector.sub(new Vector3(-15, 35, -1));

        assertEquals(vector, new Vector3(-8, -490, -27));
    }

    @Test
    public void testSubScalar() {
        Vector3 vector = new Vector3();
        vector.subScalar(50);

        assertEquals(vector, new Vector3(-50, -50, -50));

        Vector3 vector2 = new Vector3(15, -66, 78);
        vector2.subScalar(23);

        assertEquals(vector2, new Vector3(-8, -89, 55));
    }

    @Test
    public void testSubVectors() {
        Vector3 vector = Vector3.subVectors(new Vector3(15, 66, 78), new Vector3(5, 36, 68));

        assertEquals(vector, new Vector3(10, 30, 10));
    }

    @Test
    public void testMultiply() {
        Vector3 vector = new Vector3(5, 87, 564);
        vector.multiply(new Vector3(87, 864, 48));

        assertEquals(vector, new Vector3(435, 75168, 27072));

        vector.multiply(new Vector3());

        assertEquals(vector, new Vector3(0, 0, 0));
    }

    @Test
    public void testMultiplyScalar() {
        Vector3 vector = new Vector3(5, 8, 54);
        vector.multiplyScalar(15);

        assertEquals(vector, new Vector3(75, 120, 810));
    }

    @Test
    public void testMultiplyVectors() {
        Vector3 vector = Vector3.multiplyVectors(new Vector3(15, 6, 8), new Vector3(5, 36, 6));

        assertEquals(vector, new Vector3(75, 216, 48));
    }

    @Test
    public void testDivide() {
        Vector3 vector = new Vector3(54, 61, 18);
        vector.divide(new Vector3(10, 5, 2));

        assertEquals(vector, new Vector3(5.4, 12.2, 9));
    }

    @Test
    public void testDivideByZero() {

        assertTrue(this.testDivideByZero(new Vector3(0, 0, 0)));
        assertTrue(this.testDivideByZero(new Vector3(0, 23, 13)));
        assertTrue(this.testDivideByZero(new Vector3(23, 0, 123)));
        assertTrue(this.testDivideByZero(new Vector3(45, 24, 0)));
        assertTrue(this.testDivideByZero(new Vector3(0, 0, 25)));
        assertTrue(this.testDivideByZero(new Vector3(0, 132, 0)));
        assertTrue(this.testDivideByZero(new Vector3(413, 0, 0)));
        assertFalse(this.testDivideByZero(new Vector3(23, 143, 13)));

    }

    @Test
    public void testDivideScalar() {
        Vector3 vector = new Vector3(84, 867, 2485);
        vector.divideScalar(5);

        assertEquals(vector, new Vector3(16.8, 173.4, 497));
    }

    @Test(expected = ArithmeticException.class)
    public void testDivideScalarByZero() {
        Vector3 vector = new Vector3(10, 15, 35);
        vector.divideScalar(5);
        assertEquals(vector, new Vector3(2, 3, 7));

        vector.divideScalar(0);

    }

    @Test
    public void testNegate() {
        Vector3 vector = new Vector3(-15, 84787, -147);
        vector.negate();

        assertEquals(vector, new Vector3(15, -84787, 147));
    }

    @Test
    public void testDot() {
        Vector3 vector = new Vector3(234, 13, 1314);

        assertTrue(vector.dot(new Vector3()) == 0);
        assertTrue(vector.dot(new Vector3(18, -78, 454)) == 599754);
        assertTrue(vector.dot(new Vector3(18546456, -78456456, 45445456)) == 63035265960.0);
    }

    @Test
    public void testLerp() {
        Vector3 vector = new Vector3(234, 13, 1314);
        Vector3 vector2 = new Vector3(-1233, 234, -2342);

        Vector3 vector3 = vector.lerp(vector2, 0);

        assertEquals(vector3, vector);

        vector3 = vector.lerp(vector2, 1);

        assertEquals(vector3, vector2);

        vector3 = vector.lerp(vector2, 0.5);

        assertEquals(vector3, new Vector3(-499.5, 123.5, -514));

        vector3 = vector.lerp(vector2, -1);

        assertEquals(vector3, new Vector3(1701, -208, 4970));
    }

    @Test
    public void testLerpVectors() {
        Vector3 vector = new Vector3(234, 13, 1314);
        Vector3 vector2 = new Vector3(-1233, 234, -2342);

        Vector3 vector3 = Vector3.lerpVectors(vector, vector2, 0);

        assertEquals(vector3, vector);

        vector3 = Vector3.lerpVectors(vector, vector2, 1);

        assertEquals(vector3, vector2);

        vector3 = Vector3.lerpVectors(vector, vector2, 0.5);

        assertEquals(vector3, new Vector3(-499.5, 123.5, -514));

        vector3 = Vector3.lerpVectors(vector, vector2, -1);

        assertEquals(vector3, new Vector3(1701, -208, 4970));
    }

    @Test
    public void testCross() {
        Vector3 vector = new Vector3(12, 5, 7);

        vector.cross(new Vector3(87, 9, 4));

        assertEquals(vector, new Vector3(-43, 561, -327));

        vector.cross(new Vector3(3, -9, 14));

        assertEquals(vector, new Vector3(4911, -379, -1296));
    }

    @Test
    public void testCrossVectors() {
        Vector3 vector = Vector3.crossVectors(new Vector3(12, 5, 7), new Vector3(87, 9, 4));

        assertEquals(vector, new Vector3(-43, 561, -327));

        vector = Vector3.crossVectors(new Vector3(-43, 561, -327), new Vector3(3, -9, 14));

        assertEquals(vector, new Vector3(4911, -379, -1296));
    }

    @Test
    public void testDistanceTo() {
        Vector3 vector = new Vector3(234, 13, 1314);

        assertTrue(vector.distanceTo(new Vector3(-1233, 234, -2342)) == 3945.537479228907286847699141325);
        assertTrue(vector.distanceTo(new Vector3(18, 87, -78)) == 1410.6012902305172640055014379703);
    }

    @Test
    public void testDistanceToSquared() {
        Vector3 vector = new Vector3(234, 13, 1314);

        assertTrue(vector.distanceToSquared(new Vector3(-1233, 234, -2342)) == 15567266);
        assertTrue(vector.distanceToSquared(new Vector3(18, 87, -78)) == 1989796);
    }

    @Test
    public void testEquals() {
        assertFalse(new Vector3().equals(new String()));
        assertTrue(new Vector3().equals(new Vector3()));
        assertTrue(new Vector3(23, 12, 23).equals(new Vector3(23, 12, 23)));
        assertFalse(new Vector3(23, 12, 23).equals(new Vector3(24, 12, 23)));
        assertFalse(new Vector3(23, 12, 23).equals(new Vector3(23, 10, 23)));
        assertFalse(new Vector3(23, 12, 23).equals(new Vector3(23, 12, 254)));
    }

    @Test
    public void testClosestVector() {
        Vector3 vector = new Vector3();
        Vector3 farVector = new Vector3(500, 250, 100);
        Vector3 closeVector = new Vector3(25, 80, 95);

        Collection<Vector3> vectors = new ArrayList<Vector3>(2);
        vectors.add(farVector);
        vectors.add(closeVector);

        Vector3 closestVector = vector.closestVector(vectors);

        assertEquals(closestVector, closeVector);
        assertFalse(closestVector.equals(farVector));
        assertNull(vector.closestVector(new ArrayList<Vector3>(0)));
    }

    private boolean testDivideByZero(Vector3 zeroVector) {
        Vector3 vector = new Vector3();

        try {

            vector.divide(zeroVector);
        }
        catch (ArithmeticException e) {

            if ("Cannot divide by zero.".equals(e.getMessage())) {

                return true;
            }
            else {
                return false;
            }
        }

        return false;

    }
}
