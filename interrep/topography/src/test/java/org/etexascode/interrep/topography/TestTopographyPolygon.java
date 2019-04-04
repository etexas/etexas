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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class TestTopographyPolygon {

    List<Point2D> constructorPoints;

    TopographyPolygon polygon;

    int featureId = 1;

    double height = 20;

    double offset = 100;

    @Before
    public void setup() {
        constructorPoints = new ArrayList<Point2D>();
        constructorPoints.add(new Point2D.Double(0, 0));
        constructorPoints.add(new Point2D.Double(30, 0));
        constructorPoints.add(new Point2D.Double(30, 40));
        constructorPoints.add(new Point2D.Double(-20, 30));

        List<Point2D> points = new ArrayList<Point2D>();
        points.add(new Point2D.Double(300, 25));
        points.add(new Point2D.Double(10, 15));
        points.add(new Point2D.Double(30, 400));
        points.add(new Point2D.Double(-290, 320));

        polygon = new TopographyPolygon(featureId, points, height, offset);
    }

    @After
    public void teardown() {
        constructorPoints.clear();
        constructorPoints = null;
        polygon = null;
    }

    @Test
    public void testTopographyPolygonConstructor1() {

        TopographyPolygon polygon = new TopographyPolygon(featureId, constructorPoints, height);
        assertTrue(polygon.getFeatureID() == featureId);
        assertTrue(polygon.contains(new Vector3(5, 5, height - 1)));
        assertTrue(polygon.contains(new Vector3(-10, 25, height - 1)));
        assertFalse(polygon.contains(new Vector3(-10, -5, height - 1)));
    }

    @Test
    public void testTopographyPolygonConstructor2() {

        TopographyPolygon polygon = new TopographyPolygon(featureId, constructorPoints, height, offset);
        assertTrue(polygon.getFeatureID() == featureId);
        assertTrue(polygon.contains(new Vector3(5, 5, 115)));
        assertTrue(polygon.contains(new Vector3(-10, 25, 115)));
        assertFalse(polygon.contains(new Vector3(-10, -5, 115)));
        assertFalse(polygon.contains(new Vector3(3, 3, 30)));

    }

    @Test(expected = IllegalStateException.class)
    public void testNoPoints1() {
        new TopographyPolygon(featureId, null, 0, 0);
    }

    @Test(expected = IllegalStateException.class)
    public void testNoPoints2() {
        new TopographyPolygon(featureId, new ArrayList<Point2D>(0), 0, 0);
    }

    @Test(expected = IllegalStateException.class)
    public void testOnePoint() {
        List<Point2D> point = new ArrayList<Point2D>(1);
        point.add(new Point2D.Double(5, 5));
        TopographyPolygon polygon = new TopographyPolygon(featureId, point, 0, 0);
    }

    @Test(expected = IllegalStateException.class)
    public void testTwoPoints() {
        List<Point2D> points = new ArrayList<Point2D>(1);
        points.add(new Point2D.Double(5, 5));
        points.add(new Point2D.Double(10, 15));
        new TopographyPolygon(featureId, points, 0, 0);
    }

    @Test
    public void testThreePoints() {
        List<Point2D> points = new ArrayList<Point2D>(1);
        points.add(new Point2D.Double(0, 0));
        points.add(new Point2D.Double(0, 10));
        points.add(new Point2D.Double(10, 0));
        TopographyPolygon polygon = new TopographyPolygon(featureId, points, 10, 0);
        assertTrue(polygon.contains(new Vector3(1, 1, 1)));

        assertTrue(polygon.contains(new Vector3(0.1, 0.1, 1)));
        assertTrue(polygon.contains(new Vector3(0.1, 9.5, 1)));
        assertTrue(polygon.contains(new Vector3(9.5, 0.1, 1)));
        assertTrue(polygon.contains(new Vector3(4.999999, 4.999999, 1)));
        assertFalse(polygon.contains(new Vector3(-1, 0, 1)));
        assertFalse(polygon.contains(new Vector3(0, 11, 1)));
        assertFalse(polygon.contains(new Vector3(11, 0, 1)));
    }

    @Test
    public void testContains1() {

        Vector3 vector = new Vector3(50, 50, offset + 1);
        assertTrue(polygon.contains(vector));

        vector = new Vector3(50, 50, height + offset + 1);
        assertFalse(polygon.contains(vector));

        vector = new Vector3(50, 50, offset - 1);
        assertFalse(polygon.contains(vector));

        vector = new Vector3(-50, 50, offset + 1);
        assertFalse(polygon.contains(vector));

        vector = new Vector3(0, 0, 0);
        assertFalse(polygon.contains(vector));
    }

    @Test
    public void testContains2() {

        assertTrue(polygon.contains(50, 50, offset + 1));
        assertFalse(polygon.contains(50, 50, height + offset + 1));
        assertFalse(polygon.contains(50, 50, offset - 1));
        assertFalse(polygon.contains(-50, 50, offset + 1));
        assertFalse(polygon.contains(0, 0, 0));
    }

    @Test
    public void testLineIntersection() {

        Vector3 vector = polygon.lineIntersection(new Vector3(-1000, 50, 110), new Vector3(1000, 50, 110));
        assertNotNull(vector);
        assertEquals(vector, new Vector3(12, 50, 110));

        vector = polygon.lineIntersection(new Vector3(50, 50, -1000), new Vector3(50, 50, 1000));
        System.out.println(vector.getX() + "," + vector.getY() + "," + vector.getZ());
        assertNotNull(vector);
        assertEquals(vector, new Vector3(50, 50, offset));

        vector = polygon.lineIntersection(new Vector3(50, 50, 1000), new Vector3(50, 50, -1000));
        System.out.println(vector.getX() + "," + vector.getY() + "," + vector.getZ());
        assertNotNull(vector);
        assertEquals(vector, new Vector3(50, 50, height + offset));

        assertNull(polygon.lineIntersection(new Vector3(-1000, 50, 50), new Vector3(1000, 50, 50)));
        assertNull(polygon.lineIntersection(new Vector3(-100, 10, -1000), new Vector3(50, 50, 1000)));
        assertNull(polygon.lineIntersection(new Vector3(-50, 50, 110), new Vector3(-50, 50, 105)));
    }
}
