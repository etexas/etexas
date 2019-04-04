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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class TestDSRCTopography {

    DSRCTopography topography;

    long featureId1 = 1;

    long featureId2 = 2;

    @Before
    public void setup() {
        Collection<ITopographyFeature> features = new ArrayList<ITopographyFeature>(2);

        List<Point2D> points = new ArrayList<Point2D>(4);
        points.add(new Point2D.Double(0, 0));
        points.add(new Point2D.Double(100, 0));
        points.add(new Point2D.Double(100, 100));
        points.add(new Point2D.Double(0, 100));

        features.add(new TopographyPolygon(featureId1, points, 100));

        points = new ArrayList<Point2D>(4);
        points.add(new Point2D.Double(400, 0));
        points.add(new Point2D.Double(500, 0));
        points.add(new Point2D.Double(500, 100));
        points.add(new Point2D.Double(400, 100));

        features.add(new TopographyPolygon(featureId2, points, 100));

        topography = new DSRCTopography(features);
    }

    @After
    public void teardown() {
        topography = null;
    }

    @Test
    public void testGetObstructionCellular() {
        assertNull(topography.getObstruction(new Vector3(-50, 50, 50), new Vector3(1000, 50, 50), TopographyMessageType.CELLULAR));
    }

    @Test
    public void testGetObstructionBottomToTop() {

        assertNull(topography.getObstruction(new Vector3(50, 250, -50), new Vector3(50, 250, 150), TopographyMessageType.DSRC));
        assertNull(topography.getObstruction(new Vector3(250, 50, -50), new Vector3(250, 50, 150), TopographyMessageType.DSRC));

        ITopographyFeature feature = topography.getObstruction(new Vector3(50, 50, -50), new Vector3(50, 50, 150), TopographyMessageType.DSRC);
        assertNotNull(feature);
        assertTrue(feature.getFeatureID() == featureId1);
    }

    @Test
    public void testGetObstructionTopToBottom() {

        assertNull(topography.getObstruction(new Vector3(50, 250, 150), new Vector3(50, 250, -50), TopographyMessageType.DSRC));
        assertNull(topography.getObstruction(new Vector3(250, 50, 150), new Vector3(250, 50, -50), TopographyMessageType.DSRC));

        ITopographyFeature feature = topography.getObstruction(new Vector3(450, 50, 150), new Vector3(450, 50, -50), TopographyMessageType.DSRC);
        assertNotNull(feature);
        assertTrue(feature.getFeatureID() == featureId2);
    }

    @Test
    public void testGetObstructionLeftToRight() {

        assertNull(topography.getObstruction(new Vector3(-50, 250, 50), new Vector3(750, 250, 50), TopographyMessageType.DSRC));
        assertNull(topography.getObstruction(new Vector3(-50, 50, 250), new Vector3(750, 50, 250), TopographyMessageType.DSRC));

        ITopographyFeature feature = topography.getObstruction(new Vector3(-50, 50, 50), new Vector3(750, 50, 50), TopographyMessageType.DSRC);
        assertNotNull(feature);
        assertTrue(feature.getFeatureID() == featureId1);
    }

    @Test
    public void testGetObstructionRightToLeft() {

        assertNull(topography.getObstruction(new Vector3(750, 250, 50), new Vector3(-50, 250, 50), TopographyMessageType.DSRC));
        assertNull(topography.getObstruction(new Vector3(750, 50, 250), new Vector3(-50, 50, 250), TopographyMessageType.DSRC));

        ITopographyFeature feature = topography.getObstruction(new Vector3(750, 50, 50), new Vector3(-50, 50, 50), TopographyMessageType.DSRC);
        assertNotNull(feature);
        assertTrue(feature.getFeatureID() == featureId2);
    }

    @Test
    public void testGetObstructionFrontToBack() {

        assertNull(topography.getObstruction(new Vector3(50, -50, 250), new Vector3(50, 150, 250), TopographyMessageType.DSRC));
        assertNull(topography.getObstruction(new Vector3(250, -50, 50), new Vector3(250, 150, 50), TopographyMessageType.DSRC));

        ITopographyFeature feature = topography.getObstruction(new Vector3(50, -50, 50), new Vector3(50, 150, 50), TopographyMessageType.DSRC);
        assertNotNull(feature);
        assertTrue(feature.getFeatureID() == featureId1);
    }

    @Test
    public void testGetObstructionBackToFront() {

        assertNull(topography.getObstruction(new Vector3(50, 150, 250), new Vector3(50, -50, 250), TopographyMessageType.DSRC));
        assertNull(topography.getObstruction(new Vector3(250, 150, 50), new Vector3(250, -50, 50), TopographyMessageType.DSRC));

        ITopographyFeature feature = topography.getObstruction(new Vector3(450, 150, 50), new Vector3(450, -50, 50), TopographyMessageType.DSRC);
        assertNotNull(feature);
        assertTrue(feature.getFeatureID() == featureId2);
    }
}
