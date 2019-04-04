/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.interrep.topography;

import java.awt.Rectangle;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.util.List;

/**
 * This class creates a 2D polygon and uses a height parameter to make it 3D
 * 
 * @author ttevendale
 */
public class TopographyPolygon implements ITopographyFeature {

    /**
     * The ID of the feature. NOTE: must be unique.
     */
    private final long featureID;

    /**
     * The 2D polygon.
     */
    private final Path2D polygon;

    /**
     * The height of the polygon.
     */
    private final double height;

    /**
     * The offset of the polygon from sea level (0).
     */
    private final double offset;

    /**
     * The bounding box that encapsulates this feature.
     */
    private final Box boundingBox;

    /**
     * Constructor
     * 
     * @param featureID The ID of this feature.
     * @param points The points that will create the polygon. NOTE: there has to be at least three
     *        points passed in to create the simplest polygon.
     * @param height The height of the polygon.
     */
    public TopographyPolygon(long featureID, List<Point2D> points, double height) {

        this(featureID, points, height, 0);
    }

    /**
     * Constructor
     * 
     * @param featureID The ID of this feature.
     * @param points The points that will create the polygon. NOTE: there has to be at least three
     *        points passed in to create the simplest polygon.
     * @param height The height of the polygon.
     * @param offset The offset of the polygon from sea level (0).
     */
    public TopographyPolygon(long featureID, List<Point2D> points, double height, double offset) {

        if (points == null || points.size() < 3) {
            throw new IllegalStateException("There must be at least be three points.");
        }
        this.featureID = featureID;
        this.height = height;
        this.offset = offset;
        this.polygon = this.createPolygon(points);
        this.boundingBox = this.createBoundingBox();
    }

    /**
     * Creates the bounding box for this feature.
     * 
     * @return This feature's bounding box.
     */
    private Box createBoundingBox() {

        Rectangle bounds = polygon.getBounds();
        Box box = new Box(bounds.getMinX(), bounds.getMinY(), offset, bounds.getMaxX(), bounds.getMaxY(), offset + height);

        return box;
    }

    /**
     * Creates the polygon from the points passed.
     * 
     * @param points The points to create the polygon from.
     * @return The polygon.
     */
    private Path2D createPolygon(List<Point2D> points) {

        Path2D polygon = new Path2D.Double();
        polygon.moveTo(points.get(0).getX(), points.get(0).getY());

        for (int i = 1; i < points.size(); i++) {

            Point2D point = points.get(i);
            polygon.lineTo(point.getX(), point.getY());
        }

        polygon.closePath();
        return polygon;
    }

    /**
     * Finds the intersection point from the source to destination with this feature if it exists.
     * 
     * @param source The source node.
     * @param destination The destination node.
     * @return The intersection vector or null if it doesn't exist.
     */
    @Override
    public Vector3 lineIntersection(Vector3 source, Vector3 destination) {
        Vector3 intersectVector = null;
        Vector3 boxIntersection = this.boundingBox.lineIntersection(source, destination);

        if (boxIntersection != null) {

            int loopCount = (int)boxIntersection.distanceTo(destination);
            double alphaMultiplier = 1.0 / (double)loopCount;

            for (int i = 0; i <= loopCount; i++) {

                Vector3 lerpedVector = Vector3.lerpVectors(boxIntersection, destination, i * alphaMultiplier);

                if (this.contains(lerpedVector)) {

                    intersectVector = lerpedVector;
                    break;
                }
                if (!this.boundingBox.contains(lerpedVector)) {

                    break;
                }
            }
        }

        return intersectVector;
    }

    /**
     * Checks to see if the point is contained in this feature.
     * 
     * @param point The point to check.
     * @return True if the point is within the box, false otherwise.
     */
    @Override
    public boolean contains(Vector3 point) {

        boolean isWithin = false;
        double degreeOfError = 0.01;
        if (polygon.contains(point.getX(), point.getY()) && point.getZ() > offset - degreeOfError && point.getZ() < height + offset + degreeOfError) {

            isWithin = true;
        }

        return isWithin;
    }

    /**
     * Checks to see if the point is contained in this feature.
     * 
     * @param x The x coordinate to check.
     * @param y The y coordinate to check.
     * @param z The z coordinate to check.
     * @return True if the point is within the box, false otherwise.
     */
    @Override
    public boolean contains(double x, double y, double z) {

        return this.contains(new Vector3(x, y, z));
    }

    /**
     * Getter for the feature ID.
     * 
     * @return The feature ID.
     */
    @Override
    public long getFeatureID() {

        return this.featureID;
    }
}
