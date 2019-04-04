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

import java.util.Collection;
import java.util.HashSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * BoundingBoxCalculation class built from the algorithm located here
 * (http://www.3dkingdoms.com/weekly/weekly.php?a=3)
 * 
 * @author ttevendale
 */
public class BoundingBoxCalculation {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(BoundingBoxCalculation.class);

    /**
     * Enumeration for the Axis.
     */
    private enum Axis {
        X,
        Y,
        Z
    }

    /**
     * The current plane intersection point.
     */
    private Vector3 hit = new Vector3();

    /**
     * Checks to see if the line created by two 3D points intersects the box and returns the
     * intersection point if it does (intersection from node1 to node2).
     * 
     * @param box The box to be checked.
     * @param node1 The start point of the line.
     * @param node2 The end point of the line.
     * @return The intersection point of the bounding box or null if there isn't an intersection
     *         point.
     */
    public Vector3 lineIntersection(Box box, Vector3 node1, Vector3 node2) {

        if (node2.getX() < box.getXMin() && node1.getX() < box.getXMin()) {
            LOGGER.debug("Line is located below the xMin of the box, no intersection.");
            return null;
        }
        if (node2.getX() > box.getXMax() && node1.getX() > box.getXMax()) {
            LOGGER.debug("Line is located above the xMax of the box, no intersection.");
            return null;
        }
        if (node2.getY() < box.getYMin() && node1.getY() < box.getYMin()) {
            LOGGER.debug("Line is located below the yMin of the box, no intersection.");
            return null;
        }
        if (node2.getY() > box.getYMax() && node1.getY() > box.getYMax()) {
            LOGGER.debug("Line is located above the yMax of the box, no intersection.");
            return null;
        }
        if (node2.getZ() < box.getZMin() && node1.getZ() < box.getZMin()) {
            LOGGER.debug("Line is located below the zMin of the box, no intersection.");
            return null;
        }
        if (node2.getZ() > box.getZMax() && node1.getZ() > box.getZMax()) {
            LOGGER.debug("Line is located above the zMax of the box, no intersection.");
            return null;
        }

        Collection<Vector3> planeIntersections = new HashSet<Vector3>(2);
        if (node1.getX() >= box.getXMin() && node1.getX() <= box.getXMax() &&
                node1.getY() >= box.getYMin() && node1.getY() <= box.getYMax() &&
                node1.getZ() >= box.getZMin() && node1.getZ() <= box.getZMax()) {

            LOGGER.debug("node1 is located within the box.");
            planeIntersections.add(new Vector3(node1));
        }

        if ((GetIntersection(node1.getX() - box.getXMin(), node2.getX() - box.getXMin(), node1, node2) && InBox(box, Axis.X))) {
            LOGGER.debug("Intersected with xMin Plane at " + hit.getX() + "," + hit.getY() + "," + hit.getZ());
            planeIntersections.add(new Vector3(hit));
        }
        if ((GetIntersection(node1.getY() - box.getYMin(), node2.getY() - box.getYMin(), node1, node2) && InBox(box, Axis.Y))) {
            LOGGER.debug("Intersected with xMax Plane at " + hit.getX() + "," + hit.getY() + "," + hit.getZ());
            planeIntersections.add(new Vector3(hit));
        }
        if ((GetIntersection(node1.getZ() - box.getZMin(), node2.getZ() - box.getZMin(), node1, node2) && InBox(box, Axis.Z))) {
            LOGGER.debug("Intersected with yMin Plane at " + hit.getX() + "," + hit.getY() + "," + hit.getZ());
            planeIntersections.add(new Vector3(hit));
        }
        if ((GetIntersection(node1.getX() - box.getXMax(), node2.getX() - box.getXMax(), node1, node2) && InBox(box, Axis.X))) {
            LOGGER.debug("Intersected with yMax Plane at " + hit.getX() + "," + hit.getY() + "," + hit.getZ());
            planeIntersections.add(new Vector3(hit));
        }
        if ((GetIntersection(node1.getY() - box.getYMax(), node2.getY() - box.getYMax(), node1, node2) && InBox(box, Axis.Y))) {
            LOGGER.debug("Intersected with zMin Plane at " + hit.getX() + "," + hit.getY() + "," + hit.getZ());
            planeIntersections.add(new Vector3(hit));
        }
        if ((GetIntersection(node1.getZ() - box.getZMax(), node2.getZ() - box.getZMax(), node1, node2) && InBox(box, Axis.Z))) {
            LOGGER.debug("Intersected with zMax Plane at " + hit.getX() + "," + hit.getY() + "," + hit.getZ());
            planeIntersections.add(new Vector3(hit));
        }

        return node1.closestVector(planeIntersections);
    }

    /**
     * Checks if there is a plane intersection and sets the intersection point if there is.
     * 
     * @param f1 node1's distribution.
     * @param f2 node2's distribution.
     * @param node1 The start point of the line.
     * @param node2 The end point of the line.
     * @return True if there is a plane intersection, false otherwise.
     */
    private boolean GetIntersection(double f1, double f2, Vector3 node1, Vector3 node2) {
        if ((f1 * f2) >= 0.0) {
            return false;
        }
        hit = Vector3.lerpVectors(node1, node2, -f1 / (f2 - f1));
        return true;
    }

    /**
     * Checks to see if the intersection point is within the box by axis.
     * 
     * @param box The box to check.
     * @param axis The axis to check.
     * @return True if the intersection point is within the box, false otherwise.
     */
    private boolean InBox(Box box, Axis axis) {
        if (axis.equals(Axis.X) && hit.getZ() >= box.getZMin() && hit.getZ() <= box.getZMax() && hit.getY() >= box.getYMin() && hit.getY() <= box.getYMax())
            return true;
        if (axis.equals(Axis.Y) && hit.getZ() >= box.getZMin() && hit.getZ() <= box.getZMax() && hit.getX() >= box.getXMin() && hit.getX() <= box.getXMax())
            return true;
        if (axis.equals(Axis.Z) && hit.getX() >= box.getXMin() && hit.getX() <= box.getXMax() && hit.getY() >= box.getYMin() && hit.getY() <= box.getYMax())
            return true;
        return false;
    }
}
