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

/**
 * The class to represent a box.
 * 
 * @author ttevendale
 */
public class Box {

    /**
     * The minimum x coordinate of the box.
     */
    private double xMin;

    /**
     * The minimum y coordinate of the box.
     */
    private double yMin;

    /**
     * The minimum z coordinate of the box.
     */
    private double zMin;

    /**
     * The maximum x coordinate of the box.
     */
    private double xMax;

    /**
     * The maximum y coordinate of the box.
     */
    private double yMax;

    /**
     * The maximum z coordinate of the box.
     */
    private double zMax;

    /**
     * Holds the logic for bounding box calculations.
     */
    private BoundingBoxCalculation calc = new BoundingBoxCalculation();

    /**
     * Constructor
     * 
     * @param box The box to construct this box.
     */
    public Box(Box box) {
        this(box.xMin, box.yMin, box.zMin, box.xMax, box.yMax, box.zMax);
    }

    /**
     * Constructor
     * 
     * @param xMin The minimum x coordinate of the box.
     * @param yMin The minimum y coordinate of the box.
     * @param zMin The minimum z coordinate of the box.
     * @param xMax The maximum x coordinate of the box.
     * @param yMax The maximum y coordinate of the box.
     * @param zMax The maximum z coordinate of the box.
     */
    public Box(double xMin, double yMin, double zMin, double xMax, double yMax, double zMax) {
        this.xMin = xMin;
        this.yMin = yMin;
        this.zMin = zMin;
        this.xMax = xMax;
        this.yMax = yMax;
        this.zMax = zMax;
    }

    /**
     * Finds the intersection point from the source to destination with this box if it exists.
     * 
     * @param source The source node.
     * @param destination The destination node.
     * @return The intersection vector or null if it doesn't exist.
     */
    public Vector3 lineIntersection(Vector3 source, Vector3 destination) {
        return calc.lineIntersection(this, source, destination);
    }

    /**
     * Checks if the point is contained in the box.
     * 
     * @param vector The point to check.
     * @return True if the point is within the box, false otherwise.
     */
    public boolean contains(Vector3 vector) {

        if (this.xMin <= vector.getX() && this.xMax >= vector.getX()
                && this.yMin <= vector.getY() && this.yMax >= vector.getY()
                && this.zMin <= vector.getZ() && this.zMax >= vector.getZ()) {

            return true;
        }
        else {

            return false;
        }
    }

    /**
     * Gets minimum x coordinate.
     * 
     * @return the minimum x coordinate.
     */
    public double getXMin() {

        return xMin;
    }

    /**
     * Gets minimum y coordinate.
     * 
     * @return the minimum y coordinate.
     */
    public double getYMin() {

        return yMin;
    }

    /**
     * Gets minimum z coordinate.
     * 
     * @return the minimum z coordinate.
     */
    public double getZMin() {

        return zMin;
    }

    /**
     * Gets maximum x coordinate.
     * 
     * @return the minimum x coordinate.
     */
    public double getXMax() {

        return xMax;
    }

    /**
     * Gets maximum y coordinate.
     * 
     * @return the minimum y coordinate.
     */
    public double getYMax() {

        return yMax;
    }

    /**
     * Gets maximum z coordinate.
     * 
     * @return the minimum z coordinate.
     */
    public double getZMax() {

        return zMax;
    }

}
