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
package org.etexascode.interrep.datamodel.xmladapters;

/**
 * This class exists only to allow JaxB to read/write the java Polygon class
 * 
 * @author bmauldon
 */
public class EtexasPolygon {

    /**
     * the x,y coordinates and the number of points for the polygon.
     */
    private int[] x, y;

    private int n;

    /**
     * Default constructor
     */
    public EtexasPolygon() {
        this(new int[] { 0 }, new int[] { 0 }, 0);
    }

    /**
     * Constructor
     * 
     * @param x array of x coordinates for points in polygon
     * @param y array of x coordinates for points in polygon
     * @param n number of points in polygon
     */
    public EtexasPolygon(int[] x, int[] y, int n) {
        this.x = x.clone();
        this.y = y.clone();
        this.n = n;
    }

    /**
     * Get x coordinates for polygon
     * 
     * @return x coordinates
     */
    public int[] getX() {
        return x.clone();
    }

    /**
     * Set x coordinates for polygon
     * 
     * @param x coordinates
     */
    public void setX(int[] x) {
        this.x = x.clone();
    }

    /**
     * Get y coordinates for polygon
     * 
     * @return y coordinates
     */
    public int[] getY() {
        return y.clone();
    }

    /**
     * Set y coordinates for polygon
     * 
     * @param y coordinates
     */
    public void setY(int[] y) {
        this.y = y.clone();
    }

    /**
     * Get number of points in polygon
     * 
     * @return n number of points in polygon
     */
    public int getN() {
        return n;
    }

    /**
     * Set number of points in polygon
     * 
     * @param n number of points in polygon
     */
    public void setN(int n) {
        this.n = n;
    }

}
