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

import java.awt.Polygon;

import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * UtilsSpecialEquals is a utility class for specialty equality operators.
 * 
 * @author ablatt
 */
public class UtilsSpecialEquals {

    /**
     * Does a test of o1 and o2 for equality assuming that o1 or o2 could be null. We assume that o1
     * has an appropriate equals method.
     * 
     * @param o1 The first object to test
     * @param o2 The second object to test
     * @return (true/false) are they equal
     */
    public static boolean equalsPossibleNull(Object o1, Object o2) {
        if ((o1 == null) || (o2 == null)) {
            if ((o1 == null) && (o2 == null)) {
                return true;
            }
            else {
                return false;
            }
        }
        else {
            return o1.equals(o2);
        }
    }

    /**
     * Tests equality of 2 polygons specifically examining if the points which make up those 2
     * polygons are the same.
     * 
     * @param p1 A polygon.
     * @param p2 A polygon.
     * @return The points in p1 are the same as the points in p2.
     */
    public static boolean equals(Polygon p1, Polygon p2) {
        if (p1.npoints != p2.npoints) {
            return false;
        }
        else if (p1.npoints == 0) {
            return true;
        }
        else {
            int offset = 0;

            for (; offset < p1.npoints; offset++) {
                if (pointEquals(p1.xpoints[offset], p1.ypoints[offset], p2.xpoints[0], p2.ypoints[0])) {
                    break;
                }
            }

            if (offset == p1.npoints) {
                return false;
            }
            else {
                boolean wasNotFound = false;
                for (int i = 0; i < p1.npoints; i++) {
                    if (!pointEquals(p1.xpoints[(i + offset) % p1.npoints], p1.ypoints[(i + offset) % p1.npoints], p2.xpoints[i], p2.ypoints[i])) {
                        wasNotFound = true;
                    }
                }

                if (wasNotFound) {
                    for (int i = 0; i < p1.npoints; i++) {
                        if (!pointEquals(p1.xpoints[(offset + i + 1) % p1.npoints], p1.ypoints[(offset + i + 1) % p1.npoints], p2.xpoints[p1.npoints - i - 1], p2.ypoints[p1.npoints - i - 1])) {
                            return false;
                        }
                    }
                }

                return true;
            }
        }
    }

    /**
     * Is point (x1, y1) the same as the point (x2, y2)?
     * 
     * @param x1 The x value of the first point.
     * @param y1 The y value of the first point.
     * @param x2 The x value of the second point.
     * @param y2 The y value of the second point.
     * @return If the points are equals.
     */
    public static boolean pointEquals(int x1, int y1, int x2, int y2) {
        return (x1 == x2) && (y1 == y2);
    }

    /**
     * Utility function for determining if the something which is IDable is contained within a
     * sequence of IDable.
     * 
     * @param seq A sequence of IDable elements to check against.
     * @param o The IDable obejct to check is in seq.
     * @return (true/false) o is contained in seq.
     */
    public static boolean idableConatins(Iterable<? extends IDable> seq, IDable o) {
        if (seq == null) {
            return false;
        }

        if (o == null) {
            return false;
        }

        for (IDable s : seq) {
            if (o.equalsId(s)) {
                return true;
            }
        }
        return false;
    }
}
