/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */

package org.etexascode.api;

import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;

/**
 * A class used to contain static information about the configuration of the intersection model.
 * Part of the Public TEXAS Model Java API
 * 
 * @author bbadillo
 * @author ablatt
 */
public class ModelData {

    /**
     * Object that retrieves the map data from SIMPRO.
     */
    MapDataRetriever mapData = new MapDataRetriever();

    /**
     * Object that retrieves the simulation data from SIMPRO.
     */
    SimDataRetriever simData = new SimDataRetriever();

    /**
     * centerX and centerY are the distance from (latitude, longitude) to the center of the lane
     */
    double centerX;

    double centerY;

    /**
     * the latitude and longitude of the center of the model (or the spatial values of centerX,
     * centerY)
     */
    double latitude = 0.0;

    double longitude = 0.0;

    /**
     * the min and max values of each lane (creates a bounding box for all the lanes in the model)
     */
    double xMin;

    double xMax;

    double yMin;

    double yMax;

    /**
     * The size of a DT in seconds.
     */
    double dtSize;

    /**
     * The maximum number of DTs in the simulation.
     */
    long maxDT;

    /**
     * The calculator type to be used in lat,long conversions.
     */
    int geoCalculatorType = UtilsLatLongConversion.GEODETIC2D;

    public void initialize() {

        // init
        int numLanes = mapData.getNRLAN();
        double minX, maxX, minY, maxY;
        maxX = mapData.getBASELX(1);
        minX = maxX;
        maxY = mapData.getBASELY(1);
        minY = maxY;

        // find the mins and maxs
        for (int i = 1; i <= numLanes; i++) {
            double tmp = mapData.getBASELX(i);

            if (minX > tmp) {
                minX = tmp;
            }
            else if (maxX < tmp) {
                maxX = tmp;
            }

            tmp = mapData.getENDLNX(i);

            if (minX > tmp) {
                minX = tmp;
            }
            else if (maxX < tmp) {
                maxX = tmp;
            }

            tmp = mapData.getBASELY(i);

            if (minY > tmp) {
                minY = tmp;
            }
            else if (maxY < tmp) {
                maxY = tmp;
            }

            tmp = mapData.getENDLNY(i);

            if (minY > tmp) {
                minY = tmp;
            }
            else if (maxY < tmp) {
                maxY = tmp;
            }
        }

        // set the mins and maxs for the model
        xMin = minX;
        xMax = maxX;
        yMin = minY;
        yMax = maxY;

        // set the DT data
        dtSize = simData.getDT();
        if (dtSize > 0.0) {
            maxDT = Math.round(simData.getTotalSimTime() / dtSize);
        }
        else {
            maxDT = Math.round(simData.getTotalSimTime());
        }

        // calculate and set the centers
        centerX = (maxX - minX) / 2 + minX;
        centerY = (maxY - minY) / 2 + minY;

        UtilsLatLongConversion.convertCentimeterOffsetToLatLong(centerX, centerY, 0.0, 0.0, geoCalculatorType);
    }

    public double getCenterX() {
        return centerX;
    }

    public double getCenterY() {
        return centerY;
    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    MapDataRetriever getMapData() {
        return mapData;
    }

    public double getXMin() {
        return xMin;
    }

    public double getXMax() {
        return xMax;
    }

    public double getYMin() {
        return yMin;
    }

    public double getYMax() {
        return yMax;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public int getGeoCalculatorType() {
        return geoCalculatorType;
    }

    public void setGeoCalculatorType(int geoCalculatorType) {
        this.geoCalculatorType = geoCalculatorType;
    }

    /**
     * Get the duration (in seconds) of each step in time in the simulation.
     * 
     * @return The duration (in seconds) of each step in time in the simulation.
     */
    public double getDTSize() {
        return this.dtSize;
    }

    /**
     * Get the total number of DTs in the TEXAS Model simulation.
     * 
     * @return The total of DTs in the TEXAS Model simulation.
     */
    public long getMaxDT() {
        return this.maxDT;
    }
}
