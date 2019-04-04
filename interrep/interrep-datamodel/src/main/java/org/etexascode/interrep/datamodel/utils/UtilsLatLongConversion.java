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

import java.awt.geom.Point2D;
import java.util.Arrays;

import org.geotoolkit.referencing.GeodeticCalculator;
import org.geotoolkit.referencing.crs.DefaultGeocentricCRS;
import org.geotoolkit.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A class containing only static methods which converts offsets to lat, long pairs and lat, long
 * pairs to offsets. The methods rely on the geotools library, hence all the conversions go through
 * meters. The "drift" caused both by using double values and from mapping a 3D surface to a 2D
 * plane for some calculations has been shown to be less than 1/1,000th of a foot. Output: The
 * starting offset was x = 1200.0 y = 1200.0 in ft. The final offset was x = 1200.0000389217225 y =
 * 1200.0000370650055 in ft.
 * 
 * @author ablatt
 */
public class UtilsLatLongConversion {

    /**
     * Logger for convenience.
     */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(UtilsLatLongConversion.class);

    /**
     * The values used to determine which calculator to use
     */
    public static final int GEODETIC2D = 1;

    public static final int SPHERICAL = 2;

    public static final int GEODETIC3D = 3;

    public static final int CARTESIAN = 4;

    /**
     * The list of the user facing calculator types.
     */
    private static final String[] USER_FACING_CALC_TYPES = new String[] { "Geodetic 2D", "Spherical", "Geodetic 3D", "Cartesian" };

    /**
     * Get the available calculator types.
     * 
     * @return The available calculator types.
     */
    public static String[] getCalcTypes() {
        return Arrays.copyOf(USER_FACING_CALC_TYPES, USER_FACING_CALC_TYPES.length);
    }

    /**
     * Calculates the latitude and longitude of (x, y) where (x, y) is an offset in feet from the
     * reference point as denoted by the startLat and startLong
     * 
     * @param x - the x offset in feet from the reference point
     * @param y - the y offset in feet from the reference point
     * @param startLat - the latitude of the reference point
     * @param startLong - the longitude of the reference point
     * @param type The geographic calculator to use.
     * @return - an array containing the resulting latitude and longitude of the offset point in the
     *         form return[0] = latitude and return[1] = longitude
     */
    public static double[] convertFeetOffsetToLatLong(double x, double y, double startLat, double startLong, int type) {
        return convertMeterOffsetToLatLong(UtilsUnitConversion.convertFeetToMeters(x), UtilsUnitConversion.convertFeetToMeters(y), startLat, startLong, type);
    }

    /**
     * Calculates the latitude and longitude of (x, y) where (x, y) is an offset in meters from the
     * reference point as denoted by the startLat and startLong
     * 
     * @param x - the x offset in meters from the reference point
     * @param y - the y offset in meters from the reference point
     * @param startLat - the latitude of the reference point
     * @param startLong - the longitude of the reference point
     * @param type The geographic calculator to use.
     * @return - an array containing the resulting latitude and longitude of the offset point in the
     *         form return[0] = latitude and return[1] = longitude
     */
    public static double[] convertMeterOffsetToLatLong(double x, double y, double startLat, double startLong, int type) {
        /*
         * Note: ablatt - the geodetic calculator will not accept a negative x or y (it would
         * appear). As such, it will be necessary to alter the direction to compensate for this
         * issue. Note: ablatt - the geodetic calculator expects the angle to be between +180 and
         * -180.
         */

        // transform startLat
        GeodeticCalculator gc = new GeodeticCalculator(getCoordinateReferenceSystem(type));
        gc.setStartingGeographicPoint(startLong, startLat);

        if (y < 0) {
            gc.setDirection(180, -y);
        }
        else {
            gc.setDirection(0, y);
        }
        Point2D r = gc.getDestinationGeographicPoint();

        // transform startLong
        gc = new GeodeticCalculator(); // reinitialize to (hopefully) prevent any unwanted
                                       // calculations
        gc.setStartingGeographicPoint(r);

        if (x < 0) {
            gc.setDirection(-90, -x);
        }
        else {
            gc.setDirection(90, x);
        }
        // r = gc.getDestinationGeographicPoint();
        Point2D s = gc.getDestinationGeographicPoint();

        // build return
        double[] ret = new double[2];
        ret[0] = s.getY();
        ret[1] = s.getX();

        return ret;
    }

    /**
     * Calculates the latitude and longitude of (x, y) where (x, y) is an offset in centimeters from
     * the reference point as denoted by the startLat and startLong
     * 
     * @param x - the x offset in centimeters from the reference point
     * @param y - the y offset in centimeters from the reference point
     * @param startLat - the latitude of the reference point
     * @param startLong - the longitude of the reference point
     * @param type The geographic calculator to use.
     * @return - an array containing the resulting latitude and longitude of the offset point in the
     *         form return[0] = latitude and return[1] = longitude
     */
    public static double[] convertCentimeterOffsetToLatLong(double x, double y, double startLat, double startLong, int type) {
        return convertMeterOffsetToLatLong(UtilsUnitConversion.convertCentimetersToMeters(x), UtilsUnitConversion.convertCentimetersToMeters(y), startLat, startLong, type);
    }

    /**
     * Convert a pair of points denoted in lat, longs into an x, y offset from the first pair to the
     * second pair Conversion should be in meters This method should reverse
     * convertMeterOffsetToLatLong
     * 
     * @param lat1 - the latitude of the first point (should be the reference point)
     * @param long1 - the longitude of the first point (should be the reference point)
     * @param lat2 - the latitude of the second point
     * @param long2 - the longitude of the second point
     * @param type The geographic calculator to use.
     * @return - the x, y offset of the second point from the first point in meters in the form
     *         return[0] = x, return[1] = y
     */
    public static double[] convertLatLongToMeterOffset(double lat1, double long1, double lat2, double long2, int type) {
        // TODO: ablatt - Junit test this method, need to determine whether option 1 is valid and
        // complete or option 2 is necessary
        // -- can use the cases outlined in option 2 as a basis of the junit tests
        GeodeticCalculator gc = new GeodeticCalculator(getCoordinateReferenceSystem(type));
        double[] ret = new double[2];

        // find the distance and the angle
        gc.setStartingGeographicPoint(long1, lat1);
        gc.setDestinationGeographicPoint(long2, lat2);

        double theta = gc.getAzimuth();
        double hypotenuse = gc.getOrthodromicDistance();

        // theta is in degrees clockwise from north.
        // sin and cos expect angles in degrees counterclockwise from east
        // convertedAngle = 90.0 - theta;
        // Co-Function Identities: sin(90.0 - theta) = cos(theta); cos(90.0 - theta) = sin(theta);
        // So using the opposite function for x and y is correct because of the rotation and
        // direction reversal.

        ret[0] = hypotenuse * Math.sin(Math.toRadians(theta)); // calculate x in meters
        ret[1] = hypotenuse * Math.cos(Math.toRadians(theta)); // calculate y in meters

        return ret;
    }

    /**
     * Convert a pair of points denoted in lat, longs into an x, y offset from the first pair to the
     * second pair Conversion should be in centimeters This method should reverse
     * convertCentimeterOffsetToLatLong
     * 
     * @param lat1 - the latitude of the first point (should be the reference point)
     * @param long1 - the longitude of the first point (should be the reference point)
     * @param lat2 - the latitude of the second point
     * @param long2 - the longitude of the second point
     * @param type The geographic calculator to use.
     * @return - the x, y offset of the second point from the first point in centimeters in the form
     *         return[0] = x, return[1] = y
     */
    public static double[] convertLatLongToCentimeterOffset(double lat1, double long1, double lat2, double long2, int type) {
        double[] ret = convertLatLongToMeterOffset(lat1, long1, lat2, long2, type);

        ret[0] = UtilsUnitConversion.convertMetersToCentimeters(ret[0]);
        ret[1] = UtilsUnitConversion.convertMetersToCentimeters(ret[1]);

        return ret;
    }

    /**
     * Convert a pair of points denoted in lat, longs into an x, y offset from the first pair to the
     * second pair Conversion should be in feet This method should reverse
     * convertFeetOffsetToLatLong
     * 
     * @param lat1 - the latitude of the first point (should be the reference point)
     * @param long1 - the longitude of the first point (should be the reference point)
     * @param lat2 - the latitude of the second point
     * @param long2 - the longitude of the second point
     * @param type The geographic calculator to use.
     * @return - the x, y offset of the second point from the first point in feet in the form
     *         return[0] = x, return[1] = y
     */
    public static double[] convertLatLongToFeetOffset(double lat1, double long1, double lat2, double long2, int type) {
        double[] ret = convertLatLongToMeterOffset(lat1, long1, lat2, long2, type);

        ret[0] = UtilsUnitConversion.convertMetersToFeet(ret[0]);
        ret[1] = UtilsUnitConversion.convertMetersToFeet(ret[1]);

        return ret;
    }

    /*
     * Notes: Below is a list of easily available coordinate systems to use in this application:
     * WGS84 -- standard for the geodetic calculator -- DefaultGeographicCRS WGS84_3D -- takes into
     * account height from the surface -- DefaultGeographicCRS Cartesian -- The X axis points
     * towards the prime meridian. The Y axis points East. The Z axis points North. --
     * DefaultDeocentricCRS Spherical -- DefaultDeocentricCRS Cartesian_2D -- DefaultEngineeringCRS
     * Cartesian_3D -- DefaultEngineeringCRS Generic_2D -- DefaultEngineeringCRS Generic_3D --
     * DefaultEngineeringCRS
     */

    private static CoordinateReferenceSystem getCoordinateReferenceSystem(int calcType) {
        switch (calcType) {
            case GEODETIC2D:
                return DefaultGeographicCRS.WGS84;
            case GEODETIC3D:
                return DefaultGeographicCRS.WGS84_3D;
            case SPHERICAL:
                return DefaultGeocentricCRS.SPHERICAL;
            case CARTESIAN:
                return DefaultGeocentricCRS.CARTESIAN;
            default:
                throw new RuntimeException(calcType + " is not a valid Geographic Coordinate Reference System enumerated type");
        }
    }

    /**
     * Converts the String into an integer representation to use in the conversion methods in this
     * class
     * 
     * @param type The type of calculator to be translated into an int
     * @return The type of calculator to be used in etexas-api
     */
    public static int convertCalculatorType(String type) {
        if (type.equals(USER_FACING_CALC_TYPES[0])) {
            return GEODETIC2D;
        }
        else if (type.equals(USER_FACING_CALC_TYPES[1])) {
            return SPHERICAL;
        }
        else if (type.equals(USER_FACING_CALC_TYPES[2])) {
            return GEODETIC3D;
        }
        else if (type.equals(USER_FACING_CALC_TYPES[3])) {
            return CARTESIAN;
        }
        else {
            throw new RuntimeException(type + " is not a valid type");
        }
    }

    /**
     * Reverses convertCalculatorType(String)
     * 
     * @param type The type of calculator to be used in etexas-api.
     * @return The String representation used in the webapp.
     */
    public static String convertCalculatorType(int type) {
        switch (type) {
            case GEODETIC2D:
                return USER_FACING_CALC_TYPES[0];
            case GEODETIC3D:
                return USER_FACING_CALC_TYPES[2];
            case SPHERICAL:
                return USER_FACING_CALC_TYPES[1];
            case CARTESIAN:
                return USER_FACING_CALC_TYPES[3];
            default:
                throw new RuntimeException(type + " is not a valid type");
        }
    }

    /**
     * Converts values in feet to centimeters
     * 
     * @param inFeet - the value in feet
     * @return - the value in centimeters
     */
    public static double convertFeetToCentimeters(double inFeet) {
        return inFeet * 30.48; // Value according to Google's length conversion tool
    }
}
