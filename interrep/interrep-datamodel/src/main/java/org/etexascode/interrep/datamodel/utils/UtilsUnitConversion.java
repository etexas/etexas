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

/**
 * Class to assist in unit conversion. Used www.unitjuggler.com for the values.
 * 
 * @author janway
 * @author ablatt
 * @author ttevendale
 */
public class UtilsUnitConversion {

    private UtilsUnitConversion() {}

    /**
     * Converts the heading from a BSM heading to degrees azimuth.
     * 
     * @param heading The angle of the car in 0.0125 degrees clockwise from North.
     * @return The angle of the car in degrees clockwise from North.
     */
    public static double convertBSMHeadingToDegrees(int heading) {

        return (heading * 0.0125) % 360.0;
    }

    /**
     * Converts cm/sec to ft/sec.
     * 
     * @param centimetersPerSecond The value in cm/sec.
     * @return The value in ft/sec.
     */
    public static double convertCentimetersPerSecondToFeetPerSecond(double centimetersPerSecond) {

        return convertCentimetersToFeet(centimetersPerSecond);
    }

    /**
     * Converts centimeters to feet.
     * 
     * @param centimeters The value in centimeters.
     * @return The value in feet.
     */
    public static double convertCentimetersToFeet(double centimeters) {

        return centimeters * 0.032808398950131;
    }

    /**
     * Converts centimeters to meters.
     * 
     * @param centimeters The value in centimeters.
     * @return The value in meters.
     */
    public static double convertCentimetersToMeters(double centimeters) {

        return centimeters * 0.01;
    }

    /**
     * Converts the heading to a BSM heading.
     * 
     * @param headingClockwiseFromNorth The angle of the car in degrees clockwise from North.
     * @return The angle of the car in 0.0125 degrees clockwise from North.
     */
    public static int convertDegreesToBSMHeading(double headingClockwiseFromNorth) {

        return (int)((headingClockwiseFromNorth % 360.0) / 0.0125);
    }

    /**
     * Converts degrees to 1.5 degrees.
     * 
     * @param degrees The value in degrees.
     * @return The value in 1.5 degrees.
     */
    public static double convertDegreesToOneAndAHalfDegrees(double degrees) {

        return degrees * (2.0 / 3.0);
    }

    /**
     * Converts ft/sec to cm/sec.
     * 
     * @param feetPerSecond The value in ft/sec.
     * @return The value in cm/sec.
     */
    public static double convertFeetPerSecondToCentimetersPerSecond(double feetPerSecond) {

        return convertFeetToCentimeters(feetPerSecond);
    }

    /**
     * Converts feet to centimeters.
     * 
     * @param feet The value in feet.
     * @return The value in centimeters.
     */
    public static double convertFeetToCentimeters(double feet) {

        return feet * 30.48;
    }

    /**
     * Converts values in feet to meters.
     * 
     * @param feet The value in feet.
     * @return The value in meters.
     */
    public static double convertFeetToMeters(double feet) {

        return feet * 0.3048;
    }

    /**
     * Converts meters per second squared to miles per hour squared.
     * 
     * @param metersPerSecondSquared The value in meters per second squared.
     * @return The value in miles per hour squared.
     */
    public static double convertMetersPerSecondSquaredToMilesPerHourSquared(double metersPerSecondSquared) {

        return metersPerSecondSquared * 8052.9706513958;
    }

    /**
     * Converts meters per second to miles per hour.
     * 
     * @param metersPerSecond The value in meters per second.
     * @return The value in miles per hour.
     */
    public static double convertMetersPerSecondToMilesPerHour(double metersPerSecond) {

        return metersPerSecond * 2.2369362920544;
    }

    /**
     * Converts meters per second to 0.02 (1/50) meters per second.
     * 
     * @param metersPerSecond The value in m/s.
     * @return The value in 0.02 m/s.
     */
    public static double convertMetersPerSecondToOneFiftiethMetersPerSecond(double metersPerSecond) {

        return metersPerSecond * 50;
    }

    /**
     * Converts meters per second to 0.01 (1/100) meters per second.
     * 
     * @param metersPerSecond The value in m/s.
     * @return The value in 0.01 m/s.
     */
    public static double convertMetersPerSecondToOneHundredthMetersPerSecond(double metersPerSecond) {

        return metersPerSecond * 100;
    }

    /**
     * Converts meters to centimeters.
     * 
     * @param meters The value in meters.
     * @return The value in centimeters.
     */
    public static double convertMetersToCentimeters(double meters) {

        return meters * 100.0;
    }

    /**
     * Converts meters to feet.
     * 
     * @param meters The value in meters.
     * @return The value in feet.
     */
    public static double convertMetersToFeet(double meters) {

        return meters * 3.2808398950131;
    }

    /**
     * Converts miles per hour squared to meters per second squared.
     * 
     * @param milesPerHourSquared The value in miles per hour squared.
     * @return The value in meters per second squared.
     */
    public static double convertMilesPerHourSquaredToMetersPerSecondSquared(double milesPerHourSquared) {

        return milesPerHourSquared * 0.00012417777777778;
    }

    /**
     * Converts miles per hour to meters per second.
     * 
     * @param milesPerHour The value in miles per hour.
     * @return The value in meters per second.
     */
    public static double convertMilesPerHourToMetersPerSecond(double milesPerHour) {

        return milesPerHour * 0.44704;
    }

    /**
     * Converts milliseconds to seconds.
     * 
     * @param milliseconds The value in milliseconds.
     * @return The value in seconds.
     */
    public static double convertMillisecondsToSeconds(double milliseconds) {

        return milliseconds * 0.001;
    }

    /**
     * Converts 1.5 degrees to degrees.
     * 
     * @param oneAndAHalfDegrees The value in 1.5 degrees.
     * @return The value in degrees.
     */
    public static double convertOneAndAHalfDegreesToDegrees(double oneAndAHalfDegrees) {

        return oneAndAHalfDegrees * 1.5;
    }

    /**
     * Converts 0.02 (1/50) meters per second to meters per second.
     * 
     * @param oneFiftiethMetersPerSecond The value in 0.02 m/s.
     * @return The value in m/s.
     */
    public static double convertOneFiftiethMetersPerSecondToMetersPerSecond(double oneFiftiethMetersPerSecond) {

        return oneFiftiethMetersPerSecond * 0.02;
    }

    /**
     * Converts 0.01 (1/100) meters per second to meters per second.
     * 
     * @param oneHundredthMetersPerSecond The value in 0.02 m/s.
     * @return The value in m/s.
     */
    public static double convertOneHundredthMetersPerSecondToMetersPerSecond(double oneHundredthMetersPerSecond) {

        return oneHundredthMetersPerSecond * 0.01;
    }

    /**
     * Converts one-tenth-seconds to seconds.
     * 
     * @param oneTenthSeconds The value in one-tenth-seconds.
     * @return The value in seconds.
     */
    public static double convertOneTenthSecondsToSeconds(double oneTenthSeconds) {

        return oneTenthSeconds * 0.1;
    }

    /**
     * Converts seconds to milliseconds.
     * 
     * @param seconds The value in seconds.
     * @return The value in milliseconds.
     */
    public static double convertSecondsToMilliseconds(double seconds) {

        return seconds * 1000.0;
    }

    /**
     * Converts seconds to one-tenth-seconds.
     * 
     * @param seconds The value in seconds.
     * @return The value in one-tenth-seconds.
     */
    public static double convertSecondsToOneTenthSeconds(double seconds) {

        return seconds * 10.0;
    }

    /**
     * Converts 1/10th microdegrees into latitude or longitude for use by geolocational services.
     * 
     * @param oneTenthMicroDegrees The value in 1/10th microdegrees.
     * @return The value in normal degrees.
     */
    public static double convertToDegrees(int oneTenthMicroDegrees) {

        // 1/10 microdegrees is 1.0 * 10^-7 degrees
        return oneTenthMicroDegrees * 0.0000001;
    }

    /**
     * Converts a latitude or longitude to 1/10th microdegrees for usage in the BSM and Map Data
     * messages.
     * 
     * @param degrees The latitude or longitude (in degrees we want to convert).
     * @return The 1/10th microdegrees of the angle.
     */
    public static int convertToOneTenthMicrodegree(double degrees) {

        // 1/10 microdegrees is 1.0 * 10^-7 degrees
        return (int)(degrees * 10000000.0);
    }
}
