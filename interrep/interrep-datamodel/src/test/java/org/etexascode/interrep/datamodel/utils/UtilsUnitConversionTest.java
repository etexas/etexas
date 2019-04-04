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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Uses http://www.unitjuggler.com/ for conversion values
 * 
 * @author ttevendale
 */
public class UtilsUnitConversionTest {

    /** tolerance */
    private final double DELTA = 0.00001;

    @Test
    public void testConvertBSMHeadingToDegrees() {

        int bsmHeadingInputPrinciple = 3600;
        int bsmHeadingInputNonPrinciple = 61200;

        double bsmHeadingOutput = 45.0;

        assertEquals(bsmHeadingOutput, UtilsUnitConversion.convertBSMHeadingToDegrees(bsmHeadingInputPrinciple), DELTA);
        assertEquals(bsmHeadingOutput, UtilsUnitConversion.convertBSMHeadingToDegrees(bsmHeadingInputNonPrinciple), DELTA);
    }

    @Test
    public void testConvertCentimetersPerSecondToFeetPerSecond() {

        assertEquals(405.08530183727, UtilsUnitConversion.convertCentimetersPerSecondToFeetPerSecond(12347), DELTA);

        int num = 114457;
        assertEquals(num, UtilsUnitConversion.convertFeetPerSecondToCentimetersPerSecond(UtilsUnitConversion.convertCentimetersPerSecondToFeetPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertCentimetersToFeet() {

        assertEquals(1090.5839895013, UtilsUnitConversion.convertCentimetersToFeet(33241), DELTA);

        int num = 8711;
        assertEquals(num, UtilsUnitConversion.convertFeetToCentimeters(UtilsUnitConversion.convertCentimetersToFeet(num)), DELTA);
    }

    @Test
    public void testConvertCentimetersToMeters() {

        assertEquals(8798.41, UtilsUnitConversion.convertCentimetersToMeters(879841), DELTA);

        int num = -1841;
        assertEquals(num, UtilsUnitConversion.convertMetersToCentimeters(UtilsUnitConversion.convertCentimetersToMeters(num)), DELTA);
    }

    @Test
    public void testConvertDegreesToBSMHeading() {

        double headingInputPrinciple = 45.0;
        double headingInputNonPrinciple = 765.0;
        int headingOutput = 3600;

        assertEquals(headingOutput, UtilsUnitConversion.convertDegreesToBSMHeading(headingInputPrinciple), DELTA);
        assertEquals(headingOutput, UtilsUnitConversion.convertDegreesToBSMHeading(headingInputNonPrinciple), DELTA);
    }

    @Test
    public void testConvertDegreesToOneAndAHalfDegrees() {

        assertEquals(10, UtilsUnitConversion.convertDegreesToOneAndAHalfDegrees(15), DELTA);

        int num = 514847;
        assertEquals(num, UtilsUnitConversion.convertOneAndAHalfDegreesToDegrees(UtilsUnitConversion.convertDegreesToOneAndAHalfDegrees(num)), DELTA);
    }

    @Test
    public void testConvertFeetPerSecondToCentimetersPerSecond() {

        assertEquals(112684.56, UtilsUnitConversion.convertFeetPerSecondToCentimetersPerSecond(3697), DELTA);

        int num = -78784;
        assertEquals(num, UtilsUnitConversion.convertCentimetersPerSecondToFeetPerSecond(UtilsUnitConversion.convertFeetPerSecondToCentimetersPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertFeetToCentimeters() {

        assertEquals(30495971.52, UtilsUnitConversion.convertFeetToCentimeters(1000524), DELTA);

        double num = -232.1854;
        assertEquals(num, UtilsUnitConversion.convertCentimetersToFeet(UtilsUnitConversion.convertFeetToCentimeters(num)), DELTA);
    }

    @Test
    public void testConvertFeetToMeters() {

        assertEquals(-27048.2568, UtilsUnitConversion.convertFeetToMeters(-88741), DELTA);

        int num = 124;
        assertEquals(num, UtilsUnitConversion.convertMetersToFeet(UtilsUnitConversion.convertFeetToMeters(num)), DELTA);
    }

    @Test
    public void testConvertMetersPerSecondSquaredToMilesPerHourSquared() {

        assertEquals(185218.3249821, UtilsUnitConversion.convertMetersPerSecondSquaredToMilesPerHourSquared(23), DELTA);

        int num = 5;
        assertEquals(num, UtilsUnitConversion.convertMilesPerHourSquaredToMetersPerSecondSquared(UtilsUnitConversion.convertMetersPerSecondSquaredToMilesPerHourSquared(num)), DELTA);
    }

    @Test
    public void testConvertMetersPerSecondToMilesPerHour() {

        assertEquals(1221.3672154617, UtilsUnitConversion.convertMetersPerSecondToMilesPerHour(546), DELTA);

        int num = 2;
        assertEquals(num, UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(UtilsUnitConversion.convertMetersPerSecondToMilesPerHour(num)), DELTA);
    }

    @Test
    public void testConvertMetersPerSecondToOneFiftiethMetersPerSecond() {

        assertEquals(750, UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(15), DELTA);

        int num = 585484;
        assertEquals(num, UtilsUnitConversion.convertOneFiftiethMetersPerSecondToMetersPerSecond(UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertMetersPerSecondToOneHundredthMetersPerSecond() {

        assertEquals(2500, UtilsUnitConversion.convertMetersPerSecondToOneHundredthMetersPerSecond(25), DELTA);

        int num = -787451;
        assertEquals(num, UtilsUnitConversion.convertOneHundredthMetersPerSecondToMetersPerSecond(UtilsUnitConversion.convertMetersPerSecondToOneHundredthMetersPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertMetersToCentimeters() {

        assertEquals(1574100, UtilsUnitConversion.convertMetersToCentimeters(15741), DELTA);

        int num = 99987;
        assertEquals(num, UtilsUnitConversion.convertCentimetersToMeters(UtilsUnitConversion.convertMetersToCentimeters(num)), DELTA);
    }

    @Test
    public void testConvertMetersToFeet() {

        assertEquals(1679376.6404199, UtilsUnitConversion.convertMetersToFeet(511874), DELTA);

        int num = 474135;
        assertEquals(num, UtilsUnitConversion.convertFeetToMeters(UtilsUnitConversion.convertMetersToFeet(num)), DELTA);
    }

    @Test
    public void testConvertMilesPerHourSquaredToMetersPerSecondSquared() {

        assertEquals(1.2417777777778, UtilsUnitConversion.convertMilesPerHourSquaredToMetersPerSecondSquared(10000), DELTA);

        int num = 1234567;
        assertEquals(num, UtilsUnitConversion.convertMetersPerSecondSquaredToMilesPerHourSquared(UtilsUnitConversion.convertMilesPerHourSquaredToMetersPerSecondSquared(num)), DELTA);
    }

    @Test
    public void testConvertMilesPerHourToMetersPerSecond() {

        assertEquals(434.07584, UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(971), DELTA);

        int num = -15;
        assertEquals(num, UtilsUnitConversion.convertMetersPerSecondToMilesPerHour(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertMillisecondsToSeconds() {

        assertEquals(0.648, UtilsUnitConversion.convertMillisecondsToSeconds(648), DELTA);

        int num = 22247;
        assertEquals(num, UtilsUnitConversion.convertSecondsToMilliseconds(UtilsUnitConversion.convertMillisecondsToSeconds(num)), DELTA);
    }

    @Test
    public void testConvertOneAndAHalfDeggresToDegrees() {

        assertEquals(4822.5, UtilsUnitConversion.convertOneAndAHalfDegreesToDegrees(3215), DELTA);

        int num = 194847;
        assertEquals(num, UtilsUnitConversion.convertDegreesToOneAndAHalfDegrees(UtilsUnitConversion.convertOneAndAHalfDegreesToDegrees(num)), DELTA);
    }

    @Test
    public void testConvertOneFiftiethMetersPerSecondToMetersPerSecond() {

        assertEquals(-16.3, UtilsUnitConversion.convertOneFiftiethMetersPerSecondToMetersPerSecond(-815), DELTA);

        int num = 8484;
        assertEquals(num, UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(UtilsUnitConversion.convertOneFiftiethMetersPerSecondToMetersPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertOneHundredthMetersPerSecondToMetersPerSecond() {

        assertEquals(98.25, UtilsUnitConversion.convertOneHundredthMetersPerSecondToMetersPerSecond(9825), DELTA);

        int num = -66232;
        assertEquals(num, UtilsUnitConversion.convertMetersPerSecondToOneHundredthMetersPerSecond(UtilsUnitConversion.convertOneHundredthMetersPerSecondToMetersPerSecond(num)), DELTA);
    }

    @Test
    public void testConvertOneTenthSecondsToSeconds() {

        assertEquals(151.4, UtilsUnitConversion.convertOneTenthSecondsToSeconds(1514), DELTA);

        double num = 23.283;
        assertEquals(num, UtilsUnitConversion.convertSecondsToOneTenthSeconds(UtilsUnitConversion.convertOneTenthSecondsToSeconds(num)), DELTA);
    }

    @Test
    public void testConvertSecondsToMilliseconds() {

        assertEquals(112120, UtilsUnitConversion.convertSecondsToMilliseconds(112.12), DELTA);

        double num = 22247.8874;
        assertEquals(num, UtilsUnitConversion.convertMillisecondsToSeconds(UtilsUnitConversion.convertSecondsToMilliseconds(num)), DELTA);
    }

    @Test
    public void testConvertSecondsToOneTenthSeconds() {

        assertEquals(1141.18, UtilsUnitConversion.convertSecondsToOneTenthSeconds(114.118), DELTA);

        double num = 88.123;
        assertEquals(num, UtilsUnitConversion.convertOneTenthSecondsToSeconds(UtilsUnitConversion.convertSecondsToOneTenthSeconds(num)), DELTA);
    }

    @Test
    public void testConvertToDegrees() {

        assertEquals(2.1547711, UtilsUnitConversion.convertToDegrees(21547711), DELTA);

        int num = 215487544;
        assertEquals(num, UtilsUnitConversion.convertToOneTenthMicrodegree(UtilsUnitConversion.convertToDegrees(num)), DELTA);
    }

    @Test
    public void testConvertToOneTenthMicrodegree() {

        assertEquals(12441, UtilsUnitConversion.convertToOneTenthMicrodegree(0.0012441), DELTA);

        double num = 5.0001541;
        assertEquals(num, UtilsUnitConversion.convertToDegrees(UtilsUnitConversion.convertToOneTenthMicrodegree(num)), DELTA);
    }
}
