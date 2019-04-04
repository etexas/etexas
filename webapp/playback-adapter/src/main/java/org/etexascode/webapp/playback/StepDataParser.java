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
package org.etexascode.webapp.playback;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class that parses files and strings for stepData, Vehicle, detectorEvent, and SignalIndication
 * data types.
 * 
 * @author janway
 * @author bmauldon
 * @author ablatt
 */
public class StepDataParser {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(StepDataParser.class);

    protected static StepData getStepData(long stepNum, File stepData) {
        File step = new File(stepData, stepNum + ".txt");
        String contents = "";

        FileInputStream fis = null;
        try {
            fis = new FileInputStream(step);
            byte[] tmp = new byte[fis.available()];
            fis.read(tmp);
            contents = UtilsStringOnModel.decodeUTF8(tmp);
            tmp = null;
            fis.close();
        }
        catch (FileNotFoundException e) {
            LOGGER.error("Exception", e);
            throw new RuntimeException(stepNum + " is not a valid step number");
        }
        catch (IOException e) {
            LOGGER.error("Exception", e);
            throw new RuntimeException(stepNum + ".txt is malformed");
        }
        finally {
            try {
                if (fis != null) {
                    fis.close();
                }
            }
            catch (IOException e) {
                LOGGER.error("Exception", e);
            }
        }

        String[] split = contents.split("\n==section separator==\n");
        contents = null;
        StepData ret = new StepData();

        for (String s : split) {
            String[] tmp2 = s.split("\n");
            if (tmp2[0].equals("vehicle")) {
                ret.addVehicle(parseVehicle(tmp2));
            }
            else if (tmp2[0].equals("detector")) {
                ret.addDetectorEvent(parseDetectorEvent(tmp2));
            }
            else if (tmp2[0].equals("signal")) {
                ret.addSignalIndication(parseSignalIndication(tmp2));
            }
            else {
                System.out.println("section type " + tmp2[0] + " does not exist");
                throw new RuntimeException("invalid section type");
            }
        }

        return ret;
    }

    private static Vehicle parseVehicle(String[] split) {
        Vehicle ret = new Vehicle(UtilsPlaybackParsers.parseInt(split[1]), UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(split[9])),
                UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(split[10])), UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(split[3])),
                UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(split[4])), 0.0);

        ret.setSpeed(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(UtilsPlaybackParsers.parseDouble(split[7])));
        ret.setAcceleration(UtilsUnitConversion.convertMilesPerHourSquaredToMetersPerSecondSquared(UtilsPlaybackParsers.parseDouble(split[8])));

        ret.setLaneID(UtilsPlaybackParsers.parseInt(split[11]));

        return ret;
    }

    private static DetectorEvent parseDetectorEvent(String[] split) {
        DetectorEvent ret = new DetectorEvent();

        ret.setDetectorId(UtilsPlaybackParsers.parseInt(split[1]));
        ret.setSpeed(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(UtilsPlaybackParsers.parseDouble(split[2])));
        ret.setLength(UtilsUnitConversion.convertFeetToCentimeters(UtilsPlaybackParsers.parseDouble(split[3])));
        ret.setPresence(true);

        return ret;
    }

    private static SignalIndication parseSignalIndication(String[] split) {
        SignalIndication ret = new SignalIndication();

        ret.setLaneId(UtilsPlaybackParsers.parseInt(split[1]));
        ret.setColorIndication(parseColor(split[2]));
        ret.setTypeIndication(parseType(split[2]));

        if (split.length > 3) {
            ret.setTimeToChange(UtilsPlaybackParsers.parseDouble(split[3]) / 1000.0); // parse
            // milliseconds
            // and
            // convert
            // to
            // seconds
        }
        ret.setStateIndication(SignalIndication.State.STEADY); // TODO: ablatt -
        // is it right
        // to make this
        // assumption?

        return ret;
    }

    private static SignalIndication.Color parseColor(String line) {
        String color = line.split(":")[1].split("-")[0];

        if (color.equalsIgnoreCase("green")) {
            return SignalIndication.Color.GREEN;
        }
        else if (color.equalsIgnoreCase("red")) {
            return SignalIndication.Color.RED;
        }
        else if (color.equalsIgnoreCase("yellow")) {
            return SignalIndication.Color.YELLOW;
        }
        else if (color.equalsIgnoreCase("none")) {
            return SignalIndication.Color.NONE;
        }
        else {
            System.out.println(line + " is not properly formatted or does not have a correct color.");
            throw new RuntimeException("Colors must be: green, yellow, red or none (case doesn't matter)");
        }
    }

    private static SignalIndication.Type parseType(String line) {
        String type = line.split(":")[1].split("-")[1];

        if (type.equalsIgnoreCase("ball")) {
            return SignalIndication.Type.BALL;
        }
        else if (type.equalsIgnoreCase("left_arrow")) {
            return SignalIndication.Type.LEFT_ARROW;
        }
        else if (type.equalsIgnoreCase("right_arrow")) {
            return SignalIndication.Type.RIGHT_ARROW;
        }
        else if (type.equalsIgnoreCase("straight_arrow")) {
            return SignalIndication.Type.STRAIGHT_ARROW;
        }
        else if (type.equalsIgnoreCase("uturn_arrow")) {
            return SignalIndication.Type.UTURN_ARROW;
        }
        else if (type.equalsIgnoreCase("stop_sign")) {
            return SignalIndication.Type.STOP_SIGN;
        }
        else if (type.equalsIgnoreCase("yield_sign")) {
            return SignalIndication.Type.YIELD_SIGN;
        }
        else {
            System.out.println(line + " is not a properly formated type");
            throw new RuntimeException("Types must be: ball, left_arrow, right_arrow, straight_arrow, uturn_arrow, stop_sign, yield_sign (case doesn't matter)");
        }
    }
}
