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
package org.etexascode.test;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Parses the peachtree vehicle trajectory file.
 * 
 * @author janway
 */
public class PeachtreeVehicleParser {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(PeachtreeVehicleParser.class);

    /**
     * The IDs of the lanes in the intersection.
     */
    private static List<Integer> lanes = new LinkedList<Integer>(Arrays.asList(-1, 36, 37, 38, 45, 39, 40, 35, 34, 41, 42, 43, 44, 26, 25, 33, 27, 28, 29));

    /**
     * Counts the number of vehicle entries that are within our bounding box but don't get assigned
     * to a lane.
     */
    private static int numNegOnes = 0;

    /**
     * Reads each line of the vehicle trajectory file and translates it into an NGSimVehicles
     * object.
     * 
     * @param trajectoryFileLocation The location of the input file.
     * @return The list of vehicles.
     */
    public static List<NGSimVehicles> getPeachTreeVehicles(String trajectoryFileLocation) {
        List<NGSimVehicles> ret = new LinkedList<NGSimVehicles>();

        File file = new File(trajectoryFileLocation);

        if (!file.exists()) {
            LOGGER.info("Couldn't find trajectories file.");
        }

        try {
            Scanner scanner = new Scanner(file, "UTF-8");

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                line = line.trim();
                String[] vals = line.split("\\s+");

                int id = new Integer(vals[0]).intValue();
                double x = new Double(vals[4]).doubleValue();
                double y = new Double(vals[5]).doubleValue();
                double speed = new Double(vals[11]).doubleValue(); // feet per second
                speed = speed * 3600.0 / 5280; // miles per hour
                double length = new Double(vals[8]).doubleValue();
                double width = new Double(vals[9]).doubleValue();
                int laneId = new Integer(vals[13]).intValue();
                int direction = new Integer(vals[18]).intValue();
                laneId = calcLaneId(x, y, laneId, direction);

                if (lanes.contains(laneId)) {
                    if (laneId == -1)
                        numNegOnes++;
                    ret.add(new NGSimVehicles(id, x, y, speed, length, width, laneId));
                }

            }
            scanner.close();
        }
        catch (Exception e) {
            LOGGER.info("Caught an exception: {}", e);
        }

        LOGGER.info("Number of vehicles parsed with laneId of -1: {}", numNegOnes);
        LOGGER.info("Number of vehicles parsed total: {}", ret.size());

        return ret;
    }

    /**
     * Determines what lane a vehicle is in. Should match exactly the logic in the python playback
     * parser.
     * 
     * @param x The x offset of the vehicle.
     * @param y The y offset of the vehicle.
     * @param laneId The laneId from the trajectory file.
     * @param direction The direction of the vehicle (Eastbound, etc.)
     * @return The laneId.
     */
    private static int calcLaneId(double x, double y, int laneId, int direction) {

        if (!(-63.74 <= x && 787.0 <= y && 78.1 >= x && 1615.0 >= y)) {
            return -2;
        }

        String str = "";
        if (x > 0.0)
            str += "+,";
        else
            str += "-,";
        if (y > 1240.0)
            str += "+,";
        else
            str += "-,";
        str += direction;
        str += ",";
        str += laneId;

        List<String> rejects = new LinkedList<String>(Arrays.asList("+,+,2,11", "-,-,4,11", "+,-,4,11", "+,+,1,11", "+,+,3,11", "+,+,3,2", "+,-,1,2", "-,+,2,11", "+,-,3,2", "+,-,3,11", "-,-,3,11"));

        if (rejects.contains(str) || laneId > 11)
            return -2;

        int ret = -1;
        ret = (str.equals("+,+,1,1")) || (str.equals("+,-,1,1")) ? 35 : ret;
        ret = (str.equals("+,+,3,1")) || (str.equals("+,-,3,1")) ? 34 : ret;
        ret = (str.equals("+,+,2,2")) || (str.equals("-,+,2,2")) ? 36 : ret;
        ret = (str.equals("+,+,2,1")) || (str.equals("-,+,2,1")) ? 37 : ret;

        ret = (str.equals("+,+,4,11")) || (str.equals("-,+,4,11")) ? 38 : ret;
        ret = (str.equals("+,+,4,1")) || (str.equals("-,+,4,1")) ? 39 : ret;
        ret = (str.equals("+,+,4,2")) || (str.equals("-,+,4,2")) ? 40 : ret;
        ret = (str.equals("-,+,1,2")) || (str.equals("-,-,1,2")) ? 41 : ret;

        ret = (str.equals("-,+,1,1")) || (str.equals("-,-,1,1")) ? 42 : ret;
        ret = (str.equals("-,+,3,1")) || (str.equals("-,-,3,1")) ? 43 : ret;
        ret = (str.equals("-,+,3,2")) || (str.equals("-,-,3,2")) ? 44 : ret;
        ret = (str.equals("+,-,2,2")) || (str.equals("-,-,2,2")) ? 25 : ret;

        ret = (str.equals("+,-,2,1")) || (str.equals("-,-,2,1")) ? 26 : ret;
        ret = (str.equals("+,-,2,11")) || (str.equals("-,-,2,11")) ? 33 : ret;
        ret = (str.equals("+,-,4,1")) || (str.equals("-,-,4,1")) ? 28 : ret;
        ret = (str.equals("+,-,4,2")) || (str.equals("-,-,4,2")) ? 29 : ret;

        return ret;
    }
}
