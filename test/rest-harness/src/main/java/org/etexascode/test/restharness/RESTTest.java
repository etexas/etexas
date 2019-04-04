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
package org.etexascode.test.restharness;

import java.io.File;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

/**
 * A program which is configured after compilation.
 * 
 * @author ablatt
 */
public class RESTTest {

    /**
     * Main method for testing.
     * 
     * @param args Name value pairs.
     */
    public static void main(String[] args) throws URISyntaxException, JAXBException {

        // setup default arguments
        Map<String, String> options = new HashMap<String, String>();

        options.put("user", "Unauthenticated");
        options.put("numReps", "1");
        options.put("hostLocation", "localhost");
        options.put("projName", "proj1");
        options.put("keysToDownload", "Queue Length MOE,Total Phase Failures,Queue Delay - Total");
        options.put("stepsAtOnce", "1");
        options.put("printOut", "true");
        options.put("rootDir", "/home/ablatt/rest-res");

        for (String arg : args) {
            String[] tmp = parseArg(arg);
            if (tmp != null) {
                String key = tmp[0].substring(2); // remove the first 2 characters
                if (options.containsKey(key)) {
                    options.put(key, tmp[1]);
                }
                else {
                    System.out.println(key + "is not a valid option");
                }
            }
        }

        /*
         * RESTHarness.executeProject(options.get("hostLocation"), options.get("projName"),
         * options.get("user"), options.get("keysToDownload").split(","),
         * parseRootDir(options.get("rootDir")), parseStepsAtOnce(options.get("stepsAtOnce")),
         * parseNumReps(options.get("numReps")), parsePrintOut(options.get("printOut")));
         */

        /*
         * RESTServiceTests.testAllFunctions(options.get("hostLocation"), options.get("user"));
         * RESTSimulationTest.testAllFunctions(options.get("hostLocation"), options.get("user"));
         * RESTSimulationTest.testServerFileOverloading(options .get("hostLocation"));
         */
    }

    /**
     * Parse an argument into a key value pair.
     * 
     * @param arg The argument.
     * @return The argument.
     */
    public static String[] parseArg(String arg) {
        if (arg.contains("=")) {
            return arg.split("=");
        }

        return null;
    }

    /**
     * Generate the root directory file. Throw an expection is there is a problem with the root
     * directory file (does not exist, is not a folder).
     * 
     * @param dirLoc
     * @return The root directory.
     */
    private static File parseRootDir(String dirLoc) {
        File ret = new File(dirLoc);

        if (!ret.exists()) {
            throw new RuntimeException("rootDir does not exist");
        }

        if (!ret.isDirectory()) {
            throw new RuntimeException("rootDir is not a directory");
        }

        return ret;
    }

    /**
     * Function for containing the error generation to an understandable place.
     * 
     * @param stepsAtOnce
     * @return The number of steps at once.
     */
    private static int parseStepsAtOnce(String stepsAtOnce) {
        return Integer.parseInt(stepsAtOnce);
    }

    /**
     * Function for containing the error generation to an understandable place.
     * 
     * @param numReps
     * @return The number of reps.
     */
    private static int parseNumReps(String numReps) {
        return Integer.parseInt(numReps);
    }

    /**
     * Function for containing the error generation to an understandable place.
     * 
     * @param printOut
     * @return True/False.
     */
    private static boolean parsePrintOut(String printOut) {
        return Boolean.parseBoolean(printOut);
    }
}
