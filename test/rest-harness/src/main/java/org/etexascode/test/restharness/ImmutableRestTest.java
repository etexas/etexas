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

/**
 * A program which is configured before compilation.
 * 
 * @author ablatt
 */
public class ImmutableRestTest {

    /*
     * Note: when there is an array, each element of the array is used for the execution of a
     * specific project
     */
    /**
     * The number of times to repeat a project.
     */
    public static final int[] numReps = new int[] { 1 };

    /**
     * The number of steps to perform at the same time in the project.
     */
    public static final int[] numStepsAtOnce = new int[] { 10 };

    /**
     * The name of the project on the server.
     */
    public static final String[] projNames = new String[] { "playback" };

    /**
     * The location of the server.
     */
    public static final String hostLocation = "localhost";

    /**
     * The user name to use.
     */
    public static final String userName = "Unauthenticated";

    /**
     * Should we print stuff out as things are running?
     */
    public static final boolean printOut = true;

    /**
     * The place to write all the files for all the app logs.
     */
    public static final File rootDir = new File("/home/ablatt/rest-res");

    /**
     * The keys to qurey the server for.
     */
    public static final String[] moeKeysToDownload = new String[] { "Queue Length MOE", "Total Phase Failures", "Queue Delay - Total"// ,
            // "DEBUG Veh Lanes",
            // "DEBUG Veh Ids",
            // "DEBUG - Vehicle"
    };

    /**
     * Main method for executing all the projects.
     * 
     * @param args
     */
    public static void main(String[] args) {
        if (projNames.length != numStepsAtOnce.length) {
            System.out.println("numReps.length != numStepsAtOnce.length" + " != projNames.length ... please fix ... exiting");
            return;
        }

        if (numStepsAtOnce.length != numReps.length) {
            System.out.println("numReps.length != numStepsAtOnce.length" + " != projNames.length ... please fix ... exiting");
            return;
        }

        for (int i = 0; i < projNames.length; i++) {
            /*
             * RESTHarness.executeProject(hostLocation, projNames[i], userName, moeKeysToDownload,
             * rootDir, numStepsAtOnce[i], numReps[i], printOut);
             */
        }

        /*
         * try { RESTSimulationTest.testAllFunctions(hostLocation, userName); } catch
         * (URISyntaxException e) { e.printStackTrace(); } catch (JAXBException e) {
         * e.printStackTrace(); } RESTSimulationTest.testVehicleInjection(hostLocation, userName);
         */
    }
}
