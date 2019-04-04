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
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * A simple class for performing common operations against our REST API.
 * 
 * @author ablatt
 * @author bbadillo
 */
public class RESTHarness {

    /**
     * (Non-functional) Execute a project from start to finish.
     * 
     * @param hostLocation The location of the server.
     * @param projName The name of the project on the server.
     * @param userName User Name.
     * @param keysToDownload A list of keys to download from the server.
     * @param rootDir The root directory to download files into.
     * @param stepsAtOnce The number of steps to execute per rest call.
     * @param numReps The number of simulations to execute.
     * @param printOut Should we print updates to the command line?
     */
    public static void executeProject(String hostLocation, String projName,
            String userName, String[] keysToDownload, File rootDir,
            int stepsAtOnce, int numReps, boolean printOut) {
        String[] simIds = getSimIds(hostLocation, projName, userName);
        Random r = new Random();

        for (String sid : simIds) {
            for (int i = 0; i < numReps; i++) {
                int randomSeed = r.nextInt();
                String execId = executeSim(hostLocation, sid, randomSeed, stepsAtOnce, printOut);
                writeOutTables(hostLocation, execId, randomSeed, keysToDownload, rootDir);

                if (printOut) {
                    System.out.println("finished write out for execution " + (i + 1));
                }
            }
        }

        if (printOut) {
            System.out.println("finishing the project");
        }
    }

    /**
     * Get the Sim Ids associated with a project.
     * 
     * @param hostLocation The location of the server.
     * @param pid Project Id
     * @param user User Name
     * @return Sim Ids
     */
    public static String[] getSimIds(String hostLocation, String pid, String user) {
        Map<String, String> params = new HashMap<String, String>();
        params.put("uname", user);
        String s = RESTUtils.executeResource(hostLocation, "ExecService", "getSimByProj", pid, params, String.class);
        return s.split("\n");
    }

    /**
     * (Non-Functional) Executes a simulation.
     * 
     * @param hostLocation The location of the server.
     * @param id Id of the simulation to execute.
     * @param randomSeed The seed to use for this simulation.
     * @param numStepsAtOnce The number of steps to execute per rest call.
     * @param printToCommandLine Should we print updates to the command line?
     * @return The id of the execution.
     */
    public static String executeSim(String hostLocation, String id, int randomSeed, int numStepsAtOnce, boolean printToCommandLine) {
        String execId = startSim(hostLocation, id, randomSeed);
        if (!execId.isEmpty()) {

            int i = 0;
            double predTime = 0.0;
            double prevTime = Double.NEGATIVE_INFINITY;

            if (printToCommandLine) {
                System.out.println("starting exec: " + execId + "  with seed: " + randomSeed);
            }

            while (!closeEnough(predTime, prevTime)) {
                prevTime = predTime;
                predTime = execAdvance(hostLocation, execId, numStepsAtOnce);
                if (printToCommandLine) {
                    System.out.println("executed step " + i + " which returned time " + predTime);
                }
                i++;
            }

            if (printToCommandLine) {
                System.out.println("finishing exec: " + execId);
            }
        }

        if (printToCommandLine) {
            System.out.println("finishing simulation");
        }

        return execId;
    }

    /**
     * (Monadic Structure) Start a simulation.
     * 
     * @param hostLocation The location of the server.
     * @param id The sim id.
     * @param randomSeed The random seed to use in the simulation.
     * @return The id of the execution started.
     */
    public static String startSim(String hostLocation, String id, int randomSeed) {
        Map<String, String> params = new HashMap<String, String>();
        params.put("r", Integer.toString(randomSeed));
        return RESTUtils.executeResource(hostLocation, "ExecService", "startSim", id, params, String.class);
    }

    /**
     * (Non-Functional) Advances the execution.
     * 
     * @param hostLocation The location of the server.
     * @param id The execution id.
     * @param numSteps The number of steps to execute at once.
     * @return The current execution time according to the server.
     */
    public static double execAdvance(String hostLocation, String id, int numSteps) {
        Map<String, String> params = new HashMap<String, String>();
        params.put("n", numSteps + "");
        String tmp = RESTUtils.executeResource(hostLocation, "ExecService", "advance", id, params, String.class);
        return Double.parseDouble(tmp);
    }

    /**
     * Get the current execution time according to the server.
     * 
     * @param hostLocation The location of the server.
     * @param id The execution id.
     * @return The current execution time according to the server.
     */
    public static double getCurrTime(String hostLocation, String id) {
        String tmp = RESTUtils.executeResource(hostLocation, "ExecService", "getCurrentTime", id, String.class);
        return Double.parseDouble(tmp);
    }

    /**
     * Get the table in csv form of the app logs for a specific execution. concerning a specific
     * key.
     * 
     * @param hostLocation The location of the server.
     * @param execId The id of the execution.
     * @param key The key in the app logs.
     * @return The csv file in string form.
     */
    public static String getCsv(String hostLocation, String execId, String key) {
        Map<String, String> keys = new HashMap<String, String>();
        keys.put("appKey", key);
        return RESTUtils.executeResource(hostLocation, "ExecService", "getAppLogTable", execId, keys, String.class);
    }

    /**
     * (Non-Functional) Write out the csv files for the keys associated with the execution.
     * 
     * @param hostLocation The location of the server.
     * @param execId The id of the execution.
     * @param randomSeed The random seed associated with this execution.
     * @param keysToDownload A list of keys to download from the server.
     * @param rootDir The root directory to download files into.
     */
    public static void writeOutTables(String hostLocation, String execId, int randomSeed, String[] keysToDownload, File rootDir) {
        for (String s : keysToDownload) {
            writeTable(hostLocation, execId, randomSeed, s, rootDir);
        }
    }

    /**
     * (Non-Function) Write out the csv files for the key associated with the execution.
     * 
     * @param hostLocation The location of the server.
     * @param simId The id of the execution.
     * @param randomSeed The random seed associated with this execution.
     * @param key A key to download from the server.
     * @param root The root directory to download files into.
     */
    public static void writeTable(String hostLocation, String simId, int randomSeed, String key, File root) {
        File f = new File(root, simId + "-" + randomSeed + "-" + key + ".csv");
        try {
            PrintWriter pw = new PrintWriter(f);
            pw.print(getCsv(hostLocation, simId, key));
            pw.close();
        }
        catch (FileNotFoundException fnf) {
            fnf.printStackTrace();
        }

        System.gc(); // just performed some huge mem alloc
    }

    /**
     * Is d1 close enough to d2 to be considered equal?
     * 
     * @param d1 The double 1.
     * @param d2 The double 2.
     * @return Rather the values are equal.
     */
    private static boolean closeEnough(double d1, double d2) {
        return ((d1 - d2) < 0.05) && ((d2 - d1) < 0.05);
    }
}
