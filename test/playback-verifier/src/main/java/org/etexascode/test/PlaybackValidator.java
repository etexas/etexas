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
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.etexascode.datalayer.inmemory.DefaultVehicleIdComponent;
import org.etexascode.datalayer.inmemory.SingleIntersectionDataLayer;
import org.etexascode.datalayer.interfaces.IInterRepCoordinationDataLayer;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.webapp.playback.PlaybackSimulator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Compares the GDVSIM signal times to the playback signal times.
 * 
 * @author janway
 * @author cdeisher
 * @author bmauldon
 */
public class PlaybackValidator {

    /**
     * The total number of signal comparisons made.
     */
    private static int signalsCompared = 0;

    /**
     * The number of signal comparisons that failed.
     */
    private static int signalsFailed = 0;

    /**
     * The total number of vehicle comparisons made.
     */
    private static int vehiclesCompared = 0;

    /**
     * The number of vehicle comparisons that failed.
     */
    private static int vehiclesFailed = 0;

    /**
     * The directory File that has the playback data (output from the python parser).
     */
    private static File file;

    /**
     * The interRep representation of the playback data.
     */
    private static InterRep interRep;

    /**
     * The list of signals parsed by the validator parser.
     */
    private static List<NGSimSignals> simSignals;

    /**
     * The list of vehicles parsed by the validator parser.
     */
    private static List<NGSimVehicles> simVehicles;

    /**
     * Put the peachtree signal timing data in the user home.
     */
    private final String signalTimingDirectory = System.getProperty("user.home") + "\\NGSIMData\\peachtree-main-data\\signal-timing\\signal-change";

    /** The playback simulator */
    private static PlaybackSimulator playSim;

    // /**
    // * The signal timing files to read for the 4 o'clock data set.
    // */
    // private List<String> filenames = Arrays.asList(
    // "Peachtree_12th_EB_0400-0415.txt",
    // "Peachtree_12th_NB_0400-0415.txt",
    // "Peachtree_12th_SB_0400-0415.txt",
    // "Peachtree_12th_WB_0400-0415.txt"
    // );
    // /**
    // * The location of the vehicle trajectory data for 4 o'clock.
    // */
    // private String trajectoryFileLocation =
    // "C:\\Users\\janway\\Desktop\\Playback\\Peachtree-Main-Data\\vehicle-trajectory-data\\0400pm-0415pm\\trajectories-0400pm-0415pm.txt";
    // /**
    // * The location of the python parser output for the 4 o'clock data.
    // */
    // private String outputFolderLocation =
    // "C:\\Users\\janway\\Desktop\\Playback\\four_take_3";

    /**
     * The signal timing files to read for the 12 o'clock data set.
     */
    private final List<String> filenames = Arrays.asList("Peachtree_12th_EB_1245-0100.txt", "Peachtree_12th_NB_1245-0100.txt", "Peachtree_12th_SB_1245-0100.txt", "Peachtree_12th_WB_1245-0100.txt");

    /**
     * Put the vehicle trajectory data for 12 o'clock in the user directory.
     */
    private final String trajectoryFileLocation = System.getProperty("user.home") + "\\NGSIMData\\peachtree-main-data\\vehicle-trajectory-data\\1245pm-0100pm\\trajectories-1245pm-0100pm.txt";

    /**
     * The location of the python parser output for the 12 o'clock data.
     */
    private final String outputFolderLocation = System.getProperty("user.home") + "\\twelve_take_3";

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(PlaybackValidator.class);

    /**
     * Constructor that opens and parses all of the files, so all of the data structures to be
     * compared are populated.
     * 
     * @throws URISyntaxException If a URI syntax exception occurs.
     */
    public PlaybackValidator() throws URISyntaxException {
        simSignals = PeachtreeSignalTimingsParser.getPeachtreeSignalTimes(signalTimingDirectory, filenames);
        simVehicles = PeachtreeVehicleParser.getPeachTreeVehicles(trajectoryFileLocation);
        file = new File(outputFolderLocation);

        playSim = new PlaybackSimulator(file);
        ExecMetaData execData = new ExecMetaData();
        ReferencePoint[] refPoint = new ReferencePoint[1];
        refPoint[0] = new ReferencePoint();
        refPoint[0].setLatitude(0.0);
        refPoint[0].setLongitude(0.0);
        execData.setReferencePoints(refPoint);
        execData.setExecDetectors(new ArrayList<Detector>());
        List<IDeviceData> devs = new ArrayList<IDeviceData>(0);
        IInterRepCoordinationDataLayer dataLayer = new SingleIntersectionDataLayer(devs, null, 1, new DefaultVehicleIdComponent());

        interRep = new InterRep(0, playSim, execData, dataLayer);
    }

    /**
     * Compares the signals and vehicles produced by the validator parsers with those produced by
     * the python parser.
     * 
     * @param args The agurments.
     */
    public static void main(String[] args) {

        try {
            new PlaybackValidator();
        }
        catch (URISyntaxException e) {
            LOGGER.error("Could not construct Playback Validator.", e);
        }

        while (interRep.update() && (interRep.getCurrentTimeStep() < 10051)) {
            // Update goes until 10052, but we only have files for up to 10051

            // grab data from Playback Simulator
            List<SignalIndication> signals = playSim.getStepData(interRep.getCurrentTimeStep()).getSignalIndication();
            List<Vehicle> vehicles = playSim.getStepData(interRep.getCurrentTimeStep()).getVehicles();

            // compare signals
            for (SignalIndication ind : signals) {
                if (!isInNGSIM(ind, simSignals, interRep.getCurrentTimeStep())) {
                    signalsFailed++;
                }
                signalsCompared++;
            }

            // compare vehicles
            for (Vehicle vehicle : vehicles) {
                boolean found = false;
                for (NGSimVehicles ngv : simVehicles) {
                    if (ngv.matches(vehicle)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    vehiclesFailed++;
                    LOGGER.info("Failed to match vehicle at time {}:\n {}", interRep.getCurrentTimeStep(), vehicle.toString());
                    if (vehiclesFailed > 100)
                        return;
                }
                vehiclesCompared++;
            }
        }

        LOGGER.info("Compared {} and failed {} signals.", signalsCompared, signalsFailed);
        LOGGER.info("Expected signal comparisons: {}", 9 * 9635);
        LOGGER.info("Compared {}, failed {}, passed {} vehicles.", vehiclesCompared, vehiclesFailed, vehiclesCompared - vehiclesFailed);

    }

    /**
     * Finds a corresponding validator-produced signal for the corresponding python-produced signal.
     * 
     * @param ind The signal to be matched.
     * @param signals The list of signals to search.
     * @param timestep The timestep of the signal.
     * @return True if found, false otherwise.
     */
    private static boolean isInNGSIM(SignalIndication ind, List<NGSimSignals> signals, int timestep) {
        for (NGSimSignals sig : signals) {
            if (sig.getPrevTimeStep() <= timestep && timestep <= sig.getTimestep()) {
                if (sig.getLaneIds().contains(ind.getLaneId())) {
                    if (ind.getTypeIndication() == SignalIndication.Type.BALL) {
                        if (ind.getColorIndication() == SignalIndication.Color.GREEN) {
                            if (sig.getState().equals(NGSimSignals.State.G_THRU)) {
                                return true;
                            }
                            else {
                                LOGGER.info("Failed with Green-Ball expecting {}", sig.getState().toString());
                            }
                        }
                        if (ind.getColorIndication() == SignalIndication.Color.YELLOW) {
                            if (sig.getState().equals(NGSimSignals.State.Y_THRU)) {
                                return true;
                            }
                            else {
                                LOGGER.info("Failed with Yellow-Ball expecting {}", sig.getState().toString());
                            }
                        }
                        if (ind.getColorIndication() == SignalIndication.Color.RED) {
                            if (sig.getState().equals(NGSimSignals.State.R_THRU)) {
                                return true;
                            }
                            else {
                                LOGGER.info("Failed with Red-Ball expecting {}", sig.getState().toString());
                            }
                        }
                    }
                }
            }
        }

        LOGGER.info("Failing at time: {} with indication: {}", timestep, ind.toString());
        return false;
    }
}
