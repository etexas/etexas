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
import java.io.FileFilter;
import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.SimulatorMessage;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.utils.UtilsArchiveOperations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implementation of the SimulatorInterface for use with playback simulator types.
 * 
 * @author janway
 * @author bbaddillo
 * @author ablatt
 * @author dgolman
 * @author jrutherford
 * @author bmauldon
 */
public class PlaybackSimulator implements SimulatorInterface {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(PlaybackSimulator.class);

    public static final String FILE_LOCATION_KEY = "proj-dir";

    public static final String ARCHIVE_KEY = "proj-files";

    protected File staticData;

    protected File stepData;

    public PlaybackSimulator(File static_data, File step_data) {
        staticData = static_data;
        stepData = step_data;
    }

    public PlaybackSimulator(File rootDataFolder) {
        DataFileFilter fileFilter = new DataFileFilter();
        File[] fileList = rootDataFolder.listFiles(fileFilter);
        boolean filesFound = false;

        if (fileList != null) {

            Arrays.sort(fileList);

            if (fileList.length == 2 && fileList[0].exists() && fileList[1].exists() && fileList[0].getName().equals("static_data") && fileList[1].getName().equals("step_data")) {
                staticData = fileList[0];
                stepData = fileList[1];
                filesFound = true;
            }
        }

        if (!filesFound) {
            throw new RuntimeException("The playback data folders were not found");
        }
    }

    /**
     * Will be used in test cases if nowhere else...
     * 
     * @param fileLocation The location to find the test files...
     */
    public PlaybackSimulator(String fileLocation) {
        this(new File(fileLocation));
    }

    /**
     * Do Not Delete - needed for the RA
     */
    public PlaybackSimulator() {} // this is the constructor that must be used by the simulation

    @Override
    public void init(Map<String, Object> conf) throws RemoteException {
        String fileLocation = (String)conf.get(FILE_LOCATION_KEY);
        byte[] archive = (byte[])conf.get(ARCHIVE_KEY);

        if (fileLocation == null) {
            throw new RuntimeException("No File Location was found for the playback simulator");
        }

        if (archive == null) {
            throw new RuntimeException("No archive was found for the playback simulator");
        }

        File rootDataFolder = new File(fileLocation);

        try {
            UtilsArchiveOperations.setupArchive(rootDataFolder, archive);
        }
        catch (IOException e) {

            LOGGER.error("Archive Setup Failure", e);
            throw new RuntimeException("The playback data folders were not setup correctly");
        }

        DataFileFilter fileFilter = new DataFileFilter();
        File[] fileList = rootDataFolder.listFiles(fileFilter);
        boolean filesFound = false;

        if (fileList != null) {

            Arrays.sort(fileList);

            if (fileList.length == 2 && fileList[0].exists() && fileList[1].exists() && fileList[0].getName().equals("static_data") && fileList[1].getName().equals("step_data")) {
                staticData = fileList[0];
                stepData = fileList[1];
                filesFound = true;
            }
        }

        if (!filesFound) {
            throw new RuntimeException("The playback data folders were not found");
        }
    }

    @Override
    public void addSignalCommand(SignalCommand arg0) {
        // Signal Data cannot be updated for the Playback Simulator
    }

    @Override
    public void addVehicleCommand(VehicleCommand arg0) {
        // Vehicle Data cannot be updated for the Playback Simulator
    }

    @Override
    public void addVehicleInjectionRequest(VehicleInjectionRequest request) {
        // Vehicles cannot be injected in the Playback Simulator
    }

    @Override
    public void close() {}

    @Override
    public StaticData getStaticData() {
        return StaticDataParser.parseStaticData(staticData, stepData);
    }

    @Override
    public StepData getStepData(long stepNum) {
        return StepDataParser.getStepData(stepNum, stepData);
    }

    static private class DataFileFilter implements FileFilter {

        @Override
        public boolean accept(File pathname) {
            if (pathname.getName().endsWith("data")) {
                return true;
            }
            else {
                return false;
            }
        }
    }

    @Override
    public List<SimulatorMessage> checkForErrorOutput(String execId) throws RemoteException {
        return new ArrayList<SimulatorMessage>();
    }

    @Override
    public List<SimulatorMessage> checkForWarningOutput(String simDirectory) throws RemoteException {
        return new ArrayList<SimulatorMessage>();
    }
}