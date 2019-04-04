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
package org.etexascode.webapp.ra;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.rmi.AccessException;
import java.rmi.RemoteException;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.output.ByteArrayOutputStream;
import org.etexascode.api.RunnerUtils;
import org.etexascode.interrep.datamodel.RemoteSimulatorInterface;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.SimulatorMessage;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.utils.UtilsArchiveOperations;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException.SimResourceErrorType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class that can build and zip up a simulation for download and implements SimulatorInterface.
 * 
 * @author bbaddillo
 * @author jrutherford
 * @author ablatt
 * @author janway
 */
public class EtexasRmiWrapper implements SimulatorInterface {

    /** The simulation id. Expects String. */
    public static final String KEY_SIM_ID = "SimId";

    /**
     * The {@link File} object that represents the project folder. Expects String.
     */
    public static final String KEY_PROJECT_FOLDER = "ProjDir";

    /** The path to the TEXAS sys_dat directory. Expects String. */
    public static final String KEY_SYSDAT_FOLDER = "SysDat";

    /** The execution directory location. Expects String. */
    public static final String KEY_EXEC_FOLDER = "ExecDir";

    /** The debugging port. Expects String. */
    public static final String KEY_DEBUG_PORT = "DebugPort";

    /** The path to a "java" binary to use to run eTEXAS. Expects String. */
    public static final String KEY_JAVA_COMMAND = "JavaCommand";

    /** The zip file key. Expects byte[]. */
    public static final String KEY_ARCHIVE = "Archive";

    /** The nReps key. Expects Integer. */
    public static final String KEY_NREP = "NRep";

    /** The random seed key. Expects Integer. */
    public static final String KEY_SEED = "Seed";

    /** The port number key. Expects Integer. */
    public static final String KEY_PORT = "Port";

    /** Logger for convenience. */
    private static final Logger LOGGER = LoggerFactory.getLogger(EtexasRmiWrapper.class);

    /** The controller for the eTEXAS simulation process. */
    private SimProcessRunner procRunner;

    /** The unique identifier of this simulation. */
    private String simId;

    /** Holds the directory of this simulation. */
    private File dir;

    /** The eTEXAS simulation remote object interface. */
    private RemoteSimulatorInterface simDriver;

    /** Size of the byte buffer to use for stream transfers. */
    private static final int BUFFER_SIZE = 4096;

    /** Holds the location of the project directory */
    private String projDir;

    /**
     * Constructor. Intentionally left empty.
     */
    public EtexasRmiWrapper() {}

    /**
     * Initializes a simulator interface from a configuration file.
     * 
     * @param conf The configuration file to setup simulator from.
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public void init(Map<String, Object> conf) throws RemoteException {

        simId = (String)conf.get(KEY_SIM_ID);

        projDir = (String)conf.get(KEY_PROJECT_FOLDER);
        File tmpF1 = new File(projDir);

        dir = new File(tmpF1, simId);
        if (!dir.mkdirs()) {

            if (!dir.exists()) {

                LOGGER.error("Couldn't make directory.");
                throw new RuntimeException("Couldn't make directory.");
            }
        }

        byte[] arch = null;
        try {

            arch = build((byte[])conf.get(KEY_ARCHIVE), (String)conf.get(KEY_SYSDAT_FOLDER), (String)conf.get(KEY_EXEC_FOLDER));
        }
        catch (SimResourceException e) {

            LOGGER.error("Exception e", e);
            throw new RuntimeException("An exception occured while building the archives with the following message: " + e.getMessage());
        }

        dir = new File(tmpF1, simId);
        if (!dir.mkdirs()) {

            if (!dir.exists()) {

                LOGGER.error("Couldn't make directory.");
                throw new RuntimeException("Couldn't make directory.");
            }
        }
        try {

            UtilsArchiveOperations.setupArchive(dir, arch);
            String projName = RunnerUtils.getProjectNameFromParFile(dir);

            RunnerUtils.runTexasExecutable(dir,
                    RunnerUtils.setUpdvPro((String)conf.get(KEY_SYSDAT_FOLDER), projName, (String)conf.get(KEY_EXEC_FOLDER), ((Integer)conf.get(KEY_NREP)), ((Integer)conf.get(KEY_SEED))));
        }
        catch (IOException ex) {

            LOGGER.error("Archive Setup Failure", ex);
        }
        procRunner = new SimProcessRunner(this.simId, ((Integer)conf.get(KEY_PORT)), (String)conf.get(KEY_EXEC_FOLDER), dir, (String)conf.get(KEY_SYSDAT_FOLDER), (String)conf.get(KEY_JAVA_COMMAND),
                (String)conf.get(KEY_DEBUG_PORT));
        try {

            procRunner.start();
        }
        catch (IOException ex) {

            LOGGER.debug("Exception", ex);
        }
        try {

            simDriver = procRunner.connectToSimDriver();
        }
        catch (AccessException ex) {

            LOGGER.debug("Exception", ex);
            throw new RuntimeException(ex);
        }
        catch (SimResourceException ex) {

            LOGGER.debug("Exception", ex);
            if (ex.getErrorType() == SimResourceErrorType.REMOTE) {

                throw new RuntimeException(ex);
            }
        }
    }

    /**
     * Build a zipped archive of eTEXAS simulation files (including design files).
     * 
     * @param archive The zip file.
     * @param sysDatDir The path to the TEXAS sys_dat directory.
     * @param execDir The execution directory for the simulation files.
     * @return A byte array of a zipped archive of the built project.
     * @throws RemoteException If a remote exception occurs.
     * @throws SimResourceException If the file is invalid.
     */
    byte[] build(byte[] archive, String sysDatDir, String execDir) throws SimResourceException {

        // Create an input stream from the byte array and unzip the contents to
        // disk.
        ByteArrayInputStream bis = new ByteArrayInputStream(archive);
        BufferedInputStream bufferedInputStream = new BufferedInputStream(bis, BUFFER_SIZE);
        ZipInputStream zis = new ZipInputStream(bufferedInputStream);
        String projectName = null;
        ZipEntry entry;
        try {

            // Iterate through each entry in the zip archive and write them out
            // to file.
            while ((entry = zis.getNextEntry()) != null) {

                LOGGER.info("[eTEXAS RA {}] Extracting: {}", new String[] { simId, entry.toString() });

                int count;
                byte buffer[] = new byte[BUFFER_SIZE];
                String filename = entry.getName();

                // Check the name of the file and extract the project name
                // if it a design file.
                int index = filename.lastIndexOf("_gdvdata");
                if (index < 0) {

                    index = filename.lastIndexOf("_simdata");
                }
                if (index >= 0) {

                    filename = "a" + filename.substring(index);
                    projectName = "a";
                }

                File newFile = new File(dir, filename);
                if (newFile.getAbsolutePath().length() > 60) {

                    throw new SimResourceException("The path to the file was longer than the maximum of 60 characters", SimResourceErrorType.FILE_FORMAT);
                }

                // Output the file to disk.
                FileOutputStream fos;
                try {

                    fos = new FileOutputStream(newFile);
                }
                catch (FileNotFoundException ex) {

                    LOGGER.debug(null, ex);
                    throw new SimResourceException(ex, SimResourceErrorType.FILE_NOT_FOUND);
                }

                BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(fos, BUFFER_SIZE);
                try {

                    while ((count = zis.read(buffer, 0, BUFFER_SIZE)) != -1) {

                        bufferedOutputStream.write(buffer, 0, count);
                    }
                    bufferedOutputStream.flush();
                    bufferedOutputStream.close();
                }
                catch (IOException ex) {

                    LOGGER.debug(null, ex);
                    throw new SimResourceException(ex, SimResourceErrorType.IO);
                }
                finally {

                    try {

                        if (bufferedOutputStream != null) {

                            bufferedOutputStream.close();
                        }
                    }
                    catch (IOException ex) {

                        LOGGER.debug(null, ex);
                    }
                }
            }
        }
        catch (IOException ex) {

            LOGGER.debug(null, ex);
            throw new SimResourceException(ex, SimResourceErrorType.IO);
        }
        finally {

            try {

                if (zis != null) {

                    zis.close();
                }
            }
            catch (IOException ex) {

                LOGGER.debug(null, ex);
            }
        }

        // Build the design files.
        try {

            buildProcessRunner(sysDatDir, dir, projectName, execDir);
        }
        catch (IOException ex) {

            LOGGER.debug(null, ex);
            throw new SimResourceException(ex, SimResourceErrorType.IO);
        }

        // Zip up the built project files to a byte array.
        // Important! Using Apache Commons IO ByteArrayOutputStream because
        // of the way that it grows the backing buffer without copying and
        // garbage collecting. This is necessary because the resulting size of
        // the zip is unknown at this time.
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(BUFFER_SIZE);
        ZipOutputStream zos = new ZipOutputStream(byteArrayOutputStream);
        zip(dir, dir.getAbsolutePath().length(), zos);
        try {

            zos.close();
        }
        catch (IOException ex) {

            LOGGER.debug(null, ex);
            throw new SimResourceException(ex, SimResourceErrorType.IO);
        }
        finally {

            if (zos != null) {

                try {

                    zos.close();
                }
                catch (IOException ex) {

                    LOGGER.debug(null, ex);
                }
            }

        }

        byte[] resultArray = byteArrayOutputStream.toByteArray();
        return resultArray;
    }

    /**
     * Run the processes to build the project files.
     * 
     * @param sysDatDir The path to the TEXAS sys_dat directory.
     * @param projDir The {@link File} object that represents the project folder.
     * @param projName The name of the project (used in project files).
     * @param libPath The path to the native SIMPRO shared library.
     */
    private void buildProcessRunner(String sysDatDir, File projDir, String projName, String libPath) throws IOException {

        RunnerUtils.writeSimpro(projDir, projName, sysDatDir);

        RunnerUtils.runTexasExecutable(projDir, RunnerUtils.setUpGDVConv(sysDatDir, projName, libPath));
        RunnerUtils.runTexasExecutable(projDir, RunnerUtils.setUpGeoPro(sysDatDir, projName, libPath));
        RunnerUtils.runTexasExecutable(projDir, RunnerUtils.setUpdvPro(sysDatDir, projName, libPath));
        RunnerUtils.runTexasExecutable(projDir, RunnerUtils.setUpSimConv(sysDatDir, projName, libPath));
    }

    /**
     * Zip up the contents of a directory to a zip stream.
     * 
     * @param directory The current directory to add to the recursive.
     * @param basePathLen The length of base path string.
     * @param zos The destination zip stream.
     * @throws SimResourceException If the file cannot be zipped.
     */
    private void zip(File directory, int basePathLen, ZipOutputStream zos) throws SimResourceException {

        File[] files = directory.listFiles();
        byte[] buffer = new byte[BUFFER_SIZE];
        int read = 0;

        if (files == null) {

            throw new SimResourceException("There were no files to zip up", SimResourceErrorType.IO);
        }
        for (File file : files) {

            if (file.isDirectory()) {

                zip(file, basePathLen, zos);
            }
            else {

                FileInputStream in = null;
                try {

                    in = new FileInputStream(file);
                    ZipEntry entry = new ZipEntry(file.getAbsolutePath().substring(basePathLen + 1));
                    zos.putNextEntry(entry);
                    while (-1 != (read = in.read(buffer))) {

                        zos.write(buffer, 0, read);
                    }
                    in.close();
                }
                catch (IOException ex) {

                    LOGGER.debug(null, ex);
                    throw new SimResourceException(ex, SimResourceErrorType.IO);
                }
                finally {

                    try {

                        if (in != null) {

                            in.close();
                        }
                    }
                    catch (IOException ex) {

                        LOGGER.debug(null, ex);
                    }
                }
            }
        }
    }

    /**
     * Gets the static data for the simulator interface.
     * 
     * @return The static data from the simulator interface.
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public StaticData getStaticData() throws RemoteException {

        try {

            return simDriver.getStaticData();
        }
        catch (Exception e) {

            close();
            throw new RemoteException("Error getting staticData.", e);
        }
    }

    /**
     * Gets the step data from the simulator interface.
     * 
     * @param stepNum The step number for the simulation.
     * @return The step data for that specific step number in the simulation.
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public StepData getStepData(long stepNum) throws RemoteException {

        try {

            return simDriver.getStepData(stepNum);
        }
        catch (Exception e) {

            close();
            throw new RemoteException("Error getting stepData.", e);
        }
    }

    /**
     * Shuts down the simulator interface.
     * 
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public void close() throws RemoteException {

        simDriver.close();
        procRunner.stop();
        File file = new File(projDir, simId);
        try {

            FileUtils.deleteDirectory(file);
        }
        catch (IOException e) {

            LOGGER.info("", e);
        }
    }

    /**
     * Adds a vehicle command to the simulator interface.
     * 
     * @param command The vehicle command to add to the simulator interface.
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public void addVehicleCommand(VehicleCommand command) throws RemoteException {

        simDriver.addVehicleCommand(command);
    }

    /**
     * Adds a signal command to the simulator interface.
     * 
     * @param command The signal command to add to the simulator interface.
     */
    @Override
    public void addSignalCommand(SignalCommand command) throws RemoteException {

        simDriver.addSignalCommand(command);
    }

    /**
     * Adds a vehicle injection request to the simulator interface.
     * 
     * @param request The vehicle injection request to add to the simulator interface.
     */
    @Override
    public void addVehicleInjectionRequest(VehicleInjectionRequest request) throws RemoteException {

        simDriver.addVehicleInjectionRequest(request);
    }

    /**
     * Checks the simulator interface for error messages.
     * 
     * @param execId The execution ID to check for errors.
     * @return The error messages for the execution ID.
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public List<SimulatorMessage> checkForErrorOutput(String execId) throws RemoteException {

        return simDriver.checkForErrorOutput(dir.getAbsolutePath());
    }

    @Override
    public List<SimulatorMessage> checkForWarningOutput(String execId) throws RemoteException {

        return simDriver.checkForWarningOutput(dir.getAbsolutePath());
    }
}