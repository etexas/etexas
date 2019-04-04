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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.etexascode.api.eTEXAS;
import org.etexascode.interrep.datamodel.RemoteSimulatorInterface;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorMessage;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class which interacts with the eTEXAS API and sets up the port and UUID.
 * 
 * @author ablatt
 * @author ttevendale
 */
public class EtexasDriver implements RemoteSimulatorInterface {

    /** Logger for convenience. */
    private static final Logger LOGGER = LoggerFactory.getLogger(EtexasDriver.class);

    /** Reference to the eTEXAS API. */
    private eTEXAS etexas;

    /** The port number. */
    private int portNum;

    /** The UUID. */
    private String uuid;

    /** The buffered reader for error output */
    private BufferedReader errorReader;

    /** The buffered reader for warning output */
    private BufferedReader warningReader;

    /**
     * The main class which starts a simulator.
     * 
     * @param args The command line arguments.
     * @throws RemoteException If a remote exception occurs.
     * @throws ClassNotFoundException If a class not found exception occurs.
     */
    public static void main(String[] args) throws RemoteException, ClassNotFoundException {

        try {

            LOGGER.trace("Entered {} main method.", EtexasDriver.class.getName());

            EtexasDriver simDriver = new EtexasDriver();
            simDriver.setUUID(args[0]);
            simDriver.setPort(Integer.parseInt(args[1]));

            final Registry registry = LocateRegistry.getRegistry(simDriver.getPort());
            LOGGER.debug("Binding to RMI using uuid={}", simDriver.getUUID());
            RemoteSimulatorInterface stub = (RemoteSimulatorInterface)UnicastRemoteObject.exportObject(simDriver, 0);
            registry.rebind(simDriver.getUUID(), stub);

            simDriver.init();

            LOGGER.trace("Exiting {} main method.", EtexasDriver.class.getName());
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
            throw e;
        }
    }

    /**
     * Gets the port number.
     * 
     * @return The port number.
     */
    public int getPort() {

        return portNum;
    }

    /**
     * Sets the port number.
     * 
     * @param port The new port number.
     */
    public void setPort(int port) {

        this.portNum = port;
    }

    /**
     * Gets the UUID.
     * 
     * @return The UUID.
     */
    public String getUUID() {

        return uuid;
    }

    /**
     * Sets the UUID.
     * 
     * @param uuid The new UUID.
     */
    public void setUUID(String uuid) {

        this.uuid = uuid;
    }

    /**
     * A separate initialization method outside of the constructor to allow the class to be remotely
     * bound even if there is an error. This also allows the class to be bound faster and delays
     * initialization.
     * 
     * @throws ClassNotFoundException Thrown if InterCom implementations are not found.
     */
    synchronized public void init() throws ClassNotFoundException {

        try {

            // Initialize the simulation
            LOGGER.trace("Entered {} init method.", EtexasDriver.class.getName());
            etexas = new eTEXAS();
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
            throw e;
        }
    }

    /**
     * Retrieves the static data from the simulator.
     * 
     * @return The static data from the simulator.
     */
    @Override
    synchronized public StaticData getStaticData() {

        try {

            LOGGER.trace("Entered {} getStaticData method.", EtexasDriver.class.getName());
            return etexas.getStaticData();
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
            throw e;
        }
    }

    /**
     * Gets the step data from the simulation.
     * 
     * @param stepNum The current step number from the simulation.
     */
    @Override
    synchronized public StepData getStepData(long stepNum) {

        try {

            LOGGER.trace("Entered {} getStepData method.", EtexasDriver.class.getName());
            return etexas.getStepData(stepNum);
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
            throw e;
        }
    }

    /**
     * Closes and unbinds the simulation.
     */
    @Override
    synchronized public void close() {

        Registry registry;
        try {

            registry = LocateRegistry.getRegistry(portNum);
            registry.unbind(uuid);
            UnicastRemoteObject.unexportObject(this, true);

            if (errorReader != null) {

                errorReader.close();
            }
            if (warningReader != null) {

                warningReader.close();
            }
        }
        catch (RemoteException e) {

            LOGGER.error("Error connecting to remote resource.", e);
        }
        catch (NotBoundException e) {

            LOGGER.error("Could not unbind the eTEXAS driver.", e);
        }
        catch (IOException e) {

            LOGGER.error("Could not close file readers");
        }
    }

    /**
     * Adds a vehicle command to the simulation.
     * 
     * @param command The vehicle command to add to the simulation.
     */
    @Override
    synchronized public void addVehicleCommand(VehicleCommand command) {

        try {

            LOGGER.trace("Entered {} addVehicleCommand method.", EtexasDriver.class.getName());
            etexas.addVehicleCommand(command);
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
            throw e;
        }
    }

    /**
     * Adds a signal command to the simulation.
     * 
     * @param command The signal command to add to the simulation.
     */
    @Override
    synchronized public void addSignalCommand(SignalCommand command) {

        try {

            LOGGER.trace("Entered {} addSignalCommand method.", EtexasDriver.class.getName());
            etexas.addSignalCommand(command);
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
            throw e;
        }
    }

    /**
     * Adds a vehicle injection request to the simulation.
     * 
     * @param request The vehicle injection request to add to the simulation.
     */
    @Override
    synchronized public void addVehicleInjectionRequest(VehicleInjectionRequest request) {

        try {

            LOGGER.trace("Entered {} addVehicleInjectionRequest method.", EtexasDriver.class.getName());
            etexas.addVehicleInjectionRequest(request);
        }
        catch (RuntimeException e) {

            LOGGER.error("Uncaught runtime exception while stepping.", e);
        }
    }

    /**
     * Initialize the builder interface
     * 
     * @param conf The map configuration for the builder
     * @throws RemoteException If a remote exception occurs.
     */
    @Override
    public void init(Map<String, Object> conf) throws RemoteException {} // do nothing

    /**
     * Checks to see if TEXAS had any errors generated during the simulation.
     * 
     * @param simDirectory The directory error files would appear in.
     * @return The list of error simulator messages.
     */
    @Override
    synchronized public List<SimulatorMessage> checkForErrorOutput(String simDirectory) throws RemoteException {

        if (errorReader == null) {

            File errorFile = new File(simDirectory + "/error.txt");
            errorReader = setupFileReader(errorFile);
        }
        return getOutput(errorReader);
    }

    /**
     * Checks to see if TEXAS had any warnings generated during the simulation.
     * 
     * @param simDirectory The directory warning files would appear in.
     * @return The list of warning simulator messages.
     */
    @Override
    synchronized public List<SimulatorMessage> checkForWarningOutput(String simDirectory) throws RemoteException {

        if (warningReader == null) {

            File warningFile = new File(simDirectory + "/warning.txt");
            warningReader = setupFileReader(warningFile);
        }
        return getOutput(warningReader);
    }

    /**
     * Setups the file reader if the file exists.
     * 
     * @param file The file to setup the file reader with.
     * @return The file reader.
     */
    private BufferedReader setupFileReader(File file) {

        BufferedReader br = null;

        if (file.exists()) {

            try {

                br = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
            }
            catch (FileNotFoundException e) {

                LOGGER.error("Error getting file", e);
            }
            catch (UnsupportedEncodingException e) {

                LOGGER.error("Error getting file due to encoding", e);
            }
        }

        return br;
    }

    /**
     * Gets a list of simulator messages contained in the file.
     * 
     * @param br The buffered reader to get the simulator messages from.
     * @return The list of simulator messages.
     */
    private List<SimulatorMessage> getOutput(BufferedReader br) {

        List<SimulatorMessage> simMessages = new ArrayList<SimulatorMessage>();

        if (br == null) {

            return simMessages;
        }

        try {

            String line = br.readLine();
            while (line != null) {

                SimulatorMessage simMessage = new SimulatorMessage(line);
                simMessages.add(simMessage);
                line = br.readLine();
            }
        }
        catch (IOException e) {

            LOGGER.error("Error reading output", e);
        }

        return simMessages;
    }
}
