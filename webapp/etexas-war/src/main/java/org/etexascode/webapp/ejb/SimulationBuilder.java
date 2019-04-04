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
package org.etexascode.webapp.ejb;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.playback.PlaybackSimulator;
import org.etexascode.webapp.ra.EtexasRmiWrapper;
import org.etexascode.webapp.ra.api.SimFactory;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException;
import org.slf4j.LoggerFactory;

/**
 * Provides utility methods to build a simulation.
 * 
 * @author ablatt
 * @author bmauldon
 * @author emyers
 */
final class SimulationBuilder {

    /** The JNDI lookup for the playback directory. */
    private static final String JNDI_DIRECTORY_PLAYBACK = "resource/playbackDir";

    /** The JNDI lookup for the texas project directory. */
    private static final String JNDI_DIRECTORY_TEXAS_PROJECT = "resource/texasProjDir";

    /** The JNDI lookup for the texas system directory. */
    private static final String JNDI_DIRECTORY_TEXAS_SYS = "resource/texasSysDataDir";

    /** The JNDI lookup for the texas exe directory. */
    private static final String JNDI_DIRECTORY_TEXAS_EXE = "resource/texasExeDir";

    /** The JNDI lookup for the debug port. */
    private static final String JNDI_DEBUG_PORT = "resource/debugPort";

    /** The JNDI lookup for the debug port. */
    private static final String JNDI_JAVA_COMMAND = "resource/javaCommand";

    /**
     * Builds the appropriate simulator for the specified simulation.
     * 
     * @param factory The factory to build the simulator.
     * @param simulation The simulation to build.
     * @return The new simulator that was built.
     * @throws SimResourceException If a new simulator cannot be built.
     */
    static SimulatorInterface buildSimulator(SimFactory factory, Simulation simulation) throws SimResourceException {

        return buildSimulator(factory, String.format("sim%d", simulation.getId()), simulation, 0);
    }

    /**
     * Builds the appropriate simulator for the specified simulation.
     * 
     * @param factory The factory to build the simulator.
     * @param uuid The string UUID to set.
     * @param simulation The simulation to build.
     * @param seedNumber The integer seed number to execute.
     * @return The new simulator that was built.
     * @throws SimResourceException If a new simulator cannot be built.
     */
    static SimulatorInterface buildSimulator(SimFactory factory, String uuid, Simulation simulation, int seedNumber) throws SimResourceException {

        byte[] fileData = simulation.getFileData().getData();

        switch (simulation.getType()) {

            case PLAYBACK:
                return factory.createSim(uuid, PlaybackSimulator.class, SimulationBuilder.createPlaybackConfiguration(uuid, fileData));
            case TEXAS:
                return factory.createSim(uuid, EtexasRmiWrapper.class, SimulationBuilder.createEtexasConfiguration(uuid, fileData, seedNumber));

            default:
        }

        throw new IllegalStateException("An unrecognized simulation type was assigned to the simulation.");
    }

    /**
     * Returns configuration properties for a new eTEXAS simulation.
     * 
     * @param uuid The string identifier for the simulation.
     * @param data The bytes of file data for the simulation.
     * @param seedNumber The integer seed number to execute.
     * @return A map of configuration properties for a new eTEXAS simulation.
     */
    private static Map<String, Object> createEtexasConfiguration(String uuid, byte[] data, int seedNumber) {

        Map<String, Object> configMap = new HashMap<String, Object>();
        configMap.put(EtexasRmiWrapper.KEY_SIM_ID, uuid);

        try {

            InitialContext context = new InitialContext();
            configMap.put(EtexasRmiWrapper.KEY_PROJECT_FOLDER, (String)context.lookup(SimulationBuilder.JNDI_DIRECTORY_TEXAS_PROJECT));
            configMap.put(EtexasRmiWrapper.KEY_SYSDAT_FOLDER, (String)context.lookup(SimulationBuilder.JNDI_DIRECTORY_TEXAS_SYS));
            configMap.put(EtexasRmiWrapper.KEY_EXEC_FOLDER, (String)context.lookup(SimulationBuilder.JNDI_DIRECTORY_TEXAS_EXE));

            try {

                configMap.put(EtexasRmiWrapper.KEY_DEBUG_PORT, (String)context.lookup(SimulationBuilder.JNDI_DEBUG_PORT));
            }
            catch (NamingException exception) {

                LoggerFactory.getLogger(SimulationBuilder.class).debug("Debugging TEXAS disabled: no port number property was found.");
            }

            try {

                configMap.put(EtexasRmiWrapper.KEY_JAVA_COMMAND, (String)context.lookup(SimulationBuilder.JNDI_JAVA_COMMAND));
            }
            catch (NamingException exception) {

                LoggerFactory.getLogger(SimulationBuilder.class).debug("Using default Java command: no java command was provided.");
            }
        }
        catch (NamingException exception) {

            LoggerFactory.getLogger(SimulationBuilder.class).error("", exception);
            throw new RuntimeException(exception);
        }

        configMap.put(EtexasRmiWrapper.KEY_ARCHIVE, data);
        configMap.put(EtexasRmiWrapper.KEY_NREP, 1);
        configMap.put(EtexasRmiWrapper.KEY_SEED, seedNumber);
        configMap.put(EtexasRmiWrapper.KEY_PORT, 1099);

        return configMap;
    }

    /**
     * Returns configuration properties for a new Playback simulation.
     * 
     * @param uuid The string identifier for the simulation.
     * @param data The bytes of file data for the simulation.
     * @return A map of configuration properties for a new Playback simulation.
     */
    private static Map<String, Object> createPlaybackConfiguration(String uuid, byte[] data) {

        Map<String, Object> configMap = new HashMap<String, Object>();

        try {

            InitialContext context = new InitialContext();
            configMap.put(PlaybackSimulator.FILE_LOCATION_KEY, new File(((String)context.lookup(SimulationBuilder.JNDI_DIRECTORY_PLAYBACK)), uuid).getPath());
            configMap.put(PlaybackSimulator.ARCHIVE_KEY, data);
        }
        catch (NamingException exception) {

            LoggerFactory.getLogger(SimulationBuilder.class).error("", exception);
            throw new RuntimeException(exception);
        }

        return configMap;
    }

    /* prevents instantiation */
    private SimulationBuilder() {}
}