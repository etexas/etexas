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
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.rmi.AccessException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;
import java.util.List;

import org.etexascode.CoberturaIgnore;
import org.etexascode.api.eTEXAS;
import org.etexascode.interrep.datamodel.RemoteSimulatorInterface;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException.SimResourceErrorType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A runner for starting new eTEXAS simulation processes. This entire class should be
 * package-private.
 * 
 * @author bbadillo
 */
class SimProcessRunner {

    /** Logger for convenience. */
    private static final Logger LOGGER = LoggerFactory.getLogger(SimProcessRunner.class);

    /** Unique identifier to reference the RMI remote object. */
    private String uuid;

    /** The port used to reference the RMI remote object. */
    private int port;

    /** The process commands to use for starting the process. */
    private ProcessBuilder processBuilder;

    /** The process managed by this class. */
    private Process process;

    /** A thread that acts as a stream gobbler. */
    private Thread outputThread;

    /**
     * The input stream representing the output from the process managed by this class.
     */
    private BufferedReader reader;

    /**
     * Constructor
     * 
     * @param uuid The UUID for server.
     * @param port The port number that the server.
     * @param libPath The path to the native SIMPRO shared library.
     * @param projDir The {@link File} object that represents the project folder.
     * @param sysDatDir The path to the TEXAS sys_dat directory.
     * @param javaCommand The path to a "java" binary to use to run eTEXAS.
     */
    SimProcessRunner(String uuid, int port, String libPath, File projDir, String sysDatDir, String javaCommand, String debugPort) {

        this.uuid = uuid;

        this.port = port;

        try {
            // Find the path to the shared library
            String simDTPath = new File(libPath).getCanonicalPath();

            List<String> command = new ArrayList<String>();
            if (javaCommand == null || javaCommand.isEmpty()) {
                command.add("java");
            }
            else {
                command.add(javaCommand);
            }
            // NOTE: These parameters are for debugging purposes only.
            if (debugPort != null) {
                command.add("-Xdebug");
                command.add("-Xrunjdwp:transport=dt_socket,address=" + debugPort + ",server=y,suspend=n");
            }
            command.add("-Djna.library.path=" + simDTPath);

            try {
                URI cpURI1 = SimulatorInterface.class.getProtectionDomain().getCodeSource().getLocation().toURI();
                cpURI1 = fixJBossURI(cpURI1);
                File cpFile1 = new File(cpURI1);
                URI cpURI2 = EtexasDriver.class.getProtectionDomain().getCodeSource().getLocation().toURI();
                cpURI2 = fixJBossURI(cpURI2);
                File cpFile2 = new File(cpURI2);
                URI cpURI3 = eTEXAS.class.getProtectionDomain().getCodeSource().getLocation().toURI();
                cpURI3 = fixJBossURI(cpURI3);
                File cpFile3 = new File(cpURI3);
                String cpString = String.format("%s%s%s%s%s", cpFile1.toString(), File.pathSeparator, cpFile2.toString(), File.pathSeparator, cpFile3.toString());
                LOGGER.info("[eTEXAS RA] Using classpath of {}", cpString);
                // NOTE: bbadillo - The following line adds a code base path for
                // Java RMI.
                // However, it proved to be unnecessary in Windows platforms and
                // somehow caused errors in Linux platforms. As such it has been
                // commented out.
                // command.add("-Djava.rmi.server.codebase=\"" + cpURI + "\"");
                command.add("-cp");
                command.add(cpString);
                // command.add(SimDriver.class.getName());
                command.add(EtexasDriver.class.getName());
                command.add(String.valueOf(uuid));
                command.add(String.valueOf(port));
            }
            catch (URISyntaxException ex) {
                LOGGER.debug(ex.toString());
            }

            processBuilder = new ProcessBuilder(command);
            // Set the project directory as the current working directory
            processBuilder.directory(projDir);
            processBuilder.redirectErrorStream(true);

        }
        catch (IOException ex) {
            LOGGER.debug(ex.toString());
        }
    }

    /**
     * Starts the eTEXAS process.
     * 
     * @throws IOException
     */
    @CoberturaIgnore
    void start() throws IOException {
        process = processBuilder.start();

        InputStreamReader inputStreamReader = new InputStreamReader(process.getInputStream(), "ASCII");
        reader = new BufferedReader(inputStreamReader);

        outputThread = new Thread(new Runnable() {

            @Override
            public void run() {
                try {
                    String readLine;
                    while (reader != null) {

                        if (!reader.ready()) {
                            try {
                                Thread.sleep(1000);
                            }
                            catch (InterruptedException ex) {
                                LOGGER.info("eTEXAS output thread interupted.");
                            }
                        }
                        else {

                            readLine = reader.readLine();
                            if (readLine != null && readLine.trim().length() > 0) {
                                LOGGER.info(readLine);
                            }
                        }
                    }
                }
                catch (IOException ex) {
                    LOGGER.debug(ex.toString());
                }
                finally {
                    try {
                        if (reader != null) {
                            reader.close();
                        }
                    }
                    catch (IOException ex) {
                        LOGGER.debug(ex.toString());
                    }
                    finally {
                        reader = null;
                    }
                }
            }
        }, "etexas-output-thread");
        outputThread.start();
    }

    /**
     * Stop the eTEXAS process and clean up resources.
     */
    @CoberturaIgnore
    void stop() {
        LOGGER.info("Closing eTEXASLauncher for {}", uuid);
        outputThread.interrupt();
        outputThread = null;

        try {
            reader.close();
        }
        catch (IOException ex) {
            LOGGER.debug(ex.toString());
        }
        finally {
            try {
                process.getOutputStream().close();
            }
            catch (IOException ex) {
                LOGGER.debug(ex.toString());
            }
            finally {
                process.destroy();
                reader = null;
                process = null;

                try {
                    Registry registry = LocateRegistry.getRegistry(port);
                    try {
                        registry.unbind(uuid);
                    }
                    catch (NotBoundException ex) {
                        LOGGER.debug(null, ex);
                    }
                    catch (AccessException ex) {
                        LOGGER.debug(null, ex);
                    }
                }
                catch (RemoteException ex) {
                    LOGGER.debug(null, ex);
                }
            }
        }
    }

    /**
     * Connects to the remote simulator interface.
     * 
     * @return The remote simulator interface.
     * @throws SimResourceException
     * @throws AccessException
     */
    @CoberturaIgnore
    RemoteSimulatorInterface connectToSimDriver() throws SimResourceException, AccessException {
        for (int i = 1; i <= 10; i++) {
            try {
                Registry registry = LocateRegistry.getRegistry(port);
                try {
                    RemoteSimulatorInterface simDriver = (RemoteSimulatorInterface)registry.lookup(uuid);
                    return simDriver;
                }
                catch (NotBoundException ex) {
                    LOGGER.warn("Could not connect to TEXAS via RMI using uuid={}, {} of 10 tries...", new Object[] { uuid, i });
                }
            }
            catch (RemoteException ex) {
                LOGGER.debug(ex.toString());
            }

            try {
                Thread.sleep(1000);
            }
            catch (InterruptedException e) {
                LOGGER.debug(e.toString());
            }
        }

        throw new SimResourceException("Could not connect to TEXAS via RMI using uuid=" + uuid, SimResourceErrorType.REMOTE);
    }

    /**
     * Fixes an issue with JBoss where the URI begins with jar:file:/ instead of just file:/.
     * 
     * @param uri The URI to fix.
     * @return The new URI.
     */
    @CoberturaIgnore
    private URI fixJBossURI(URI uri) {
        String file = uri.toString();
        if (!file.startsWith("jar")) {
            return uri;
        }
        file = file.substring(4, file.length() - 2);
        try {
            return new URI(file);
        }
        catch (URISyntaxException e) {
            LOGGER.error("Unable to fix JBoss URI.", e);
        }
        return uri;
    }
}