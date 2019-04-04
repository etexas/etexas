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
package org.etexascode.devicedata;

import java.util.Collection;

import org.etexascode.CoberturaIgnore;

/**
 * Class used to distinguish between native app definitions and actually created/simulated apps
 * (instances).
 * 
 * @author janway
 */
public class NativeApp<T> implements IConnectedVehicleApp<T>, IAppLifecycle, IAppName {

    /**
     * The id of the native app definition this is an instance of.
     */
    private String appId;

    /**
     * The command the native agent should run to start this app process.
     */
    private String commandLine;

    /**
     * The host the app will run on.
     */
    private String hostName;

    /**
     * The port the app will run on.
     */
    private int portNumber;

    /**
     * Default constructor - necessary for construction using Class.newInstance()
     */
    public NativeApp() {

    }

    /**
     * Constructor
     * 
     * @param appId The appId to set
     * @param commandLine The command for the app
     * @param hostName The host VM for the app
     * @param portNumber The port for the app
     */
    public NativeApp(String appId, String commandLine, String hostName, int portNumber) {
        this.appId = appId;
        this.commandLine = commandLine;
        this.hostName = hostName;
        this.portNumber = portNumber;
    }

    @Override
    public void init(String[] appConfigs) {
        this.appId = appConfigs[0];
        this.commandLine = appConfigs[1];
        this.hostName = appConfigs[2];
        this.portNumber = Integer.parseInt(appConfigs[3]);
    }

    @CoberturaIgnore
    @Override
    public void appShutdown(AppLogger logger) {
        // do nothing
    }

    /**
     * Getter
     * 
     * @return the appId
     */
    @Override
    public String getAppName() {
        return appId;
    }

    @CoberturaIgnore
    @Override
    public void performUpdate(T device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        // do nothing
    }

    /**
     * Getter
     * 
     * @return the commandLine
     */
    public String getCommandLine() {
        return commandLine;
    }

    /**
     * Getter
     * 
     * @return the hostName
     */
    public String getHostName() {
        return hostName;
    }

    /**
     * Getter
     * 
     * @return the portNumber
     */
    public int getPortNumber() {
        return portNumber;
    }
}
