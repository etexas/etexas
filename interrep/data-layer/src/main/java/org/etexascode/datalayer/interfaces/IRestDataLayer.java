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
package org.etexascode.datalayer.interfaces;

import java.util.List;

import org.etexascode.appslayerdata.RemoteProxyApp;
import org.etexascode.devicedata.IDeviceData;

/**
 * The data layer used by the REST API.
 * 
 * @author jrutherford
 * @author ablatt
 * @author janway
 */
public interface IRestDataLayer {

    /**
     * Gets the list of application devices that were started during the current time step.
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The list of application devices that were started.
     */
    public Iterable<IDeviceData> getStartedAppDevices(int stepNum);

    /**
     * Gets the started remote apps.
     * 
     * @param timestep The timestep.
     * @return List of started remote apps.
     */
    public List<RemoteProxyApp<?>> getStartedRemoteApps(int timestep);

    /**
     * Gets the list of application devices that were stopped during the current time step.
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The list of application devices that were stopped.
     */
    public Iterable<IDeviceData> getStoppedAppDevices(int stepNum);

    /**
     * Gets the stopped remote apps.
     * 
     * @param timestep The timestep.
     * @return The stopped remote apps.
     */
    public List<RemoteProxyApp<?>> getStoppedRemoteApps(int timestep);

    /**
     * Gets the current time in the simulation.
     * 
     * @param stepNum The step number to get the time for.
     * @return The time in seconds.
     */
    public double getSimTime(int stepNum);
}