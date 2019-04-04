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
package org.etexascode.appslayer;

import java.util.List;

import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.devicedata.NativeApp;

/**
 * This class is responsible for communicating with the native-agent on a host VM. It provides
 * AppLayerInput and receives AppLayerOutput.
 * 
 * @author janway
 */
public interface INativeAppManager {

    /**
     * Tells the agent a new app instance is needed to be initialized.
     * 
     * @param apps An app to register with this manager.
     */
    public void registerNativeApps(List<NativeApp<?>> apps);

    /**
     * Tells the agent the execution has stopped.
     */
    public void deregisterExecution();

    /**
     * Receives output from the native-agent, parses it into AppLayerOutputs and returns them.
     * 
     * @return The output from the native apps.
     */
    public List<AppLayerOutput> getOutputs();

    /**
     * Receives input from the App Layer, restructures it and sends it to the native-agent.
     * 
     * @param inputs The input from the App Layer.
     * @param simTime The time this input corresponds to.
     */
    public void sendInputs(List<AppLayerInput> inputs, double simTime);
}
