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

import java.awt.Polygon;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;

/**
 * Interface for accessing a container for managing messages.
 * 
 * @author ablatt
 */
public interface IMessageComponent {

    /**
     * Adds messages from apps to the container.
     * 
     * @param stepNum The step number to associate the app layer outputs with.
     * @param outputs The app layer outputs to add to the container.
     */
    public void putAppLayerOutputs(int stepNum, Collection<AppLayerOutput> outputs);

    /**
     * Adds messages from apps to the container and includes devices that don't have any messages
     * (so they can still recieve messages even if they are not sending any).
     * 
     * @param stepNum The step number to associate the app layer outputs with.
     * @param outputs The app layer outputs to add to the container.
     * @param locationsMap The map of device locations by mac address.
     * @param devices The full list of active devices.
     */
    public void putAllDeviceOutputs(int stepNum, Collection<AppLayerOutput> outputs, Map<Long, IDistanceable> locationsMap, Iterable<IDeviceData> devices);

    /**
     * Adds the outputs of the wave sim to the container
     * 
     * @param stepNum The step number the wave sim is operating on
     * @param outputs The outputs of the wave sim
     */
    public void putWaveSimOutputs(int stepNum, Iterable<Rx> outputs);

    /**
     * Gets a sequence of the inputs to the wave sim of points within a polygon. A value of null for
     * the polygon denotes all inputs.
     * 
     * @param stepNum The step num the wave sim is operating on
     * @param p The polygon in which the devices emitting and receiving messages must be located
     * @return The sequence of inputs expected by the data layer
     */
    public Iterable<Tx> getWaveSimInputs(int stepNum, Polygon p);

    /**
     * Gets the messages to be received by devices on a specific time step
     * 
     * @param stepNum The time step messages are to be received on
     * @return Key: Device MAC; Value: List of messages to be received by that Device
     */
    public Map<Long, List<BasicMessage>> getIndicationsForDevicesByTimeStep(int stepNum);

    /**
     * Gets the messages just transmitted.
     * 
     * @param stepNum The time step that messages were transmitted
     * @return A map of the messages just transmitted
     */
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum);
}
