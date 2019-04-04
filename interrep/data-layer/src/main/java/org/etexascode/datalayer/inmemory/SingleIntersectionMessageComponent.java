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
package org.etexascode.datalayer.inmemory;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.WaveMessage;

/**
 * Component for managing the messages for a single intersection.
 * 
 * @author ablatt
 */
public class SingleIntersectionMessageComponent implements IMessageComponent {

    /**
     * Messages currently in storage which were output by the app layer but not yet processed by the
     * wave sim
     */
    Map<String, BasicMessage> messageStorage = new HashMap<String, BasicMessage>();

    /**
     * The current inputs into the wave simulator
     */
    List<Tx> waveSimInputs = new LinkedList<Tx>();

    /**
     * The current inputs to the app layer
     */
    Map<Integer, Map<Long, List<BasicMessage>>> appInputs = new HashMap<Integer, Map<Long, List<BasicMessage>>>();

    /**
     * A means of managing the message id such that every id is unique
     */
    long messId = 0;

    /**
     * Adds application layer messages to the storage area
     * 
     * @param stepNum The current step number
     * @param outputs The output messages to be added
     */
    @Override
    public void putAppLayerOutputs(int stepNum, Collection<AppLayerOutput> outputs) {
        waveSimInputs = new ArrayList<Tx>(outputs.size());
        messageStorage.clear();
        appInputs.remove(stepNum - 1);
        for (AppLayerOutput alo : outputs) {
            if (alo.getMessageType() != null) {
                List<WaveMessage> messes = new ArrayList<WaveMessage>(alo.getOutMessages().size());
                for (BasicMessage message : alo.getOutMessages()) {
                    String s = String.valueOf(messId);
                    messId++;
                    messageStorage.put(s, message);
                    messes.add(new WaveMessage(s, message.getPeerMACAddress(), message.getSize()));
                }
                waveSimInputs.add(new Tx(alo.getMessageType(), alo.location, alo.mac, messes));
            }
        }
    }

    /**
     * Reads application messages from storage into the message component
     * 
     * @param stepNum The current step number
     * @param outputs The messages to add
     */
    @Override
    public void putWaveSimOutputs(int stepNum, Iterable<Rx> outputs) {
        for (Rx r : outputs) {
            for (Entry<String, Integer> entry : r.messages.entrySet()) {

                BasicMessage message = messageStorage.get(entry.getKey());

                // Create a new message for the Rx so that separate objects exist for Rxs vs
                // Txs.
                BasicMessage routedInd = message.copy();
                routedInd.setPeerMACAddress(r.mac);

                // Get timestep that Rx will occur.
                int timeStep = entry.getValue().intValue();

                // Get map of messages keyed by device id.
                Map<Long, List<BasicMessage>> messageMap = appInputs.get(timeStep);
                if (messageMap == null) {
                    messageMap = new HashMap<Long, List<BasicMessage>>();
                    appInputs.put(timeStep, messageMap);
                }

                // Get list of messages for device id.
                List<BasicMessage> deviceMessages = messageMap.get(r.mac);
                if (deviceMessages == null) {
                    deviceMessages = new LinkedList<BasicMessage>();
                    messageMap.put(r.mac, deviceMessages);
                }

                // Add new message for device.
                deviceMessages.add(routedInd);
            }
        }
    }

    /**
     * Gets the wave sim inputs
     * 
     * @param stepNum The current step number
     * @param p The polygon representing the wave sim
     * @return The wave sim inputs
     */
    @Override
    public Iterable<Tx> getWaveSimInputs(int stepNum, Polygon p) {
        return waveSimInputs;
    }

    /**
     * Gets the messages for devices by step number
     * 
     * @param stepNum The current step number
     * @return The messages
     */
    @Override
    public Map<Long, List<BasicMessage>> getIndicationsForDevicesByTimeStep(int stepNum) {
        Map<Long, List<BasicMessage>> tmp = appInputs.get(stepNum);
        if (tmp == null) {
            return new HashMap<Long, List<BasicMessage>>();
        }
        else {
            return tmp;
        }
    }

    /**
     * Gets the messages currently in storage
     * 
     * @return messageStorage The messages that are currently in storage
     */
    @Override
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum) {

        Map<Long, List<BasicMessage>> messageMap = new HashMap<Long, List<BasicMessage>>();

        for (BasicMessage message : messageStorage.values()) {

            List<BasicMessage> deviceMessages = messageMap.get(message.getOriginMACAddress());
            if (deviceMessages == null) {
                deviceMessages = new ArrayList<BasicMessage>();
                messageMap.put(message.getOriginMACAddress(), deviceMessages);
            }

            deviceMessages.add(message);
        }

        return messageMap;
    }

    @Override
    public void putAllDeviceOutputs(int stepNum, Collection<AppLayerOutput> outputs, Map<Long, IDistanceable> locationsMap, Iterable<IDeviceData> devices) {
        putAppLayerOutputs(stepNum, outputs);
    }
}
