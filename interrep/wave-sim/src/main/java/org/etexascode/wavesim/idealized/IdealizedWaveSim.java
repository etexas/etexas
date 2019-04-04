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
package org.etexascode.wavesim.idealized;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.datalayer.interfaces.IWaveSimDataLayer;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.topography.ITopography;
import org.etexascode.interrep.topography.ITopographyFeature;
import org.etexascode.interrep.topography.TopographyMessageType;
import org.etexascode.interrep.topography.Vector3;
import org.etexascode.wavesim.IDropPacket;
import org.etexascode.wavesim.IPacketTransmission;
import org.etexascode.wavesim.IWaveSim;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.etexascode.wavesim.WaveMessage;
import org.etexascode.wavesim.WaveSimLayer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Idealized wave simulator
 * 
 * @author ablatt
 * @author ttevendale
 */
public class IdealizedWaveSim implements IWaveSim {

    /**
     * Logger for convenience
     */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(IdealizedWaveSim.class);

    /**
     * Data layer
     */
    public final IWaveSimDataLayer data;

    /**
     * Polygon which the relevant Txs should fall in
     */
    public final Polygon segment;

    /**
     * Config for dropping packets //NOTICE That this config isn't being used at all below, and you
     * won't see this in the review but the PacketAlwaysMakesIt.java is made and tested incorrectly
     */
    public final IDropPacket packetDropper;

    /**
     * Config for how long the packets take
     */
    public final IPacketTransmission packetTrans;

    /**
     * The topography to be used for message loss.
     */
    public final ITopography topography;

    /**
     * The propagation loss model to be used.
     */
    public final PropagationLossModel propagationLossModel;

    /**
     * Constructor.
     * 
     * @param segment The area to get nodes from.
     * @param packetDropper Determines which packets are dropped.
     * @param packetTrans Handles packet transmission.
     * @param data The data layer.
     * @param topography The topography to use.
     * @param propagationLossModel The propagation loss model to use.
     */
    public IdealizedWaveSim(Polygon segment, IDropPacket packetDropper, IPacketTransmission packetTrans, IWaveSimDataLayer data, ITopography topography, PropagationLossModel propagationLossModel) {
        this.segment = segment;
        this.packetDropper = packetDropper;
        this.packetTrans = packetTrans;
        this.data = data;
        this.topography = topography;
        this.propagationLossModel = propagationLossModel;
    }

    /**
     * Adds node messages that were supposed to be sent out at a specific step to data
     * 
     * @param stepNum the time step for the current simulation
     */
    @Override
    public void transmitMessages(int stepNum) {
        Iterable<Tx> nodes = data.getTxs(stepNum, segment);
        Map<Long, Rx> out = new HashMap<Long, Rx>();
        Map<Long, Tx> mapOfMacToNodes = new HashMap<Long, Tx>();
        Map<String, Tx> mapOfMessageIdsToNodes = new HashMap<String, Tx>();

        for (Tx node : nodes) {
            for (WaveMessage wm : node.outgoingMessages) {

                mapOfMessageIdsToNodes.put(wm.messageId, node);
            }
            out.put(node.mac, new Rx(node.mac));
            mapOfMacToNodes.put(node.mac, node);

        }
        for (Tx node : nodes) {
            for (WaveMessage wm : node.outgoingMessages) {
                if (wm.isBroadcast()) {
                    for (Entry<Long, Tx> entry : mapOfMacToNodes.entrySet()) {
                        if (!(node.mac == entry.getKey().longValue()) && node.messageType.equals(entry.getValue().messageType)) {
                            out.get(entry.getKey()).messages.put(wm.messageId, stepNum + packetTrans.transmitPacket(node, entry.getValue()));
                        }
                    }
                }
                else {
                    // check if destination node exists
                    Rx rx = out.get(wm.destination);
                    if (rx != null) {
                        rx.messages.put(wm.messageId, stepNum + packetTrans.transmitPacket(node, mapOfMacToNodes.get(wm.destination)));
                    }
                }
            }
        }

        Collection<Rx> rxs = this.handleTopography(out.values(), mapOfMacToNodes, mapOfMessageIdsToNodes);
        rxs = this.handleDSRCMaxDistance(rxs, mapOfMacToNodes, mapOfMessageIdsToNodes);

        data.putMessages(stepNum, rxs);
    }

    /**
     * Handles the topography message loss.
     * 
     * @param rxs The currently received messages.
     * @param mapOfMacToNodes The map of MAC address to Tx nodes.
     * @param mapOfMessageIdsToNodes The map of Message IDs to Tx nodes.
     * @return The new Rx collection.
     */
    private Collection<Rx> handleTopography(Collection<Rx> rxs, Map<Long, Tx> mapOfMacToNodes, Map<String, Tx> mapOfMessageIdsToNodes) {

        if (topography != null) {

            Collection<Rx> receivedMessages = new ArrayList<Rx>();

            for (Rx rx : rxs) {

                Rx newRx = new Rx(rx.mac);
                for (String id : rx.messages.keySet()) {

                    Tx source = mapOfMessageIdsToNodes.get(id);
                    Tx destination = mapOfMacToNodes.get(rx.mac);

                    Vector3 sourceVector = new Vector3(source.getX(), source.getY(), source.getZ());
                    Vector3 destinationVector = new Vector3(destination.getX(), destination.getY(), destination.getZ());

                    TopographyMessageType messageType = null;

                    if (MessageType.DSRC.equals(source.messageType)) {

                        messageType = TopographyMessageType.DSRC;
                    }
                    else if (MessageType.CELLULAR.equals(source.messageType)) {

                        messageType = TopographyMessageType.CELLULAR;
                    }

                    ITopographyFeature feature = topography.getObstruction(sourceVector, destinationVector, messageType);
                    if (feature == null) {

                        newRx.messages.put(id, rx.messages.get(id));
                    }

                }
                if (!newRx.messages.isEmpty()) {

                    receivedMessages.add(newRx);
                }
            }
            return receivedMessages;
        }

        return rxs;
    }

    /**
     * Handles the DSRC messages max distance loss.
     * 
     * @param rxs The currently received messages.
     * @param mapOfMacToNodes The map of MAC address to Tx nodes.
     * @param mapOfMessageIdsToNodes The map of Message IDs to Tx nodes.
     * @return The new Rx collection.
     */
    private Collection<Rx> handleDSRCMaxDistance(Collection<Rx> rxs, Map<Long, Tx> mapOfMacToNodes, Map<String, Tx> mapOfMessageIdsToNodes) {

        Collection<Rx> receivedMessages = new ArrayList<Rx>();

        for (Rx rx : rxs) {

            Tx rxNode = mapOfMacToNodes.get(rx.mac);
            if (!MessageType.DSRC.equals(rxNode.messageType)) {

                receivedMessages.add(rx);
                continue;
            }

            Rx newRx = new Rx(rx.mac);

            for (String id : rx.messages.keySet()) {

                if (UtilsCalculations.getDistance(rxNode, mapOfMessageIdsToNodes.get(id)) <= getMaxDSRCDistance()) {

                    newRx.messages.put(id, rx.messages.get(id));
                }
            }

            if (!newRx.messages.isEmpty()) {

                receivedMessages.add(newRx);
            }
        }
        return receivedMessages;
    }

    /**
     * Gets the max DSRC distance based on the propagation loss model. (Defaults to urban)
     * 
     * @return The max DSRC distance.
     */
    private int getMaxDSRCDistance() {

        int maxDistance = WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_URBAN;

        if (propagationLossModel == PropagationLossModel.OPEN) {

            maxDistance = WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_OPEN;
        }
        else if (propagationLossModel == PropagationLossModel.SUBURBAN) {

            maxDistance = WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_SUBURBAN;
        }
        return maxDistance;
    }
}
