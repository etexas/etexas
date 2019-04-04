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
package org.etexascode.wavesim;

import java.util.List;

import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * Input to the wave simulator
 * 
 * @author ablatt
 */
public class Tx implements IDistanceable {

    public enum MessageType {
        DSRC,
        CELLULAR
    };

    /**
     * The Type of message that this tx is.
     */
    public final MessageType messageType;

    /**
     * The origin location which is emitting the messages
     */
    public final IDistanceable node;

    /**
     * The mac address of the node
     */
    public final long mac;

    /**
     * The messages being sent from this node
     */
    public final List<WaveMessage> outgoingMessages;

    /**
     * Constructor
     * 
     * @param messageType the type of message (dsrc, cellular)
     * @param node The location of this node
     * @param mac The mac of this node
     * @param messages The messages being sent from this node
     */
    public Tx(MessageType messageType, IDistanceable node, long mac, List<WaveMessage> messages) {
        this.messageType = messageType;
        this.node = node;
        this.mac = mac;
        this.outgoingMessages = messages;
    }

    @Override
    @CoberturaIgnore
    public double getX() {
        return node.getX();
    }

    @Override
    @CoberturaIgnore
    public double getY() {
        return node.getY();
    }

    @Override
    @CoberturaIgnore
    public double getZ() {
        return node.getZ();
    }
}
