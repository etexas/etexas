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

/**
 * The messages as used by the wave simulator
 * 
 * @author ablatt
 */
public class WaveMessage {

    /**
     * Value used for the destination mac when the message type is broadcast
     */
    public static final long MACBROADCAST = 0xFFFFFFFFFFFFl;

    /**
     * The message id as understood by the data layer
     */
    public final String messageId;

    /**
     * The destination address of the message
     */
    public final long destination;

    /**
     * The size of the message
     */
    public final int size;

    /**
     * Constructor
     * 
     * @param messageId The message id as understood by the data layer
     * @param dest The destination address of the message
     * @param size The size of the message
     */
    public WaveMessage(String messageId, long dest, int size) {
        this.messageId = messageId;
        destination = dest;
        this.size = size;
    }

    /**
     * Conveys if this message is to be broadcast
     * 
     * @return Is this message to be broadcast?
     */
    public boolean isBroadcast() {
        return destination == MACBROADCAST;
    }
}
