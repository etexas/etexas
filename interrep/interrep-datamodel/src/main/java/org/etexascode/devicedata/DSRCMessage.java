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

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;

/**
 * The DSRC message class.
 * 
 * @author ttevendale
 */
public class DSRCMessage extends BasicMessage {

    /** Serial ID. */
    private static final long serialVersionUID = -1794885599822018085L;

    /** The DSRC Channel that the message is using */
    private final DSRCChannel channel;

    /**
     * Sets the message with data object provided, DSRC channel, and the associated destination MAC
     * address.
     * 
     * @param data The message data.
     * @param channel The channel that the message was sent on.
     * @param peerMAC The destination MAC address.
     */
    public DSRCMessage(Object data, DSRCChannel channel, long peerMAC) {

        super(data, peerMAC);
        this.channel = channel;
    }

    /**
     * Sets the message with data object provided, DSRC channel, the associated destination MAC
     * address and the size of the message.
     * 
     * @param data The message data.
     * @param channel The channel that the message was sent on.
     * @param peerMAC The destination MAC address.
     * @param size the size of the message.
     */
    public DSRCMessage(Object data, DSRCChannel channel, long peerMAC, int size) {

        super(data, peerMAC, size);
        this.channel = channel;
    }

    /**
     * Sets the message with data object provided, DSRC channel, the associated destination MAC
     * address, the size of the message, and the origin MAC address.
     * 
     * @param data The message data.
     * @param channel The channel that the message was sent on.
     * @param peerMAC The destination MAC address.
     * @param size the size of the message.
     * @param originMAC The origin MAC address.
     */
    public DSRCMessage(Object data, DSRCChannel channel, long peerMAC, int size, long originMAC) {

        super(data, peerMAC, size, originMAC);
        this.channel = channel;
    }

    @Override
    public BasicMessage copy() {

        return new DSRCMessage(this.getData(), this.getChannel(), this.getPeerMACAddress(), this.getSize(), this.getOriginMACAddress());
    }

    /**
     * Returns the channel that this message is using.
     * 
     * @return The DSRC channel that this message is using.
     */
    @CoberturaIgnore
    public DSRCChannel getChannel() {

        return channel;
    }

    @Override
    public boolean equals(Object object) {

        if (object instanceof DSRCMessage) {

            DSRCMessage dsrcMessage = (DSRCMessage)object;
            return super.equals(dsrcMessage)
                    && channel.equals(dsrcMessage.channel);
        }
        else {

            return false;
        }
    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(851, 143).appendSuper(super.hashCode()).append(channel.hashCode()).hashCode();
    }
}
