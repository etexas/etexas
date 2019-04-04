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

import java.io.Serializable;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;

/**
 * The basic message class.
 * 
 * @author bbadillo
 * @author ttevendale
 */
public abstract class BasicMessage implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = -4098820484804894782L;

    /** The message data. */
    private final Object data;

    /** The origin MAC address. */
    private long originMACAddress;

    /** The destination MAC address. */
    private long peerMACAddress;

    /** The size of the message. */
    private int size;

    /**
     * Sets the message with data object provided associated to a destination MAC address.
     * 
     * @param data The message data.
     * @param peerMAC The destination MAC address.
     */
    public BasicMessage(Object data, long peerMAC) {

        this.data = data;
        this.peerMACAddress = peerMAC;
    }

    /**
     * Sets the message with data object provided associated to a destination MAC address.
     * 
     * @param data The message data.
     * @param peerMAC The destination MAC address.
     * @param size The size of the message.
     */
    public BasicMessage(Object data, long peerMAC, int size) {

        this.data = data;
        this.peerMACAddress = peerMAC;
        this.size = size;
    }

    /**
     * Sets the message with data object provided associated to a destination MAC address.
     * 
     * @param data The message data.
     * @param peerMAC The destination MAC address.
     * @param size The size of the message.
     * @param originMac The origin MAC address.
     */
    public BasicMessage(Object data, long peerMAC, int size, long originMac) {

        this.data = data;
        this.peerMACAddress = peerMAC;
        this.size = size;
        this.originMACAddress = originMac;
    }

    /**
     * Makes a copy of this object.
     * 
     * @return a copy of this object.
     */
    public abstract BasicMessage copy();

    /**
     * Gets the message data.
     * 
     * @return Message data.
     */
    @CoberturaIgnore
    public Object getData() {

        return data;
    }

    /**
     * Setter for the origin MAC address.
     * 
     * @param originMAC The origin mac address.
     */
    @CoberturaIgnore
    public void setOriginMACAddress(long originMAC) {

        this.originMACAddress = originMAC;
    }

    /**
     * Getter for the origin MAC address.
     * 
     * @return origin MAC Address.
     */
    @CoberturaIgnore
    public long getOriginMACAddress() {

        return originMACAddress;
    }

    /**
     * Setter for the peer MAC address.
     * 
     * @param peerMACAddress The peer mac address.
     */
    @CoberturaIgnore
    public void setPeerMACAddress(long peerMACAddress) {

        this.peerMACAddress = peerMACAddress;
    }

    /**
     * Gets the destination MAC address.
     * 
     * @return The destination MAC address.
     */
    @CoberturaIgnore
    public long getPeerMACAddress() {

        return peerMACAddress;
    }

    /**
     * Getter for size of the message.
     * 
     * @return size of the message.
     */
    @CoberturaIgnore
    public int getSize() {

        return size;
    }

    /**
     * Setter for message size in bytes.
     * 
     * @param size The size of the message.
     */
    @CoberturaIgnore
    public void setSize(int size) {

        this.size = size;
    }

    @Override
    public boolean equals(Object object) {

        if (object instanceof BasicMessage) {

            BasicMessage basicMessage = (BasicMessage)object;
            return peerMACAddress == basicMessage.peerMACAddress
                    && data.equals(basicMessage.data)
                    && size == basicMessage.size
                    && originMACAddress == basicMessage.originMACAddress;
        }
        else {

            return false;
        }
    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(31, 99).append(peerMACAddress).append(data.hashCode()).append(size).append(originMACAddress).hashCode();
    }
}
