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

package org.etexascode.webapp.datamodel;

import java.io.Serializable;

import org.etexascode.devicedata.DSRCChannel;

/**
 * Class to hold info for messages that is displayed in the web-app
 * 
 * @author dranker
 * @author ablatt
 * @author bmauldon
 * @author ttevendale
 */
public class MessageData implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 32660976742624956L;

    /** DSRC channel */
    private DSRCChannel channel;

    /** destination address */
    private long destinationAddress;

    /** formatted message object */
    private Object formattedMessage;

    /** message object */
    private Object message;

    /** message ID */
    private int messageId;

    /** message type */
    private String messageType;

    /** origin address */
    private long originAddress;

    /** owner device address */
    private long ownerDeviceAddress;

    /** type of message (Tx, Rx) */
    private String type;

    /**
     * Get DSRC channel.
     * 
     * @return the DSRC channel.
     */
    public DSRCChannel getChannel() {

        return channel;
    }

    /**
     * Set DSRC channel.
     * 
     * @param channel The DSRC channel.
     */
    public void setChannel(DSRCChannel channel) {

        this.channel = channel;
    }

    /**
     * Get destination address
     * 
     * @return the message's destination address
     */
    public long getDestinationAddress() {

        return destinationAddress;
    }

    /**
     * Set destination address
     * 
     * @param destinationAddress The destination address of this message
     */
    public void setDestinationAddress(long destinationAddress) {

        this.destinationAddress = destinationAddress;
    }

    /**
     * Get formatted message.
     * 
     * @return the formatted message.
     */
    public Object getFormattedMessage() {

        return formattedMessage;
    }

    /**
     * Set formatted message.
     * 
     * @param formattedMessage The formatted message.
     */
    public void setFormattedMessage(Object formattedMessage) {

        this.formattedMessage = formattedMessage;
    }

    /**
     * Get message object
     * 
     * @return the message object
     */
    public Object getMessage() {

        return message;
    }

    /**
     * Set message object
     * 
     * @param message The message
     */
    public void setMessage(Object message) {

        this.message = message;
    }

    /**
     * Gets the message ID.
     * 
     * @return The message ID.
     */
    public int getMessageId() {

        return messageId;
    }

    /**
     * Sets the message ID.
     * 
     * @param messageId The new message ID
     */
    public void setMessageId(int messageId) {

        this.messageId = messageId;
    }

    /**
     * Get message type
     * 
     * @return messageType
     */
    public String getMessageType() {

        return messageType;
    }

    /**
     * Set message type
     * 
     * @param messageType The message type to set.
     */
    public void setMessageType(String messageType) {

        this.messageType = messageType;
    }

    /**
     * Get origin address
     * 
     * @return the message's origin address
     */
    public long getOriginAddress() {

        return originAddress;
    }

    /**
     * Set origin address
     * 
     * @param originAddress The origin address of this message
     */
    public void setOriginAddress(long originAddress) {

        this.originAddress = originAddress;
    }

    /**
     * Get owner device address.
     * 
     * @return the message's owner device address.
     */
    public long getOwnerDeviceAddress() {

        return ownerDeviceAddress;
    }

    /**
     * Set owner device address.
     * 
     * @param ownerDeviceAddress The owner device address of this message.
     */
    public void setOwnerDeviceAddress(long ownerDeviceAddress) {

        this.ownerDeviceAddress = ownerDeviceAddress;
    }

    /**
     * Gets the type.
     * 
     * @return The type.
     */
    public String getType() {

        return type;
    }

    /**
     * Sets the type
     * 
     * @param type The new type.
     */
    public void setType(String type) {

        this.type = type;
    }
}
