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
package org.etexascode.j2735.exp;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Message Object
 * 
 * @author jrutherford
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MessageObjectExp", propOrder = { "identifier", "size", "payload" })
public class MessageObjectExp {

    /** The identifier. */
    @XmlElement
    private byte identifier;

    /** The size of the payload. */
    @XmlElement
    private byte size;

    /** The payload. */
    @XmlElement
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    private byte payload[];

    /** Blank constructor for XML serialization. */
    public MessageObjectExp() {

    }

    /**
     * Constructor.
     * 
     * @param id The id of the object.
     * @param size The size of the payload.
     */
    public MessageObjectExp(byte id, byte size) {
        this.identifier = id;
        this.size = size;
    }

    /**
     * Gets the identifier.
     * 
     * @return The identifier.
     */
    public byte getIdentifier() {
        return this.identifier;
    }

    /**
     * Gets the size.
     * 
     * @return Size of the payload.
     */
    public byte getSize() {
        return this.size;
    }

    /**
     * Gets the object payload.
     * 
     * @return The object payload.
     */
    public byte[] getPayload() {
        return this.payload;
    }

    /**
     * Sets the payload data.
     * 
     * @param payload The new payload.
     */
    public void setPayload(byte payload[]) {
        this.payload = payload;
        if (size != 0 && payload.length != size) {
            throw new IllegalArgumentException("Expected data of size " + size + ", but received data of size " + payload.length);
        }
        else {
            this.size = (byte)payload.length;
        }
    }
}