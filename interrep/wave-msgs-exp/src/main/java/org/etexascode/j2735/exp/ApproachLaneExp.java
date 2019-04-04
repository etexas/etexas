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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * An approach lane
 * 
 * @author ablatt
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ApproachLaneExp", propOrder = { "lane", "laneAttributes", "nodeList", "any" })
public class ApproachLaneExp implements Serializable {

    /**
     * Generated Serial Id
     */
    private static final long serialVersionUID = -3220390917497684866L;

    public static final byte GID_LANE = (byte)0x05;

    public static final byte GID_LANE_ATTRIBUTES = (byte)0x06;

    public static final byte GID_WIDTH = (byte)0x08;

    public static final byte GID_NODE_LIST = (byte)0x09;

    public static final byte GID_CONNECTION = (byte)0x0A;

    public static final byte GID_REFERENCE_LANE = (byte)0x0B;

    /**
     * The lane information. Byte 1: Lane Number Byte 2: Use Constants.
     */
    @XmlElement(required = true)
    protected MessageObjectExp lane = new MessageObjectExp(GID_LANE, (byte)0x02);

    /** The lane attributes. */
    @XmlElement(required = true)
    protected MessageObjectExp laneAttributes = new MessageObjectExp(GID_LANE_ATTRIBUTES, (byte)0x02);

    /**
     * The node list. Length varies but the pattern is: 1 byte for attribute. 0 bit: Width included.
     * 1 bit: Node data packed in 12 but format. Rest of the bytes are nodes data. Nodes consist of:
     * 2 bytes: Eastern offset. 2 bytes: Northern offset. 2 bytes: Elevation (Optional). 2 bytes:
     * Width (Optional).
     */
    @XmlElement(required = true)
    protected MessageObjectExp nodeList = new MessageObjectExp(GID_NODE_LIST, (byte)0x00);

    /** Any object. */
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the lane.
     * 
     * @return The lane.
     */
    public MessageObjectExp getLane() {
        return lane;
    }

    /**
     * Gets the node list.
     * 
     * @return The node list.
     */
    public MessageObjectExp getNodeList() {
        return nodeList;
    }

    /**
     * Get the lane attribute.
     * 
     * @return The lane attribute.
     */
    public MessageObjectExp getLaneAttributes() {
        return laneAttributes;
    }

    /**
     * Gets the value of the any property.
     * 
     * @return The any object.
     */
    public Object getAny() {
        return any;
    }

    /**
     * Sets the value of the any property.
     * 
     * @param value The new object.
     */
    public void setAny(Object value) {
        this.any = value;
    }
}
