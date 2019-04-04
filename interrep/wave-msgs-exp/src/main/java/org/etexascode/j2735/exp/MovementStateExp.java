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
 * The movement state object.
 * 
 * @author jrutherford.
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MovementStateExp", propOrder = { "beginning", "laneSet", "currState", "minTimeRemaining", "maxTimeRemaining", "yellState", "yellTimeToChange", "pedDetect", "pedCount", "any" })
public class MovementStateExp implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = -3415929127321391778L;

    // Constants.
    public static final byte MS_MOVEMENT = (byte)0x04;

    public static final byte MS_LANESET = (byte)0x05;

    public static final byte MS_CURRENT_STATE = (byte)0x06;

    public static final byte MS_MIN_TIME_REMAIN = (byte)0x07;

    public static final byte MS_MAX_TIME_REMAIN = (byte)0x08;

    public static final byte MS_YELLOW_STATE = (byte)0x09;

    public static final byte MS_YELLOW_TIME = (byte)0x0A;

    public static final byte MS_PED_DETECTED = (byte)0x0B;

    public static final byte MS_VEH_PED_COUNT = (byte)0x0C;

    /** The beginning of a movement state message. This is required. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected byte beginning = MS_MOVEMENT;

    /** The lane set. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected MessageObjectExp laneSet = new MessageObjectExp(MS_LANESET, (byte)0x00);

    /** The current state. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected MessageObjectExp currState = new MessageObjectExp(MS_CURRENT_STATE, (byte)0x00);

    /** The minimum time remaining. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected MessageObjectExp minTimeRemaining = new MessageObjectExp(MS_MIN_TIME_REMAIN, (byte)0x02);

    /** The maximum time remaining. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected MessageObjectExp maxTimeRemaining = new MessageObjectExp(MS_MAX_TIME_REMAIN, (byte)0x02);

    /** The yellow state. */
    @XmlElement(type = MessageObjectExp.class)
    protected MessageObjectExp yellState;

    /** The yellow time to change. */
    @XmlElement(type = MessageObjectExp.class)
    protected MessageObjectExp yellTimeToChange;

    /** The pedestrian detected. */
    @XmlElement(type = MessageObjectExp.class)
    protected MessageObjectExp pedDetect;

    /** The pedestrian count. */
    @XmlElement(type = MessageObjectExp.class)
    protected MessageObjectExp pedCount;

    /** Any other object. */
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the laneSet property.
     * 
     * @return The lane set.
     */
    public MessageObjectExp getLaneSet() {
        return laneSet;
    }

    /**
     * Gets the value of the currState property.
     * 
     * @return The current state.
     */
    public MessageObjectExp getCurrState() {
        return currState;
    }

    /**
     * Gets the minimum time remaining.
     * 
     * @return The minimum time remaining object.
     */
    public MessageObjectExp getMinTimeRemaining() {
        return minTimeRemaining;
    }

    /**
     * The maximum time remaining object.
     * 
     * @return The maximum time remaining.
     */
    public MessageObjectExp getMaxTimeRemaining() {
        return maxTimeRemaining;
    }

    /**
     * Gets the value of the yellState property.
     * 
     * @return The yellow state object.
     */
    public MessageObjectExp getYellState() {
        yellState = new MessageObjectExp(MS_YELLOW_STATE, (byte)0x00);
        return yellState;
    }

    /**
     * Gets the value of the yellTimeToChange property.
     * 
     * @return Time to yellow change property.
     */
    public MessageObjectExp getYellTimeToChange() {
        yellTimeToChange = new MessageObjectExp(MS_YELLOW_TIME, (byte)0x02);
        return yellTimeToChange;
    }

    /**
     * Gets the value of the pedDetect property.
     * 
     * @return Pedestrian detected object.
     */
    public MessageObjectExp getPedDetect() {
        pedDetect = new MessageObjectExp(MS_PED_DETECTED, (byte)0x01);
        return pedDetect;
    }

    /**
     * Gets the value of the pedCount property.
     * 
     * @return pedestrian count object.
     */
    public MessageObjectExp getPedCount() {
        pedCount = new MessageObjectExp(MS_VEH_PED_COUNT, (byte)0x01);
        return pedCount;
    }

    /**
     * Gets the value of the any property.
     * 
     * @return the object.
     */
    public Object getAny() {
        return any;
    }

    /**
     * Sets the value of the any property.
     * 
     * @param value the new value.
     */
    public void setAny(Object value) {
        this.any = value;
    }
}