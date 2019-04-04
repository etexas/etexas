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
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * An approach object.
 * 
 * @author jrutherford
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ApproachObjectExp", propOrder = { "approach",
        // "barrierAttributes",
        "vehicleLanes", "any" })
public class ApproachObjectExp implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = -6542826310619841703L;

    // Constants.
    public static final byte GID_APPROACH = (byte)0x04;

    public static final byte GID_LANE_ATTRIBUTES = (byte)0x06;

    public static final byte GID_BARRIER_ATTRIBUTES = (byte)0x07;

    /** The approach value. */
    @XmlElement(required = true)
    protected MessageObjectExp approach = new MessageObjectExp(GID_APPROACH, (byte)0x01);

    @XmlElement(required = true)
    protected ApproachLanes vehicleLanes;

    /** Any object. */
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the approach.
     * 
     * @return The new approach.
     */
    public MessageObjectExp getApproach() {
        return approach;
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

    /**
     * Gets the approach lanes for vehicles
     * 
     * @return the approach lanes
     */
    public ApproachLanes getVehicleLanes() {
        if (vehicleLanes == null) {
            vehicleLanes = new ApproachLanes();
        }

        return vehicleLanes;
    }

    /**
     * Holds the list of approaches.
     * 
     * @author ablatt
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "approachObject" })
    public static class ApproachLanes implements Serializable {

        /** Serial ID. */
        private static final long serialVersionUID = -6169281673833183629L;

        /** The list. */
        @XmlElement(name = "ApproachObject", required = true)
        protected List<ApproachLaneExp> approachObject;

        public List<ApproachLaneExp> getApproachLaneObject() {
            if (approachObject == null) {
                approachObject = new ArrayList<ApproachLaneExp>();
            }
            return this.approachObject;
        }
    }
}