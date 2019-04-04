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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * The Intersection State of the SPAT message.
 * 
 * @author jrutherford
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "IntersectionStateExp", propOrder = { "id", "status", "timeStamp", "states", "endBlob", "any" })
public class IntersectionStateExp implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = -339879516142985026L;

    // Intersection State Data Type Constants
    public static final byte IS_INTERSECTION_ID = (byte)0x01;

    public static final byte IS_INTERSECTION_STATUS = (byte)0x02;

    public static final byte IS_MESSAGE_TIMESTAMP = (byte)0x03;

    public static final byte IS_END_OF_BLOB = (byte)0xFF;

    /** The intersection id. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected MessageObjectExp id = new MessageObjectExp(IS_INTERSECTION_ID, (byte)0x04);

    /** The intersection status. */
    @XmlElement(required = true, type = MessageObjectExp.class)
    protected MessageObjectExp status = new MessageObjectExp(IS_INTERSECTION_STATUS, (byte)0x01);

    /** The time stamp object. */
    protected MessageObjectExp timeStamp = new MessageObjectExp(IS_MESSAGE_TIMESTAMP, (byte)0x05);

    /** The intersection states. */
    @XmlElement(required = true)
    protected IntersectionStateExp.States states = new IntersectionStateExp.States();

    /** The end of blob object. */
    protected byte endBlob = IS_END_OF_BLOB;

    /** Any object. */
    protected Object any;

    /**
     * Gets the value of the id property.
     * 
     * @return The intersection id object.
     */
    public MessageObjectExp getId() {
        return id;
    }

    /**
     * Gets the value of the status property.
     * 
     * @return The status object.
     */
    public MessageObjectExp getStatus() {
        return status;
    }

    /**
     * Gets the value of the timeStamp property.
     * 
     * @return The time stamp object.
     */
    public MessageObjectExp getTimeStamp() {
        return timeStamp;
    }

    /**
     * Gets the value of the states property.
     * 
     * @return Gets the movement states.
     */
    public IntersectionStateExp.States getStates() {
        return states;
    }

    /**
     * Sets the value of the states property.
     * 
     * @param states the movement states.
     */
    public void setStates(IntersectionStateExp.States states) {
        this.states = states;
    }

    /**
     * Gets the end blob object.
     * 
     * @return The end blob object.
     */
    public byte getEndBlob() {
        return endBlob;
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
     * @param value The new object.
     */
    public void setAny(Object value) {
        this.any = value;
    }

    /**
     * The list of movement states.
     * 
     * @author jrutherford
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "movementState" })
    public static class States implements Serializable {

        /** Serial ID. */
        private static final long serialVersionUID = -1778491847323527846L;

        @XmlElement(name = "MovementState", required = true)
        protected List<MovementStateExp> movementState;

        /**
         * Gets the value of the movementState property.
         * <p>
         * This accessor method returns a reference to the live list, not a snapshot. Therefore any
         * modification you make to the returned list will be present inside the JAXB object. This
         * is why there is not a <CODE>set</CODE> method for the movementState property.
         * <p>
         * For example, to add a new item, do as follows:
         * 
         * <pre>
         * getMovementState().add(newItem);
         * </pre>
         * <p>
         * Objects of the following type(s) are allowed in the list {@link MovementStateExp }
         */
        public List<MovementStateExp> getMovementState() {
            if (movementState == null) {
                movementState = new ArrayList<MovementStateExp>();
            }
            return this.movementState;
        }

    }

}