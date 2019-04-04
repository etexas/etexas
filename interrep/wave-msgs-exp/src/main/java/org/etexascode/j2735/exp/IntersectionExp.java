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
 * The intersection object for GDI messages.
 * 
 * @author jrutherford
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "IntersectionExp", propOrder = { "attributes", "intersectionId", "referencePoint", "approaches", "endOfMessage" })
public class IntersectionExp implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = -8590100084756666084L;

    // GDI Data Type Constants
    public static final byte GID_MESSAGE_ATTRIBUTES = (byte)0x01;

    public static final byte GID_INTERSECTION_ID = (byte)0x02;

    public static final byte GID_REFERENCE_POINT = (byte)0x03;

    public static final byte GID_END_OF_MESSAGE = (byte)0xFF;

    /** The Attributes. */
    @XmlElement(required = true)
    protected MessageObjectExp attributes = new MessageObjectExp(GID_MESSAGE_ATTRIBUTES, (byte)0x01);

    /** The intersection ID. */
    @XmlElement(required = true)
    protected MessageObjectExp intersectionId = new MessageObjectExp(GID_INTERSECTION_ID, (byte)0x04);

    /**
     * Reference point. Can be 8 or 10 bytes. First 4: Latitude Second 4: Longitude Last 2
     * (Optional): Elevation
     */
    @XmlElement(required = true)
    protected MessageObjectExp referencePoint = new MessageObjectExp(GID_REFERENCE_POINT, (byte)0x00);

    @XmlElement(required = true)
    protected IntersectionExp.Approaches approaches = new IntersectionExp.Approaches();

    /** End of Message Data. */
    @XmlElement(required = true)
    protected byte endOfMessage = GID_END_OF_MESSAGE;

    /**
     * Gets the attributes.
     * 
     * @return The attributes.
     */
    public MessageObjectExp getAttributes() {
        return attributes;
    }

    /**
     * Gets the intersection ID.
     * 
     * @return The intersection ID.
     */
    public MessageObjectExp getIntersectionId() {
        return intersectionId;
    }

    /**
     * Gets the reference point.
     * 
     * @return The reference point.
     */
    public MessageObjectExp getReferencePoint() {
        return referencePoint;
    }

    /**
     * Gets the approaches.
     * 
     * @return The approaches.
     */
    public IntersectionExp.Approaches getApproaches() {
        return approaches;
    }

    /**
     * The end of message.
     * 
     * @return The end of message.
     */
    public byte getEndOfMessage() {
        return endOfMessage;
    }

    /**
     * Holds the list of approaches.
     * 
     * @author jrutherford
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "approachObject" })
    public static class Approaches implements Serializable {

        /** Serial ID. */
        private static final long serialVersionUID = -6169281673833183629L;

        /** The list. */
        @XmlElement(name = "ApproachObject", required = true)
        protected List<ApproachObjectExp> approachObject;

        /**
         * Gets the value of the approachObject property.
         * <p>
         * This accessor method returns a reference to the live list, not a snapshot. Therefore any
         * modification you make to the returned list will be present inside the JAXB object. This
         * is why there is not a <CODE>set</CODE> method for the approachObject property.
         * <p>
         * For example, to add a new item, do as follows:
         * 
         * <pre>
         * getApproachObject().add(newItem);
         * </pre>
         * <p>
         * Objects of the following type(s) are allowed in the list {@link ApproachObjectExp }
         */
        public List<ApproachObjectExp> getApproachObject() {
            if (approachObject == null) {
                approachObject = new ArrayList<ApproachObjectExp>();
            }
            return this.approachObject;
        }
    }
}