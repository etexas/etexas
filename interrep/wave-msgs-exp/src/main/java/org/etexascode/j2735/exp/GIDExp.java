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
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.etexascode.nonstd.MessageExp;

/**
 * The GID message object.
 * 
 * @author jrutherford
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlType(name = "GID", propOrder = { "msgID", "contentVersion", "size", "intersections", "crc", "any" })
public class GIDExp extends MessageExp implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 9145427718308418936L;

    /** The message ID. 0x87 for GID. */
    @XmlElement(required = true)
    protected byte msgID = (byte)0x87;

    /** The content version . */
    @XmlElement(required = true)
    protected byte contentVersion;

    /** The size of the payload. */
    @XmlElement(required = true)
    protected short size;

    /** The payload. */
    @XmlElement(required = true)
    protected GIDExp.Intersections intersections = new GIDExp.Intersections();

    // Note: ablatt - I'm leaving Intersections as a list because (in theory according to the
    // experimental spec)
    // it is possible to have more than 1 intersection. We are presently ignoring this possibility
    // in the producer.

    /** The CRC (2 bytes). */
    @XmlElement(required = true)
    protected short crc;

    /** The any element. */
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the msgID property.
     * 
     * @return The message id.
     */
    public byte getMsgID() {
        return msgID;
    }

    /**
     * Sets the value of the msgID property.
     * 
     * @param value The value.
     */
    public void setMsgID(byte value) {
        this.msgID = value;
    }

    /**
     * Get the content version.
     * 
     * @return The content version.
     */
    public byte getContentVersion() {
        return contentVersion;
    }

    /**
     * Set the content version.
     * 
     * @param contentVersion The new content version.
     */
    public void setContentVersion(byte contentVersion) {
        this.contentVersion = contentVersion;
    }

    /**
     * Gets the size.
     * 
     * @return The size.
     */
    public short getSize() {
        return size;
    }

    /**
     * Sets the size.
     * 
     * @param size The new size.
     */
    public void setSize(short size) {
        this.size = size;
    }

    /**
     * Gets the value of the intersections property.
     * 
     * @return The intersection list.
     */
    public GIDExp.Intersections getIntersections() {
        return intersections;
    }

    /**
     * Sets the value of the intersections property.
     * 
     * @param value The intersection list.
     */
    public void setIntersections(GIDExp.Intersections value) {
        this.intersections = value;
    }

    /**
     * Gets the value of the crc property.
     * 
     * @return The CRC.
     */
    public short getCrc() {
        return crc;
    }

    /**
     * Sets the value of the crc property.
     * 
     * @param value The new CRC.
     */
    public void setCrc(short value) {
        this.crc = value;
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
     * @param value Sets the new any object.
     */
    public void setAny(Object value) {
        this.any = value;
    }

    /**
     * The list of intersection objects.
     * 
     * @author jrutherford
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlRootElement
    @XmlType(name = "", propOrder = { "intersection" })
    public static class Intersections implements Serializable {

        /** Serial ID. */
        private static final long serialVersionUID = -1679822061362915613L;

        /** The list of intersections. */
        @XmlElement(name = "Intersection", required = true)
        protected List<IntersectionExp> intersection;

        /**
         * Gets the value of the intersection property.
         * <p>
         * This accessor method returns a reference to the live list, not a snapshot. Therefore any
         * modification you make to the returned list will be present inside the JAXB object. This
         * is why there is not a <CODE>set</CODE> method for the intersection property.
         * <p>
         * For example, to add a new item, do as follows:
         * 
         * <pre>
         * getIntersection().add(newItem);
         * </pre>
         * <p>
         * Objects of the following type(s) are allowed in the list {@link IntersectionExp }
         */
        public List<IntersectionExp> getIntersection() {
            if (intersection == null) {
                intersection = new ArrayList<IntersectionExp>();
            }
            return this.intersection;
        }
    }
}