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
 * The SPAT message.
 * 
 * @author jrutherford
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlType(name = "SPATExp", propOrder = { "msgID", "contentVersion", "size", "spatBlob", "CRC", "any" })
public class SPATExp extends MessageExp implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 577715794568511227L;

    /** The message ID. 0x8D for SPAT. */
    @XmlElement(required = true)
    protected byte msgID = (byte)0x8D;

    /** The content version. */
    @XmlElement(required = true)
    protected byte contentVersion;

    /** The payload size. */
    @XmlElement(required = true)
    protected short size;

    /** The payload. */
    @XmlElement(required = true)
    protected IntersectionStateExp spatBlob;

    /** The CRC. */
    @XmlElement
    protected short CRC;

    /** Any object. */
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the msgID property.
     * 
     * @return the message id.
     */
    public byte getMsgID() {
        return msgID;
    }

    /**
     * Sets the value of the msgID property. Sets the message id.
     */
    public void setMsgID(byte value) {
        this.msgID = value;
    }

    /**
     * Gets the content version.
     * 
     * @return The content version.
     */
    public byte getContentVersion() {
        return contentVersion;
    }

    /**
     * Sets the content version.
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

    public IntersectionStateExp getSpatBlob() {
        return spatBlob;
    }

    public void setSpatBlob(IntersectionStateExp spatBlob) {
        this.spatBlob = spatBlob;
    }

    /**
     * Gets the CRC.
     * 
     * @return The CRC.
     */
    public short getCRC() {
        return CRC;
    }

    /**
     * Sets the CRC.
     * 
     * @param cRC The new CRC.
     */
    public void setCRC(short cRC) {
        CRC = cRC;
    }

    /**
     * Gets the value of the any property.
     * 
     * @return The object.
     */
    public Object getAny() {
        return any;
    }

    /**
     * Sets the value of the any property.
     * 
     * @param value The new value of the object.
     */
    public void setAny(Object value) {
        this.any = value;
    }

    /**
     * The payload class.
     * 
     * @author jrutherford
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlRootElement
    @XmlType(name = "", propOrder = { "intersectionState" })
    public static class Intersections implements Serializable {

        /** Serial ID. */
        private static final long serialVersionUID = -4339857229247639787L;

        /** The intersections. */
        @XmlElement(name = "IntersectionState", required = true)
        protected List<IntersectionStateExp> intersectionState;

        /**
         * Gets the value of the intersectionState property.
         * <p>
         * This accessor method returns a reference to the live list, not a snapshot. Therefore any
         * modification you make to the returned list will be present inside the JAXB object. This
         * is why there is not a <CODE>set</CODE> method for the intersectionState property.
         * <p>
         * For example, to add a new item, do as follows:
         * 
         * <pre>
         * getIntersectionState().add(newItem);
         * </pre>
         * <p>
         * Objects of the following type(s) are allowed in the list {@link IntersectionStateExp }
         */
        public List<IntersectionStateExp> getIntersectionState() {
            if (intersectionState == null) {
                intersectionState = new ArrayList<IntersectionStateExp>();
            }
            return this.intersectionState;
        }
    }
}