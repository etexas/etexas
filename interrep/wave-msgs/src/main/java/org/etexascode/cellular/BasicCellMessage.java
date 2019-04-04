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

package org.etexascode.cellular;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.etexascode.nonstd.Message;

/**
 * Java class for a BasicCellMessage
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlType(name = "BasicCellMessage", propOrder = { "msgID", "message", "location" })
public class BasicCellMessage extends Message implements Serializable {

    @XmlElement(required = true)
    protected String msgID;

    @XmlElement(required = true)
    protected String message;

    @XmlElement(required = true)
    protected String location;

    /**
     * Gets the value of the msgID property.
     * 
     * @return possible object is {@link String }
     */
    public String getMsgID() {
        return msgID;
    }

    /**
     * Sets the value of the msgID property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setMsgID(String value) {
        this.msgID = value;
    }

    /**
     * Gets the value of the msgID property.
     * 
     * @return possible object is {@link String }
     */
    public String getMessage() {
        return message;
    }

    /**
     * Sets the value of the msgID property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setMessage(String value) {
        this.message = value;
    }

    /**
     * Gets the value of the msgID property.
     * 
     * @return possible object is {@link String }
     */
    public String getLocation() {
        return location;
    }

    /**
     * Sets the value of the msgID property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setLocation(String value) {
        this.location = value;
    }
}
