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
//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.2-7 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2010.06.02 at 03:16:39 PM EDT 
//

package org.etexascode.j2735;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.w3c.dom.Element;

/**
 * <p>
 * Java class for EmergencyVehicleAlert complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="EmergencyVehicleAlert">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="msgID" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DSRCmsgID"/>
 *         &lt;element name="id" type="{http://www.DSRC-Adopted-02-00-36/DSRC}TemporaryID" minOccurs="0"/>
 *         &lt;element name="rsaMsg" type="{http://www.DSRC-Adopted-02-00-36/DSRC}RoadSideAlert"/>
 *         &lt;element name="responseType" type="{http://www.DSRC-Adopted-02-00-36/DSRC}ResponseType" minOccurs="0"/>
 *         &lt;element name="details" type="{http://www.DSRC-Adopted-02-00-36/DSRC}EmergencyDetails" minOccurs="0"/>
 *         &lt;element name="mass" type="{http://www.DSRC-Adopted-02-00-36/DSRC}VehicleMass" minOccurs="0"/>
 *         &lt;element name="basicType" type="{http://www.DSRC-Adopted-02-00-36/DSRC}VehicleType" minOccurs="0"/>
 *         &lt;element name="vehicleType" type="{http://www.DSRC-Adopted-02-00-36/ITIS}VehicleGroupAffected" minOccurs="0"/>
 *         &lt;element name="responseEquip" type="{http://www.DSRC-Adopted-02-00-36/ITIS}IncidentResponseEquipment" minOccurs="0"/>
 *         &lt;element name="responderType" type="{http://www.DSRC-Adopted-02-00-36/ITIS}ResponderGroupAffected" minOccurs="0"/>
 *         &lt;element name="crc" type="{http://www.DSRC-Adopted-02-00-36/DSRC}MsgCRC"/>
 *         &lt;any processContents='lax' namespace='##other' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "EmergencyVehicleAlert", propOrder = { "msgID", "id", "rsaMsg", "responseType", "details", "mass", "basicType", "vehicleType", "responseEquip", "responderType", "crc", "any" })
public class EmergencyVehicleAlert implements Serializable {

    @XmlElement(required = true)
    protected String msgID;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] id;

    @XmlElement(required = true)
    protected RoadSideAlert rsaMsg;

    protected ResponseType responseType;

    protected Short details;

    protected Short mass;

    protected String basicType;

    protected String vehicleType;

    protected String responseEquip;

    protected String responderType;

    @XmlElement(required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] crc;

    @XmlAnyElement(lax = true)
    protected Object any;

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
     * Gets the value of the id property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getId() {
        return id.clone();
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setId(byte[] value) {
        this.id = ((byte[])value.clone());
    }

    /**
     * Gets the value of the rsaMsg property.
     * 
     * @return possible object is {@link RoadSideAlert }
     */
    public RoadSideAlert getRsaMsg() {
        return rsaMsg;
    }

    /**
     * Sets the value of the rsaMsg property.
     * 
     * @param value allowed object is {@link RoadSideAlert }
     */
    public void setRsaMsg(RoadSideAlert value) {
        this.rsaMsg = value;
    }

    /**
     * Gets the value of the responseType property.
     * 
     * @return possible object is {@link ResponseType }
     */
    public ResponseType getResponseType() {
        return responseType;
    }

    /**
     * Sets the value of the responseType property.
     * 
     * @param value allowed object is {@link ResponseType }
     */
    public void setResponseType(ResponseType value) {
        this.responseType = value;
    }

    /**
     * Gets the value of the details property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getDetails() {
        return details;
    }

    /**
     * Sets the value of the details property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setDetails(Short value) {
        this.details = value;
    }

    /**
     * Gets the value of the mass property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getMass() {
        return mass;
    }

    /**
     * Sets the value of the mass property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setMass(Short value) {
        this.mass = value;
    }

    /**
     * Gets the value of the basicType property.
     * 
     * @return possible object is {@link String }
     */
    public String getBasicType() {
        return basicType;
    }

    /**
     * Sets the value of the basicType property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setBasicType(String value) {
        this.basicType = value;
    }

    /**
     * Gets the value of the vehicleType property.
     * 
     * @return possible object is {@link String }
     */
    public String getVehicleType() {
        return vehicleType;
    }

    /**
     * Sets the value of the vehicleType property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setVehicleType(String value) {
        this.vehicleType = value;
    }

    /**
     * Gets the value of the responseEquip property.
     * 
     * @return possible object is {@link String }
     */
    public String getResponseEquip() {
        return responseEquip;
    }

    /**
     * Sets the value of the responseEquip property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setResponseEquip(String value) {
        this.responseEquip = value;
    }

    /**
     * Gets the value of the responderType property.
     * 
     * @return possible object is {@link String }
     */
    public String getResponderType() {
        return responderType;
    }

    /**
     * Sets the value of the responderType property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setResponderType(String value) {
        this.responderType = value;
    }

    /**
     * Gets the value of the crc property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getCrc() {
        return crc.clone();
    }

    /**
     * Sets the value of the crc property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setCrc(byte[] value) {
        this.crc = ((byte[])value.clone());
    }

    /**
     * Gets the value of the any property.
     * 
     * @return possible object is {@link Element } {@link Object }
     */
    public Object getAny() {
        return any;
    }

    /**
     * Sets the value of the any property.
     * 
     * @param value allowed object is {@link Element } {@link Object }
     */
    public void setAny(Object value) {
        this.any = value;
    }

}
