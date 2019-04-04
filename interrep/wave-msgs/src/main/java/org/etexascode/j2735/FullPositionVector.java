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
 * Java class for FullPositionVector complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FullPositionVector">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="utcTime" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DDateTime" minOccurs="0"/>
 *         &lt;element name="long" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Longitude"/>
 *         &lt;element name="lat" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Latitude"/>
 *         &lt;element name="elevation" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Elevation" minOccurs="0"/>
 *         &lt;element name="heading" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Heading" minOccurs="0"/>
 *         &lt;element name="speed" type="{http://www.DSRC-Adopted-02-00-36/DSRC}TransmissionAndSpeed" minOccurs="0"/>
 *         &lt;element name="posAccuracy" type="{http://www.DSRC-Adopted-02-00-36/DSRC}PositionalAccuracy" minOccurs="0"/>
 *         &lt;element name="timeConfidence" type="{http://www.DSRC-Adopted-02-00-36/DSRC}TimeConfidence" minOccurs="0"/>
 *         &lt;element name="posConfidence" type="{http://www.DSRC-Adopted-02-00-36/DSRC}PositionConfidenceSet" minOccurs="0"/>
 *         &lt;element name="speedConfidence" type="{http://www.DSRC-Adopted-02-00-36/DSRC}SpeedandHeadingandThrottleConfidence" minOccurs="0"/>
 *         &lt;any processContents='lax' namespace='##other' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FullPositionVector", propOrder = { "utcTime", "_long", "lat", "elevation", "heading", "speed", "posAccuracy", "timeConfidence", "posConfidence", "speedConfidence", "any" })
public class FullPositionVector implements Serializable {

    protected DDateTime utcTime;

    @XmlElement(name = "long")
    protected int _long;

    protected int lat;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] elevation;

    protected Integer heading;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] speed;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] posAccuracy;

    protected TimeConfidence timeConfidence;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] posConfidence;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] speedConfidence;

    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the utcTime property.
     * 
     * @return possible object is {@link DDateTime }
     */
    public DDateTime getUtcTime() {
        return utcTime;
    }

    /**
     * Sets the value of the utcTime property.
     * 
     * @param value allowed object is {@link DDateTime }
     */
    public void setUtcTime(DDateTime value) {
        this.utcTime = value;
    }

    /**
     * Gets the value of the long property.
     */
    public int getLong() {
        return _long;
    }

    /**
     * Sets the value of the long property.
     */
    public void setLong(int value) {
        this._long = value;
    }

    /**
     * Gets the value of the lat property.
     */
    public int getLat() {
        return lat;
    }

    /**
     * Sets the value of the lat property.
     */
    public void setLat(int value) {
        this.lat = value;
    }

    /**
     * Gets the value of the elevation property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getElevation() {
        return elevation.clone();
    }

    /**
     * Sets the value of the elevation property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setElevation(byte[] value) {
        this.elevation = ((byte[])value.clone());
    }

    /**
     * Gets the value of the heading property.
     * 
     * @return possible object is {@link Integer }
     */
    public Integer getHeading() {
        return heading;
    }

    /**
     * Sets the value of the heading property.
     * 
     * @param value allowed object is {@link Integer }
     */
    public void setHeading(Integer value) {
        this.heading = value;
    }

    /**
     * Gets the value of the speed property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getSpeed() {
        return speed.clone();
    }

    /**
     * Sets the value of the speed property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setSpeed(byte[] value) {
        this.speed = ((byte[])value.clone());
    }

    /**
     * Gets the value of the posAccuracy property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getPosAccuracy() {
        return posAccuracy.clone();
    }

    /**
     * Sets the value of the posAccuracy property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPosAccuracy(byte[] value) {
        this.posAccuracy = ((byte[])value.clone());
    }

    /**
     * Gets the value of the timeConfidence property.
     * 
     * @return possible object is {@link TimeConfidence }
     */
    public TimeConfidence getTimeConfidence() {
        return timeConfidence;
    }

    /**
     * Sets the value of the timeConfidence property.
     * 
     * @param value allowed object is {@link TimeConfidence }
     */
    public void setTimeConfidence(TimeConfidence value) {
        this.timeConfidence = value;
    }

    /**
     * Gets the value of the posConfidence property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getPosConfidence() {
        return posConfidence.clone();
    }

    /**
     * Sets the value of the posConfidence property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPosConfidence(byte[] value) {
        this.posConfidence = ((byte[])value.clone());
    }

    /**
     * Gets the value of the speedConfidence property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getSpeedConfidence() {
        return speedConfidence.clone();
    }

    /**
     * Sets the value of the speedConfidence property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setSpeedConfidence(byte[] value) {
        this.speedConfidence = ((byte[])value.clone());
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
