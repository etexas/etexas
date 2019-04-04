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
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.w3c.dom.Element;

/**
 * <p>
 * Java class for CrosswalkLane complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CrosswalkLane">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="laneNumber" type="{http://www.DSRC-Adopted-02-00-36/DSRC}LaneNumber"/>
 *         &lt;element name="laneWidth" type="{http://www.DSRC-Adopted-02-00-36/DSRC}LaneWidth" minOccurs="0"/>
 *         &lt;element name="laneAttributes" type="{http://www.DSRC-Adopted-02-00-36/DSRC}CrosswalkLaneAttributes"/>
 *         &lt;element name="nodeList" type="{http://www.DSRC-Adopted-02-00-36/DSRC}NodeList"/>
 *         &lt;element name="keepOutList" type="{http://www.DSRC-Adopted-02-00-36/DSRC}NodeList" minOccurs="0"/>
 *         &lt;element name="connectsTo" type="{http://www.DSRC-Adopted-02-00-36/DSRC}ConnectsTo" minOccurs="0"/>
 *         &lt;any processContents='lax' namespace='##other' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CrosswalkLane", propOrder = { "laneNumber", "laneWidth", "laneAttributes", "nodeList", "keepOutList", "connectsTo", "any" })
public class CrosswalkLane implements Serializable {

    @XmlElement(required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] laneNumber;

    protected Integer laneWidth;

    @XmlElement(required = true)
    protected CrosswalkLaneAttributes laneAttributes;

    @XmlList
    @XmlElement(required = true)
    protected List<String> nodeList;

    @XmlList
    protected List<String> keepOutList;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] connectsTo;

    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the laneNumber property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getLaneNumber() {
        return laneNumber.clone();
    }

    /**
     * Sets the value of the laneNumber property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setLaneNumber(byte[] value) {
        this.laneNumber = ((byte[])value.clone());
    }

    /**
     * Gets the value of the laneWidth property.
     * 
     * @return possible object is {@link Integer }
     */
    public Integer getLaneWidth() {
        return laneWidth;
    }

    /**
     * Sets the value of the laneWidth property.
     * 
     * @param value allowed object is {@link Integer }
     */
    public void setLaneWidth(Integer value) {
        this.laneWidth = value;
    }

    /**
     * Gets the value of the laneAttributes property.
     * 
     * @return possible object is {@link CrosswalkLaneAttributes }
     */
    public CrosswalkLaneAttributes getLaneAttributes() {
        return laneAttributes;
    }

    /**
     * Sets the value of the laneAttributes property.
     * 
     * @param value allowed object is {@link CrosswalkLaneAttributes }
     */
    public void setLaneAttributes(CrosswalkLaneAttributes value) {
        this.laneAttributes = value;
    }

    /**
     * Gets the value of the nodeList property.
     * <p>
     * This accessor method returns a reference to the live list, not a snapshot. Therefore any
     * modification you make to the returned list will be present inside the JAXB object. This is
     * why there is not a <CODE>set</CODE> method for the nodeList property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getNodeList().add(newItem);
     * </pre>
     * <p>
     * Objects of the following type(s) are allowed in the list {@link String }
     */
    public List<String> getNodeList() {
        if (nodeList == null) {
            nodeList = new ArrayList<String>();
        }
        return this.nodeList;
    }

    /**
     * Gets the value of the keepOutList property.
     * <p>
     * This accessor method returns a reference to the live list, not a snapshot. Therefore any
     * modification you make to the returned list will be present inside the JAXB object. This is
     * why there is not a <CODE>set</CODE> method for the keepOutList property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getKeepOutList().add(newItem);
     * </pre>
     * <p>
     * Objects of the following type(s) are allowed in the list {@link String }
     */
    public List<String> getKeepOutList() {
        if (keepOutList == null) {
            keepOutList = new ArrayList<String>();
        }
        return this.keepOutList;
    }

    /**
     * Gets the value of the connectsTo property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getConnectsTo() {
        return connectsTo.clone();
    }

    /**
     * Sets the value of the connectsTo property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setConnectsTo(byte[] value) {
        this.connectsTo = ((byte[])value.clone());
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
