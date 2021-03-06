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
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.w3c.dom.Element;

/**
 * <p>
 * Java class for IntersectionState complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="IntersectionState">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DescriptiveName" minOccurs="0"/>
 *         &lt;element name="id" type="{http://www.DSRC-Adopted-02-00-36/DSRC}IntersectionID"/>
 *         &lt;element name="status" type="{http://www.DSRC-Adopted-02-00-36/DSRC}IntersectionStatusObject"/>
 *         &lt;element name="timeStamp" type="{http://www.DSRC-Adopted-02-00-36/DSRC}TimeMark" minOccurs="0"/>
 *         &lt;element name="lanesCnt" minOccurs="0">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedByte">
 *               &lt;minInclusive value="1"/>
 *               &lt;maxInclusive value="255"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="states">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence maxOccurs="255">
 *                   &lt;element name="MovementState" type="{http://www.DSRC-Adopted-02-00-36/DSRC}MovementState"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="priority" type="{http://www.DSRC-Adopted-02-00-36/DSRC}SignalState" minOccurs="0"/>
 *         &lt;element name="preempt" type="{http://www.DSRC-Adopted-02-00-36/DSRC}SignalState" minOccurs="0"/>
 *         &lt;any processContents='lax' namespace='##other' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "IntersectionState", propOrder = { "name", "id", "status", "timeStamp", "lanesCnt", "states", "priority", "preempt", "any" })
public class IntersectionState implements Serializable {

    protected String name;

    @XmlElement(required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] id;

    @XmlElement(required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] status;

    protected Integer timeStamp;

    protected Short lanesCnt;

    @XmlElement(required = true)
    protected IntersectionState.States states;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] priority;

    @XmlElement(type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] preempt;

    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the name property.
     * 
     * @return possible object is {@link String }
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the id property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getId() {
        if (id == null) {
            return null;
        }
        else {
            return id.clone();
        }

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
     * Gets the value of the status property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getStatus() {
        return status == null ? null : status.clone();
    }

    /**
     * Sets the value of the status property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setStatus(byte[] value) {
        this.status = ((byte[])value.clone());
    }

    /**
     * Gets the value of the timeStamp property.
     * 
     * @return possible object is {@link Integer }
     */
    public Integer getTimeStamp() {
        return timeStamp;
    }

    /**
     * Sets the value of the timeStamp property.
     * 
     * @param value allowed object is {@link Integer }
     */
    public void setTimeStamp(Integer value) {
        this.timeStamp = value;
    }

    /**
     * Gets the value of the lanesCnt property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getLanesCnt() {
        return lanesCnt;
    }

    /**
     * Sets the value of the lanesCnt property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setLanesCnt(Short value) {
        this.lanesCnt = value;
    }

    /**
     * Gets the value of the states property.
     * 
     * @return possible object is {@link IntersectionState.States }
     */
    public IntersectionState.States getStates() {
        return states;
    }

    /**
     * Sets the value of the states property.
     * 
     * @param value allowed object is {@link IntersectionState.States }
     */
    public void setStates(IntersectionState.States value) {
        this.states = value;
    }

    /**
     * Gets the value of the priority property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getPriority() {
        return priority == null ? null : priority.clone();
    }

    /**
     * Sets the value of the priority property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPriority(byte[] value) {
        this.priority = ((byte[])value.clone());
    }

    /**
     * Gets the value of the preempt property.
     * 
     * @return possible object is {@link String }
     */
    public byte[] getPreempt() {
        return preempt == null ? null : preempt.clone();
    }

    /**
     * Sets the value of the preempt property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPreempt(byte[] value) {
        this.preempt = ((byte[])value.clone());
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

    /**
     * <p>
     * Java class for anonymous complex type.
     * <p>
     * The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;sequence maxOccurs="255">
     *         &lt;element name="MovementState" type="{http://www.DSRC-Adopted-02-00-36/DSRC}MovementState"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "movementState" })
    public static class States implements Serializable {

        @XmlElement(name = "MovementState", required = true)
        protected List<MovementState> movementState;

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
         * Objects of the following type(s) are allowed in the list {@link MovementState }
         */
        public List<MovementState> getMovementState() {
            if (movementState == null) {
                movementState = new ArrayList<MovementState>();
            }
            return this.movementState;
        }

    }

}
