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
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.etexascode.nonstd.Message;
import org.w3c.dom.Element;

/**
 * <p>
 * Java class for MapData complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MapData">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="msgID" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DSRCmsgID"/>
 *         &lt;element name="msgCnt" type="{http://www.DSRC-Adopted-02-00-36/DSRC}MsgCount"/>
 *         &lt;element name="name" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DescriptiveName" minOccurs="0"/>
 *         &lt;element name="layerType" type="{http://www.DSRC-Adopted-02-00-36/DSRC}LayerType" minOccurs="0"/>
 *         &lt;element name="layerID" type="{http://www.DSRC-Adopted-02-00-36/DSRC}LayerID" minOccurs="0"/>
 *         &lt;element name="intersections" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence maxOccurs="32">
 *                   &lt;element name="Intersection" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Intersection"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="dataParameters" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DataParameters" minOccurs="0"/>
 *         &lt;element name="crc" type="{http://www.DSRC-Adopted-02-00-36/DSRC}MsgCRC"/>
 *         &lt;any processContents='lax' namespace='##other' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlType(name = "MapData", propOrder = { "msgID", "msgCnt", "name", "layerType", "layerID", "intersections", "dataParameters", "crc", "any" })
public class MapData extends Message implements Serializable {

    @XmlElement(required = true)
    protected String msgID;

    protected short msgCnt;

    protected String name;

    protected String layerType;

    protected Short layerID;

    protected MapData.Intersections intersections;

    protected DataParameters dataParameters;

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
     * Gets the value of the msgCnt property.
     */
    public short getMsgCnt() {
        return msgCnt;
    }

    /**
     * Sets the value of the msgCnt property.
     */
    public void setMsgCnt(short value) {
        this.msgCnt = value;
    }

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
     * Gets the value of the layerType property.
     * 
     * @return possible object is {@link String }
     */
    public String getLayerType() {
        return layerType;
    }

    /**
     * Sets the value of the layerType property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setLayerType(String value) {
        this.layerType = value;
    }

    /**
     * Gets the value of the layerID property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getLayerID() {
        return layerID;
    }

    /**
     * Sets the value of the layerID property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setLayerID(Short value) {
        this.layerID = value;
    }

    /**
     * Gets the value of the intersections property.
     * 
     * @return possible object is {@link MapData.Intersections }
     */
    public MapData.Intersections getIntersections() {
        return intersections;
    }

    /**
     * Sets the value of the intersections property.
     * 
     * @param value allowed object is {@link MapData.Intersections }
     */
    public void setIntersections(MapData.Intersections value) {
        this.intersections = value;
    }

    /**
     * Gets the value of the dataParameters property.
     * 
     * @return possible object is {@link DataParameters }
     */
    public DataParameters getDataParameters() {
        return dataParameters;
    }

    /**
     * Sets the value of the dataParameters property.
     * 
     * @param value allowed object is {@link DataParameters }
     */
    public void setDataParameters(DataParameters value) {
        this.dataParameters = value;
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
        this.crc = value == null ? null : ((byte[])value.clone());
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
     *       &lt;sequence maxOccurs="32">
     *         &lt;element name="Intersection" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Intersection"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "intersection" })
    public static class Intersections implements Serializable {

        @XmlElement(name = "Intersection", required = true)
        protected List<Intersection> intersection;

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
         * Objects of the following type(s) are allowed in the list {@link Intersection }
         */
        public List<Intersection> getIntersection() {
            if (intersection == null) {
                intersection = new ArrayList<Intersection>();
            }
            return this.intersection;
        }

    }

}