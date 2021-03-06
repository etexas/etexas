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
 * Java class for PathHistory complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PathHistory">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="initialPosition" type="{http://www.DSRC-Adopted-02-00-36/DSRC}FullPositionVector" minOccurs="0"/>
 *         &lt;element name="currGPSstatus" type="{http://www.DSRC-Adopted-02-00-36/DSRC}GPSstatus" minOccurs="0"/>
 *         &lt;element name="itemCnt" type="{http://www.DSRC-Adopted-02-00-36/DSRC}Count" minOccurs="0"/>
 *         &lt;element name="crumbData">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;choice>
 *                   &lt;element name="pathHistoryPointSets-01">
 *                     &lt;complexType>
 *                       &lt;complexContent>
 *                         &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                           &lt;sequence maxOccurs="23">
 *                             &lt;element name="PathHistoryPointType-01" type="{http://www.DSRC-Adopted-02-00-36/DSRC}PathHistoryPointType-01"/>
 *                           &lt;/sequence>
 *                         &lt;/restriction>
 *                       &lt;/complexContent>
 *                     &lt;/complexType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-02">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="15"/>
 *                         &lt;maxLength value="345"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-03">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="12"/>
 *                         &lt;maxLength value="276"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-04">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="8"/>
 *                         &lt;maxLength value="184"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-05">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="10"/>
 *                         &lt;maxLength value="230"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-06">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="6"/>
 *                         &lt;maxLength value="138"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-07">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="11"/>
 *                         &lt;maxLength value="242"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-08">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="7"/>
 *                         &lt;maxLength value="161"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-09">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="9"/>
 *                         &lt;maxLength value="196"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                   &lt;element name="pathHistoryPointSets-10">
 *                     &lt;simpleType>
 *                       &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
 *                         &lt;minLength value="5"/>
 *                         &lt;maxLength value="104"/>
 *                       &lt;/restriction>
 *                     &lt;/simpleType>
 *                   &lt;/element>
 *                 &lt;/choice>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;any processContents='lax' namespace='##other' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PathHistory", propOrder = { "initialPosition", "currGPSstatus", "itemCnt", "crumbData", "any" })
public class PathHistory implements Serializable {

    protected FullPositionVector initialPosition;

    @XmlList
    protected List<String> currGPSstatus;

    protected Short itemCnt;

    @XmlElement(required = true)
    protected PathHistory.CrumbData crumbData;

    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the initialPosition property.
     * 
     * @return possible object is {@link FullPositionVector }
     */
    public FullPositionVector getInitialPosition() {
        return initialPosition;
    }

    /**
     * Sets the value of the initialPosition property.
     * 
     * @param value allowed object is {@link FullPositionVector }
     */
    public void setInitialPosition(FullPositionVector value) {
        this.initialPosition = value;
    }

    /**
     * Gets the value of the currGPSstatus property.
     * <p>
     * This accessor method returns a reference to the live list, not a snapshot. Therefore any
     * modification you make to the returned list will be present inside the JAXB object. This is
     * why there is not a <CODE>set</CODE> method for the currGPSstatus property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getCurrGPSstatus().add(newItem);
     * </pre>
     * <p>
     * Objects of the following type(s) are allowed in the list {@link String }
     */
    public List<String> getCurrGPSstatus() {
        if (currGPSstatus == null) {
            currGPSstatus = new ArrayList<String>();
        }
        return this.currGPSstatus;
    }

    /**
     * Gets the value of the itemCnt property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getItemCnt() {
        return itemCnt;
    }

    /**
     * Sets the value of the itemCnt property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setItemCnt(Short value) {
        this.itemCnt = value;
    }

    /**
     * Gets the value of the crumbData property.
     * 
     * @return possible object is {@link PathHistory.CrumbData }
     */
    public PathHistory.CrumbData getCrumbData() {
        return crumbData;
    }

    /**
     * Sets the value of the crumbData property.
     * 
     * @param value allowed object is {@link PathHistory.CrumbData }
     */
    public void setCrumbData(PathHistory.CrumbData value) {
        this.crumbData = value;
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
     *       &lt;choice>
     *         &lt;element name="pathHistoryPointSets-01">
     *           &lt;complexType>
     *             &lt;complexContent>
     *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *                 &lt;sequence maxOccurs="23">
     *                   &lt;element name="PathHistoryPointType-01" type="{http://www.DSRC-Adopted-02-00-36/DSRC}PathHistoryPointType-01"/>
     *                 &lt;/sequence>
     *               &lt;/restriction>
     *             &lt;/complexContent>
     *           &lt;/complexType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-02">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="15"/>
     *               &lt;maxLength value="345"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-03">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="12"/>
     *               &lt;maxLength value="276"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-04">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="8"/>
     *               &lt;maxLength value="184"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-05">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="10"/>
     *               &lt;maxLength value="230"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-06">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="6"/>
     *               &lt;maxLength value="138"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-07">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="11"/>
     *               &lt;maxLength value="242"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-08">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="7"/>
     *               &lt;maxLength value="161"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-09">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="9"/>
     *               &lt;maxLength value="196"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *         &lt;element name="pathHistoryPointSets-10">
     *           &lt;simpleType>
     *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}hexBinary">
     *               &lt;minLength value="5"/>
     *               &lt;maxLength value="104"/>
     *             &lt;/restriction>
     *           &lt;/simpleType>
     *         &lt;/element>
     *       &lt;/choice>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "pathHistoryPointSets01", "pathHistoryPointSets02", "pathHistoryPointSets03", "pathHistoryPointSets04", "pathHistoryPointSets05", "pathHistoryPointSets06",
            "pathHistoryPointSets07", "pathHistoryPointSets08", "pathHistoryPointSets09", "pathHistoryPointSets10" })
    public static class CrumbData implements Serializable {

        @XmlElement(name = "pathHistoryPointSets-01")
        protected PathHistory.CrumbData.PathHistoryPointSets01 pathHistoryPointSets01;

        @XmlElement(name = "pathHistoryPointSets-02", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets02;

        @XmlElement(name = "pathHistoryPointSets-03", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets03;

        @XmlElement(name = "pathHistoryPointSets-04", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets04;

        @XmlElement(name = "pathHistoryPointSets-05", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets05;

        @XmlElement(name = "pathHistoryPointSets-06", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets06;

        @XmlElement(name = "pathHistoryPointSets-07", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets07;

        @XmlElement(name = "pathHistoryPointSets-08", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets08;

        @XmlElement(name = "pathHistoryPointSets-09", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets09;

        @XmlElement(name = "pathHistoryPointSets-10", type = String.class)
        @XmlJavaTypeAdapter(HexBinaryAdapter.class)
        protected byte[] pathHistoryPointSets10;

        /**
         * Gets the value of the pathHistoryPointSets01 property.
         * 
         * @return possible object is {@link PathHistory.CrumbData.PathHistoryPointSets01 }
         */
        public PathHistory.CrumbData.PathHistoryPointSets01 getPathHistoryPointSets01() {
            return pathHistoryPointSets01;
        }

        /**
         * Sets the value of the pathHistoryPointSets01 property.
         * 
         * @param value allowed object is {@link PathHistory.CrumbData.PathHistoryPointSets01 }
         */
        public void setPathHistoryPointSets01(PathHistory.CrumbData.PathHistoryPointSets01 value) {
            this.pathHistoryPointSets01 = value;
        }

        /**
         * Gets the value of the pathHistoryPointSets02 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets02() {
            return pathHistoryPointSets02 == null ? null : pathHistoryPointSets02.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets02 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets02(byte[] value) {
            this.pathHistoryPointSets02 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets03 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets03() {
            return pathHistoryPointSets03 == null ? null : pathHistoryPointSets03.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets03 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets03(byte[] value) {
            this.pathHistoryPointSets03 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets04 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets04() {
            return pathHistoryPointSets04 == null ? null : pathHistoryPointSets04.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets04 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets04(byte[] value) {
            this.pathHistoryPointSets04 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets05 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets05() {
            return pathHistoryPointSets05 == null ? null : pathHistoryPointSets05.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets05 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets05(byte[] value) {
            this.pathHistoryPointSets05 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets06 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets06() {
            return pathHistoryPointSets06 == null ? null : pathHistoryPointSets06.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets06 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets06(byte[] value) {
            this.pathHistoryPointSets06 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets07 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets07() {
            return pathHistoryPointSets07 == null ? null : pathHistoryPointSets07.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets07 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets07(byte[] value) {
            this.pathHistoryPointSets07 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets08 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets08() {
            return pathHistoryPointSets08 == null ? null : pathHistoryPointSets08.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets08 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets08(byte[] value) {
            this.pathHistoryPointSets08 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets09 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets09() {
            return pathHistoryPointSets09 == null ? null : pathHistoryPointSets09.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets09 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets09(byte[] value) {
            this.pathHistoryPointSets09 = ((byte[])value.clone());
        }

        /**
         * Gets the value of the pathHistoryPointSets10 property.
         * 
         * @return possible object is {@link String }
         */
        public byte[] getPathHistoryPointSets10() {
            return pathHistoryPointSets10 == null ? null : pathHistoryPointSets10.clone();
        }

        /**
         * Sets the value of the pathHistoryPointSets10 property.
         * 
         * @param value allowed object is {@link String }
         */
        public void setPathHistoryPointSets10(byte[] value) {
            this.pathHistoryPointSets10 = ((byte[])value.clone());
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
         *       &lt;sequence maxOccurs="23">
         *         &lt;element name="PathHistoryPointType-01" type="{http://www.DSRC-Adopted-02-00-36/DSRC}PathHistoryPointType-01"/>
         *       &lt;/sequence>
         *     &lt;/restriction>
         *   &lt;/complexContent>
         * &lt;/complexType>
         * </pre>
         */
        @XmlAccessorType(XmlAccessType.FIELD)
        @XmlType(name = "", propOrder = { "pathHistoryPointType01" })
        public static class PathHistoryPointSets01 implements Serializable {

            @XmlElement(name = "PathHistoryPointType-01", required = true)
            protected List<PathHistoryPointType01> pathHistoryPointType01;

            /**
             * Gets the value of the pathHistoryPointType01 property.
             * <p>
             * This accessor method returns a reference to the live list, not a snapshot. Therefore
             * any modification you make to the returned list will be present inside the JAXB
             * object. This is why there is not a <CODE>set</CODE> method for the
             * pathHistoryPointType01 property.
             * <p>
             * For example, to add a new item, do as follows:
             * 
             * <pre>
             * getPathHistoryPointType01().add(newItem);
             * </pre>
             * <p>
             * Objects of the following type(s) are allowed in the list
             * {@link PathHistoryPointType01 }
             */
            public List<PathHistoryPointType01> getPathHistoryPointType01() {
                if (pathHistoryPointType01 == null) {
                    pathHistoryPointType01 = new ArrayList<PathHistoryPointType01>();
                }
                return this.pathHistoryPointType01;
            }

        }

    }

}
