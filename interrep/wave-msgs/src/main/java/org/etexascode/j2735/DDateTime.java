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
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for DDateTime complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DDateTime">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="year" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DYear" minOccurs="0"/>
 *         &lt;element name="month" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DMonth" minOccurs="0"/>
 *         &lt;element name="day" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DDay" minOccurs="0"/>
 *         &lt;element name="hour" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DHour" minOccurs="0"/>
 *         &lt;element name="minute" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DMinute" minOccurs="0"/>
 *         &lt;element name="second" type="{http://www.DSRC-Adopted-02-00-36/DSRC}DSecond" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DDateTime", propOrder = { "year", "month", "day", "hour", "minute", "second" })
public class DDateTime implements Serializable {

    protected Integer year;

    protected Short month;

    protected Short day;

    protected Short hour;

    protected Short minute;

    protected Integer second;

    /**
     * Gets the value of the year property.
     * 
     * @return possible object is {@link Integer }
     */
    public Integer getYear() {
        return year;
    }

    /**
     * Sets the value of the year property.
     * 
     * @param value allowed object is {@link Integer }
     */
    public void setYear(Integer value) {
        this.year = value;
    }

    /**
     * Gets the value of the month property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getMonth() {
        return month;
    }

    /**
     * Sets the value of the month property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setMonth(Short value) {
        this.month = value;
    }

    /**
     * Gets the value of the day property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getDay() {
        return day;
    }

    /**
     * Sets the value of the day property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setDay(Short value) {
        this.day = value;
    }

    /**
     * Gets the value of the hour property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getHour() {
        return hour;
    }

    /**
     * Sets the value of the hour property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setHour(Short value) {
        this.hour = value;
    }

    /**
     * Gets the value of the minute property.
     * 
     * @return possible object is {@link Short }
     */
    public Short getMinute() {
        return minute;
    }

    /**
     * Sets the value of the minute property.
     * 
     * @param value allowed object is {@link Short }
     */
    public void setMinute(Short value) {
        this.minute = value;
    }

    /**
     * Gets the value of the second property.
     * 
     * @return possible object is {@link Integer }
     */
    public Integer getSecond() {
        return second;
    }

    /**
     * Sets the value of the second property.
     * 
     * @param value allowed object is {@link Integer }
     */
    public void setSecond(Integer value) {
        this.second = value;
    }

}
