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

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for ColorState.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name="ColorState">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="dark"/>
 *     &lt;enumeration value="green"/>
 *     &lt;enumeration value="yellow"/>
 *     &lt;enumeration value="red"/>
 *     &lt;enumeration value="green-flashing"/>
 *     &lt;enumeration value="yellow-flashing"/>
 *     &lt;enumeration value="red-flashing"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 */
@XmlType(name = "ColorState")
@XmlEnum
public enum ColorState {

    @XmlEnumValue("dark")
    DARK("dark"),
    @XmlEnumValue("green")
    GREEN("green"),
    @XmlEnumValue("yellow")
    YELLOW("yellow"),
    @XmlEnumValue("red")
    RED("red"),
    @XmlEnumValue("green-flashing")
    GREEN_FLASHING("green-flashing"),
    @XmlEnumValue("yellow-flashing")
    YELLOW_FLASHING("yellow-flashing"),
    @XmlEnumValue("red-flashing")
    RED_FLASHING("red-flashing");

    private final String value;

    ColorState(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static ColorState fromValue(String v) {
        for (ColorState c : ColorState.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
