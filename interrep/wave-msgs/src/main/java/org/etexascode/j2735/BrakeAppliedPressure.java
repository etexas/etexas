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
 * Java class for BrakeAppliedPressure.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name="BrakeAppliedPressure">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="unavailable"/>
 *     &lt;enumeration value="minPressure"/>
 *     &lt;enumeration value="bkLvl-2"/>
 *     &lt;enumeration value="bkLvl-3"/>
 *     &lt;enumeration value="bkLvl-4"/>
 *     &lt;enumeration value="bkLvl-5"/>
 *     &lt;enumeration value="bkLvl-6"/>
 *     &lt;enumeration value="bkLvl-7"/>
 *     &lt;enumeration value="bkLvl-8"/>
 *     &lt;enumeration value="bkLvl-9"/>
 *     &lt;enumeration value="bkLvl-10"/>
 *     &lt;enumeration value="bkLvl-11"/>
 *     &lt;enumeration value="bkLvl-12"/>
 *     &lt;enumeration value="bkLvl-13"/>
 *     &lt;enumeration value="bkLvl-14"/>
 *     &lt;enumeration value="maxPressure"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 */
@XmlType(name = "BrakeAppliedPressure")
@XmlEnum
public enum BrakeAppliedPressure {

    @XmlEnumValue("unavailable")
    UNAVAILABLE("unavailable"),
    @XmlEnumValue("minPressure")
    MIN_PRESSURE("minPressure"),
    @XmlEnumValue("bkLvl-2")
    BK_LVL_2("bkLvl-2"),
    @XmlEnumValue("bkLvl-3")
    BK_LVL_3("bkLvl-3"),
    @XmlEnumValue("bkLvl-4")
    BK_LVL_4("bkLvl-4"),
    @XmlEnumValue("bkLvl-5")
    BK_LVL_5("bkLvl-5"),
    @XmlEnumValue("bkLvl-6")
    BK_LVL_6("bkLvl-6"),
    @XmlEnumValue("bkLvl-7")
    BK_LVL_7("bkLvl-7"),
    @XmlEnumValue("bkLvl-8")
    BK_LVL_8("bkLvl-8"),
    @XmlEnumValue("bkLvl-9")
    BK_LVL_9("bkLvl-9"),
    @XmlEnumValue("bkLvl-10")
    BK_LVL_10("bkLvl-10"),
    @XmlEnumValue("bkLvl-11")
    BK_LVL_11("bkLvl-11"),
    @XmlEnumValue("bkLvl-12")
    BK_LVL_12("bkLvl-12"),
    @XmlEnumValue("bkLvl-13")
    BK_LVL_13("bkLvl-13"),
    @XmlEnumValue("bkLvl-14")
    BK_LVL_14("bkLvl-14"),
    @XmlEnumValue("maxPressure")
    MAX_PRESSURE("maxPressure");

    private final String value;

    BrakeAppliedPressure(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static BrakeAppliedPressure fromValue(String v) {
        for (BrakeAppliedPressure c : BrakeAppliedPressure.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
