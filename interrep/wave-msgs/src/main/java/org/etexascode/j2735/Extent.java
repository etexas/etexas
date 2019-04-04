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
 * Java class for Extent.
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name="Extent">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="useInstantlyOnly"/>
 *     &lt;enumeration value="useFor3meters"/>
 *     &lt;enumeration value="useFor10meters"/>
 *     &lt;enumeration value="useFor50meters"/>
 *     &lt;enumeration value="useFor100meters"/>
 *     &lt;enumeration value="useFor500meters"/>
 *     &lt;enumeration value="useFor1000meters"/>
 *     &lt;enumeration value="useFor5000meters"/>
 *     &lt;enumeration value="useFor10000meters"/>
 *     &lt;enumeration value="useFor50000meters"/>
 *     &lt;enumeration value="useFor100000meters"/>
 *     &lt;enumeration value="forever"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 */
@XmlType(name = "Extent")
@XmlEnum
public enum Extent {

    @XmlEnumValue("useInstantlyOnly")
    USE_INSTANTLY_ONLY("useInstantlyOnly"),
    @XmlEnumValue("useFor3meters")
    USE_FOR_3_METERS("useFor3meters"),
    @XmlEnumValue("useFor10meters")
    USE_FOR_10_METERS("useFor10meters"),
    @XmlEnumValue("useFor50meters")
    USE_FOR_50_METERS("useFor50meters"),
    @XmlEnumValue("useFor100meters")
    USE_FOR_100_METERS("useFor100meters"),
    @XmlEnumValue("useFor500meters")
    USE_FOR_500_METERS("useFor500meters"),
    @XmlEnumValue("useFor1000meters")
    USE_FOR_1000_METERS("useFor1000meters"),
    @XmlEnumValue("useFor5000meters")
    USE_FOR_5000_METERS("useFor5000meters"),
    @XmlEnumValue("useFor10000meters")
    USE_FOR_10000_METERS("useFor10000meters"),
    @XmlEnumValue("useFor50000meters")
    USE_FOR_50000_METERS("useFor50000meters"),
    @XmlEnumValue("useFor100000meters")
    USE_FOR_100000_METERS("useFor100000meters"),
    @XmlEnumValue("forever")
    FOREVER("forever");

    private final String value;

    Extent(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static Extent fromValue(String v) {
        for (Extent c : Extent.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
