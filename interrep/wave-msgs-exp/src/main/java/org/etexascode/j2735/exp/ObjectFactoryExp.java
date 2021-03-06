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

package org.etexascode.j2735.exp;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * This object contains factory methods for each Java content interface and Java element interface
 * generated in the com.harmonia.j2735 package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the Java representation
 * for XML content. The Java representation of XML content can consist of schema derived interfaces
 * and classes representing the binding of schema type definitions, element declarations and model
 * groups. Factory methods for each of these are provided in this class.
 */
@XmlRegistry
public class ObjectFactoryExp {

    /**
     * Create an instance of {@link IntersectionExp }
     */
    public IntersectionExp createIntersection() {
        return new IntersectionExp();
    }

    /**
     * Create an instance of {@link IntersectionStateExp }
     */
    public IntersectionStateExp createIntersectionStateExp() {
        return new IntersectionStateExp();
    }

    /**
     * Create an instance of {@link SPATExp }
     */
    public SPATExp createSPATExp() {
        return new SPATExp();
    }

    /**
     * Create an instance of {@link MovementStateExp }
     */
    public MovementStateExp createMovementState() {
        return new MovementStateExp();
    }

    /**
     * Create an instance of {@link ApproachObjectExp }
     */
    public ApproachObjectExp createApproachObject() {
        return new ApproachObjectExp();
    }
}
