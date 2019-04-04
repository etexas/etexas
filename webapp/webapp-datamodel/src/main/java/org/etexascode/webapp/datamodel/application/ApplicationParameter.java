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
package org.etexascode.webapp.datamodel.application;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.webapp.datamodel.AbstractEntity;
import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A connected vehicle application parameter.
 * 
 * @author jrutherford
 * @author emyers
 */
@Entity
@Table(name = "application_parameters")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ApplicationParameter extends AbstractEntity implements ICopyable<ApplicationParameter> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this parameter. */
    private String name;

    /** The value for this parameter. */
    private String value;

    /**
     * Returns the name of this parameter.
     * 
     * @return The string name of this parameter.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this parameter.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the value for this parameter.
     * 
     * @return The string value for this parameter.
     */
    public String getValue() {

        return value;
    }

    /**
     * Sets the value for this parameter.
     * 
     * @param value The string value to set.
     */
    public void setValue(String value) {

        this.value = value;
    }

    @Override
    public ApplicationParameter copy() {

        ApplicationParameter parameter = new ApplicationParameter();
        parameter.setName(this.getName());
        parameter.setValue(this.getValue());

        return parameter;
    }
}