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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.webapp.datamodel.AbstractEntity;

/**
 * A connected vehicle application parameter profile.
 * 
 * @author jrutherford
 * @author emyers
 */
@Entity
@Table(name = "application_parameter_profiles")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ApplicationParameterProfile extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this parameter profile. */
    private String name;

    /** The description of this parameter profile. */
    private String description;

    /** The default value for this parameter profile. */
    @Column(name = "default_value")
    private String defaultValue;

    /** The display name for this parameter profile. */
    @Column(name = "display_name")
    private String displayName;

    /**
     * Returns the name of this parameter profile.
     * 
     * @return The string name of this parameter profile.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this parameter profile.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the description of this parameter profile.
     * 
     * @return The string description of this parameter profile.
     */
    public String getDescription() {

        return description;
    }

    /**
     * Sets the description of this parameter profile.
     * 
     * @param description The string description to set.
     */
    public void setDescription(String description) {

        this.description = description;
    }

    /**
     * Returns the default value for this parameter profile.
     * 
     * @return The string default value for this parameter profile.
     */
    public String getDefaultValue() {

        return defaultValue;
    }

    /**
     * Sets the default value for this parameter profile.
     * 
     * @param defaultValue The string default value to set.
     */
    public void setDefaultValue(String defaultValue) {

        this.defaultValue = defaultValue;
    }

    /**
     * Returns the display name for this parameter profile.
     * 
     * @return The string display name for this parameter profile.
     */
    public String getDisplayName() {

        return displayName;
    }

    /**
     * Sets the display name for this parameter profile.
     * 
     * @param displayName The string display name to set.
     */
    public void setDisplayName(String displayName) {

        this.displayName = displayName;
    }
}