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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.DiscriminatorType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.etexascode.webapp.datamodel.AbstractEntity;
import org.etexascode.webapp.datamodel.device.DeviceType;
import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A connected vehicle application.
 * 
 * @author bbadillo
 * @author emyers
 */
@Entity
@Table(name = "applications")
@XmlAccessorType(XmlAccessType.FIELD)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "type", discriminatorType = DiscriminatorType.STRING)
@XmlSeeAlso({ JarApplication.class, NativeApplication.class, RemoteApplication.class })
public abstract class Application extends AbstractEntity implements ICopyable<Application> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this application. */
    private String name;

    /** The application type for this application. */
    @Enumerated(EnumType.STRING)
    @Column(name = "type", insertable = false, updatable = false)
    private ApplicationType type;

    /** The device type for this application. */
    @Enumerated(EnumType.STRING)
    @Column(name = "device_type")
    private DeviceType deviceType;

    /** The parameters for this application. */
    @JoinColumn(name = "application")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private List<ApplicationParameter> parameters = new ArrayList<ApplicationParameter>();

    /**
     * Returns the name of this application.
     * 
     * @return The string name of this application.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this application.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the application type for this application.
     * 
     * @return The application type for this application.
     */
    public ApplicationType getType() {

        return type;
    }

    /**
     * Returns the device type for this application.
     * 
     * @return The device type for this application.
     */
    public DeviceType getDeviceType() {

        return deviceType;
    }

    /**
     * Sets the device type for this application.
     * 
     * @param deviceType The device type to set.
     */
    public void setDeviceType(DeviceType deviceType) {

        this.deviceType = deviceType;
    }

    /**
     * Returns the parameters for this application.
     * 
     * @return The list of parameters for this application.
     */
    public List<ApplicationParameter> getParameters() {

        return parameters;
    }

    /**
     * Sets the parameters for this application.
     * 
     * @param parameters The list of parameters to set.
     */
    public void setParameters(List<ApplicationParameter> parameters) {

        this.parameters = parameters;
    }

    @Override
    public Application copy() {

        Application application = copyInternal();
        application.setName(this.getName());
        application.setDeviceType(this.getDeviceType());

        List<ApplicationParameter> parameters = new ArrayList<ApplicationParameter>(this.parameters.size());

        for (ApplicationParameter parameter : this.getParameters()) {
            parameters.add(parameter.copy());
        }

        application.setParameters(parameters);

        return application;
    }

    /**
     * Creates a copy of this specific application type.
     * 
     * @return The new application.
     */
    protected abstract Application copyInternal();

    /**
     * Returns the class used to instantiate this application.
     * 
     * @return The class used to instantiate this application.
     */
    public abstract Class<?> getInstanceClass();

    /**
     * Returns the parameters used to instantiate this application.
     * 
     * @return The string array of parameters used to instantiate this application.
     */
    public abstract String[] getInstanceParameters();
}