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

/**
 * A connected vehicle application profile.
 * 
 * @author bbadillo
 * @author emyers
 */
@Entity
@Table(name = "application_profiles")
@XmlAccessorType(XmlAccessType.FIELD)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "type", discriminatorType = DiscriminatorType.STRING)
@XmlSeeAlso({ JarApplicationProfile.class, NativeApplicationProfile.class, RemoteApplicationProfile.class })
public abstract class ApplicationProfile extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this application profile. */
    private String name;

    /** The application type for this application profile. */
    @Enumerated(EnumType.STRING)
    @Column(name = "type", insertable = false, updatable = false)
    private ApplicationType type;

    /** The device type for this application profile. */
    @Enumerated(EnumType.STRING)
    @Column(name = "device_type")
    private DeviceType deviceType;

    /** The embedded status for this application profile. */
    @Column(name = "embedded")
    private boolean isEmbedded;

    /** The parameter profiles for this application profile. */
    @JoinColumn(name = "application_profile")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private List<ApplicationParameterProfile> parameterProfiles = new ArrayList<ApplicationParameterProfile>();

    /**
     * Returns the name of this application profile.
     * 
     * @return The string name of this application profile.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this application profile.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the application type for this application profile.
     * 
     * @return The application type for this application profile.
     */
    public ApplicationType getType() {

        return type;
    }

    /**
     * Returns the device type for this application profile.
     * 
     * @return The device type for this application profile.
     */
    public DeviceType getDeviceType() {

        return deviceType;
    }

    /**
     * Sets the device type for this application profile.
     * 
     * @param deviceType The device type to set.
     */
    public void setDeviceType(DeviceType deviceType) {

        this.deviceType = deviceType;
    }

    /**
     * Returns the embedded status for this application profile.
     * 
     * @return The boolean embedded status for this application profile.
     */
    public boolean isEmbedded() {

        return isEmbedded;
    }

    /**
     * Sets the embedded status for this application profile.
     * 
     * @param isEmbedded The boolean embedded status to set.
     */
    public void setEmbedded(boolean isEmbedded) {

        this.isEmbedded = isEmbedded;
    }

    /**
     * Returns the parameter profiles for this application profile.
     * 
     * @return The list of parameter profiles for this application profile.
     */
    public List<ApplicationParameterProfile> getParameterProfiles() {

        return parameterProfiles;
    }

    /**
     * Sets the parameter profiles for this application profile.
     * 
     * @param parameterProfiles The list of parameter profiles to set.
     */
    public void setParameterProfiles(List<ApplicationParameterProfile> parameterProfiles) {

        this.parameterProfiles = parameterProfiles;
    }

    /**
     * Returns an instance of this application profile.
     * 
     * @return An instance of this application profile.
     */
    public final Application createApplication() {

        Application application = createApplicationInternal();
        application.setName(name);
        application.setDeviceType(deviceType);

        List<ApplicationParameter> parameters = new ArrayList<ApplicationParameter>();

        for (ApplicationParameterProfile parameterProfile : parameterProfiles) {

            ApplicationParameter parameter = new ApplicationParameter();
            parameter.setName(parameterProfile.getName());
            parameter.setValue(parameterProfile.getDefaultValue());
            parameters.add(parameter);
        }

        application.setParameters(parameters);

        return application;
    }

    /**
     * Returns an instance of this application. The method must be implemented by application
     * subclasses to create an application instance of the appropriate type.
     * 
     * @return An instance of this application.
     */
    protected abstract Application createApplicationInternal();
}