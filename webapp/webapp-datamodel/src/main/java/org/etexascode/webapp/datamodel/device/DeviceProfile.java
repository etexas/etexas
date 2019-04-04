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
package org.etexascode.webapp.datamodel.device;

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
import org.etexascode.webapp.datamodel.application.Application;
import org.etexascode.webapp.datamodel.application.ApplicationHost;
import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A connected vehicle device profile.
 * 
 * @author bbadillo
 * @author emyers
 */
@Entity
@Table(name = "device_profiles")
@XmlAccessorType(XmlAccessType.FIELD)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "type", discriminatorType = DiscriminatorType.STRING)
@XmlSeeAlso({ CellularDeviceProfile.class, ObuDeviceProfile.class })
public abstract class DeviceProfile extends AbstractEntity implements ApplicationHost, ICopyable<DeviceProfile> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this device profile. */
    private String name;

    /** The device type for this device profile. */
    @Enumerated(EnumType.STRING)
    @Column(name = "type", insertable = false, updatable = false)
    private DeviceType type;

    /** The percentage of affected vehicles for this device profile. */
    private double percentage;

    /** The applications for this device profile. */
    @JoinColumn(name = "device_profile")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private List<Application> applications = new ArrayList<Application>();

    /**
     * Returns the name of this device profile.
     * 
     * @return The string name of this device profile.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this device profile.
     * 
     * @param name The string device profile name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the device type for this device profile.
     * 
     * @return The device type for this device profile.
     */
    public DeviceType getType() {

        return type;
    }

    /**
     * Returns the percentage of affected vehicles for this device profile.
     * 
     * @return The double percentage of affected vehicles for this device profile.
     */
    public double getPercentage() {

        return percentage;
    }

    /**
     * Sets the percentage of affected vehicles for this device profile.
     * 
     * @param percentage The double percentage of affected vehicles to set.
     */
    public void setPercentage(double percentage) {

        this.percentage = percentage;
    }

    @Override
    public List<Application> getApplications() {

        return applications;
    }

    @Override
    public void setApplications(List<Application> applications) {

        this.applications = applications;
    }

    @Override
    public boolean canHost(Application application) {

        return application.getDeviceType() == type;
    }

    @Override
    public final DeviceProfile copy() {

        DeviceProfile deviceProfile = this.specificCopy();

        if (deviceProfile == null) {

            throw new IllegalStateException("The specificCopy method cannot return a null value");
        }

        deviceProfile.setName(this.getName());

        List<Application> applications = new ArrayList<Application>(this.applications.size());

        for (Application application : this.applications) {

            applications.add(application.copy());
        }

        deviceProfile.setApplications(applications);

        return deviceProfile;

    }

    /**
     * Creates a copy of this specific device profile type.
     * 
     * @return The new device profile.
     */
    protected abstract DeviceProfile specificCopy();
}