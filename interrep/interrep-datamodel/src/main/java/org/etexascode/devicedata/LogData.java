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
package org.etexascode.devicedata;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;

/**
 * The data involved in a single app log
 * 
 * @author ablatt
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class LogData implements Serializable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -8666641790306170842L;

    /**
     * The mac address of the device
     */
    @XmlElement
    private long deviceId;

    /**
     * The name of the app
     */
    @XmlElement
    private String appName;

    /**
     * The sim time the log was made at
     */
    @XmlElement
    private BigDecimal simTime;

    /**
     * The user specified key
     */
    @XmlElement
    private String key;

    /**
     * The user specified value
     */
    @XmlElement
    private String message;

    // Default constructor needed for serialization
    public LogData() {}

    /**
     * Constructor
     * 
     * @param devId The mac
     * @param appName The app name
     * @param time The sim time
     * @param key The key
     * @param message The value
     */
    public LogData(long devId, String appName, BigDecimal time, String key, String message) {
        this.deviceId = devId;
        this.appName = appName;
        this.simTime = time;
        this.key = key;
        this.message = message;
    }

    /**
     * Gets the MAC id
     * 
     * @return deviceId The MAC id of the device
     */
    public long getDeviceId() {
        return this.deviceId;
    }

    /**
     * Gets the application name
     * 
     * @return The name of the application
     */
    public String getAppName() {
        return this.appName;
    }

    /**
     * Gets the simulation time
     * 
     * @return simTime The simulation time
     */
    public BigDecimal getSimTime() {
        return this.simTime;
    }

    /**
     * Gets the user defined key
     * 
     * @return key The user defined key
     */
    public String getKey() {
        return this.key;
    }

    /**
     * Gets the user defined value
     * 
     * @return message The user defined value
     */
    public String getMessage() {
        return this.message;
    }

    /**
     * Creates a string representation of the log data
     */
    @Override
    public String toString() {
        return String.format("Device Id: %d, App Name: %s, Sim Time: %.1f, Key: %s, Message: %s", deviceId, appName, simTime.doubleValue(), key, message);
    }

    /**
     * Equivalence check, mainly for testing
     * 
     * @return True or false if the objects are equal
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof LogData) {
            LogData data = (LogData)o;
            return this.deviceId == data.deviceId && UtilsSpecialEquals.equalsPossibleNull(this.appName, data.appName) && UtilsSpecialEquals.equalsPossibleNull(this.simTime, data.simTime)
                    && UtilsSpecialEquals.equalsPossibleNull(this.key, data.key) && UtilsSpecialEquals.equalsPossibleNull(this.message, data.message);
        }
        else {
            return false;
        }
    }

    /**
     * Creates hashcode for log entry
     * 
     * @return The hashcode
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder(431, 63).append(deviceId).hashCode();
    }

    /**
     * Convert this log data to CSV format.
     * 
     * @param execId Exec id to be used as part of the format.
     * @return The log data in csv format.
     */
    public String toCsvString(String execId) {
        StringBuilder sb = new StringBuilder();

        sb.append(execId);
        sb.append(',');
        sb.append(deviceId);
        sb.append(',');
        sb.append(appName);
        sb.append(',');
        sb.append(simTime.toString());
        sb.append(',');
        sb.append(key);
        sb.append(',');
        sb.append(message);

        return sb.toString();
    }
}
