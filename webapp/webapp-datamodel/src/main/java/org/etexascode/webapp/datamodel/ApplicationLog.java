/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.webapp.datamodel;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * An execution application log.
 * 
 * @author ttevendale
 */
@Entity
@Table(name = "application_logs")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ApplicationLog extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The ID of the parent execution. */
    @Column(name = "execution")
    private long executionId;

    /** The application's device's mac address. */
    @Column(name = "mac_address")
    private long deviceId;

    /** The name of the application. */
    @Column(name = "application_name")
    private String applicationName;

    /** The simulation time the log was made at. */
    @Column(name = "simulation_time")
    private double simulationTime;

    /** The user specified key. */
    @Column(name = "application_key")
    private String applicationKey;

    /** The user specified value. */
    @Column(name = "application_message")
    private String applicationMessage;

    /**
     * Returns the ID of the parent execution.
     * 
     * @return The long ID of the parent execution.
     */
    public long getExecutionId() {

        return executionId;
    }

    /**
     * Sets the ID of the parent execution.
     * 
     * @param executionId The long parent execution ID to set.
     */
    public void setExecutionId(long executionId) {

        this.executionId = executionId;
    }

    /**
     * Returns the device ID for this application log.
     * 
     * @return The long device ID for this application log.
     */
    public long getDeviceId() {

        return deviceId;
    }

    /**
     * Sets the device ID for this application log.
     * 
     * @param deviceId The long device ID to set.
     */
    public void setDeviceId(long deviceId) {

        this.deviceId = deviceId;
    }

    /**
     * Returns the name of this application log.
     * 
     * @return The String name for this application log.
     */
    public String getApplicationName() {

        return applicationName;
    }

    /**
     * Sets the name for this application log.
     * 
     * @param applicationName The String name to set.
     */
    public void setApplicationName(String applicationName) {

        this.applicationName = applicationName;
    }

    /**
     * Returns the simulation time for this application log.
     * 
     * @return The double simulation time for this application log.
     */
    public double getSimulationTime() {

        return simulationTime;
    }

    /**
     * Sets the simulation time for this application log.
     * 
     * @param simulationTime The double simulation time to set.
     */
    public void setSimulationTime(double simulationTime) {

        this.simulationTime = simulationTime;
    }

    /**
     * Returns the key for this application log.
     * 
     * @return The String key for this application log.
     */
    public String getApplicationKey() {

        return applicationKey;
    }

    /**
     * Sets the key for this application log.
     * 
     * @param applicationKey The String key to set.
     */
    public void setApplicationKey(String applicationKey) {

        this.applicationKey = applicationKey;
    }

    /**
     * Returns the message for this application log.
     * 
     * @return The String messsage for this application log.
     */
    public String getApplicationMessage() {

        return applicationMessage;
    }

    /**
     * Sets the message for this application log.
     * 
     * @param applicationMessage The String message to set.
     */
    public void setApplicationMessage(String applicationMessage) {

        this.applicationMessage = applicationMessage;
    }

}
