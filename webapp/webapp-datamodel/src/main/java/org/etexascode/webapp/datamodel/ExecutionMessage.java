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
package org.etexascode.webapp.datamodel;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * An execution message.
 * 
 * @author ttevendale
 */
@Entity
@Table(name = "execution_messages")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ExecutionMessage extends AbstractEntity {

    /** The serial version ID */
    private static final long serialVersionUID = 1L;

    /** The source of this simulation. */
    @Column(name = "simulation")
    private long simulationId;

    /** The type of the execution message */
    ExecutionMessageType type;

    /** The actual message */
    private String message;

    /**
     * Gets the simulation ID that the execution message is attached to.
     * 
     * @return The simulation ID.
     */
    public long getSimulationId() {

        return simulationId;
    }

    /**
     * Sets the simulation ID that the execution message is attached to.
     * 
     * @param simulationId The new simulation ID.
     */
    public void setSimulationId(long simulationId) {

        this.simulationId = simulationId;
    }

    /**
     * Gets the type of the execution message.
     * 
     * @return Gets the type of the execution message.
     */
    public ExecutionMessageType getType() {

        return type;
    }

    /**
     * Sets the type of the execution message.
     * 
     * @param type Sets the type of the execution message.
     */
    public void setType(ExecutionMessageType type) {

        this.type = type;
    }

    /**
     * Gets the actual execution message.
     * 
     * @return The actual execution message.
     */
    public String getMessage() {

        return message;
    }

    /**
     * Sets the actual execution message.
     * 
     * @param message The actual execution message.
     */
    public void setMessages(String message) {

        this.message = message.replaceAll("(\r\n|\n\r|\r|\n)", "<br />");
    }
}