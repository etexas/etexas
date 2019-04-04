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

package org.etexascode.interrep.datamodel;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;

/**
 * The Vehicle Command Abstract Class Contains variables used by all the other sub classes.
 * 
 * @author jrutherford
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlSeeAlso({ VehicleSpeedCommand.class, VehicleLaneChangeCommand.class, VehicleDestinationCommand.class })
public class VehicleCommand extends Command implements Serializable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -1176639313058357218L;

    /** The vehicle ID. 0 for all. */
    @XmlElement
    private int vehicleID;

    /** Constructor. Intentionally left blank. */
    public VehicleCommand() {}

    /**
     * Constructs a vehicle command with vehicle id and the command to perform.
     * 
     * @param vehicleID The id of the vehicle to perform the command.
     */
    public VehicleCommand(int vehicleID) {
        this.vehicleID = vehicleID;
    }

    /**
     * Gets the vehicle ID.
     * 
     * @return The vehicle ID.
     */
    public int getVehicleID() {
        return vehicleID;
    }

    /**
     * Creates an equals method for comparison.
     * 
     * @param o The object to compare to.
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof VehicleCommand) {
            VehicleCommand vc = (VehicleCommand)o;
            return (this.vehicleID == vc.vehicleID);

        }
        return false;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(41, 43).append(vehicleID).toHashCode();
    }
}