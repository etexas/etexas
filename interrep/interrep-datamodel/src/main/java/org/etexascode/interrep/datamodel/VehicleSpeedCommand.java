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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Vehicle Speed Commands.
 * 
 * @author jrutherford
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class VehicleSpeedCommand extends VehicleCommand {

    /**
     * private final static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(VehicleSpeedCommand.class);

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -6631923769198842677L;

    /** Vehicle Operations (Number corresponds to VMS message value.) */
    @XmlTransient
    public static final int MAX_ACCELERATE_TO_XX = 1;

    @XmlTransient
    public static final int MAX_DECELERATE_TO_XX = 2;

    @XmlTransient
    public static final int NORMAL_ACCELERATE_TO_XX = 3;

    @XmlTransient
    public static final int NORMAL_DECELERATE_TO_XX = 4;

    /** The vehicle speed command. */
    @XmlElement
    private int speedCommand;

    /** The vehicle speed. */
    @XmlElement
    private double speed;

    /** Constructor. */
    public VehicleSpeedCommand() {}

    /**
     * Constructor that uses the vehicle id, the command id to perform from constant and the speed.
     * 
     * @param vehicleID The vehicle ID.
     * @param commandID The command to perform. (Constants Above)
     * @param speed The new vehicle speed.
     */
    public VehicleSpeedCommand(int vehicleID, int commandID, double speed) {

        super(vehicleID);

        if (commandID < MAX_ACCELERATE_TO_XX || commandID > NORMAL_DECELERATE_TO_XX) {

            throw new IllegalArgumentException(String.format("The value \"%d\" is not a recognized vehicle speed command.", commandID));
        }

        this.speedCommand = commandID;
        this.speed = speed;
    }

    @Override
    public String getDescription() {

        return String.format("Request that vehicle %d change its speed to %.2f m/s", getVehicleID(), speed);
    }

    /**
     * Gets the command to execute.
     * 
     * @return Command constant from above.
     */
    public int getSpeedCommand() {
        return speedCommand;
    }

    /**
     * Gets the vehicle speed.
     * 
     * @return The vehicle speed.
     */
    public double getSpeed() {
        return speed;
    }

    @Override
    public String toString() {
        StringBuilder comm = new StringBuilder();

        comm.append("VehicleID = ");
        comm.append(getVehicleID());
        comm.append(", ");
        if (getSpeedCommand() == MAX_ACCELERATE_TO_XX) {
            comm.append("Command: Max Accelerate ");

        }
        else if (getSpeedCommand() == MAX_DECELERATE_TO_XX) {
            comm.append("Command: Max Decelerate ");

        }
        else if (getSpeedCommand() == NORMAL_ACCELERATE_TO_XX) {
            comm.append("Command: Normal Accelerate ");

        }
        else if (getSpeedCommand() == NORMAL_DECELERATE_TO_XX) {
            comm.append("Command: Normal Decelerate ");

        }
        else {
            LOGGER.debug("Wrong speed command entered");
        }
        comm.append(", ");
        comm.append("Speed: ");
        comm.append(getSpeed());

        return comm.toString();

    }

    /**
     * Equality check for unit testing.
     * 
     * @param o The object to compare to.
     * @return True/False whether the object is equal.
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof VehicleSpeedCommand) {
            if (super.equals(o)) {
                VehicleSpeedCommand vsc = (VehicleSpeedCommand)o;
                double tmp = this.speed - vsc.speed;
                return (this.speedCommand == vsc.speedCommand) && (tmp <= 0.05) && (tmp >= -0.05);
            }
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
        return new HashCodeBuilder(41, 43).append(speedCommand).append(speed).toHashCode();
    }
}