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

/**
 * Vehicle Destination Commands.
 * @author jrutherford
 */

/**
 * THIS IS CURRENTLY A HOLDER CLASS AS THE OPERATIONS ABOVE ARE NOT CURRENTLY SUPPORTED BY THE
 * INJECT MESSAGE API. TODO jrutherford
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class VehicleDestinationCommand extends VehicleCommand {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -8322813308917130949L;

    /**
     * Vehicle Operations. These need to be updated if functionality is added.
     */
    @XmlTransient
    public static final int TURN_LEFT_AT_INTERSECTION = 1;

    @XmlTransient
    public static final int TURN_RIGHT_AT_INTERSECTION = 2;

    @XmlTransient
    public static final int STAY_STRAIGHT_AT_INTERSECTION = 3;

    /** The vehicle destination command. */
    @XmlElement
    private int destCommand;

    /** Constructor. */
    public VehicleDestinationCommand() {}

    /**
     * Constructor.
     * 
     * @param vehicleID The vehicle ID.
     * @param commandID The command to perform. (Constants Above)
     */
    public VehicleDestinationCommand(int vehicleID, int commandID) {

        super(vehicleID);

        if (commandID < TURN_LEFT_AT_INTERSECTION || commandID > STAY_STRAIGHT_AT_INTERSECTION) {

            throw new IllegalArgumentException(String.format("The value \"%d\" is not a recognized vehicle destination command.", commandID));
        }

        this.destCommand = commandID;
    }

    @Override
    public String getDescription() {

        switch (destCommand) {

            case VehicleDestinationCommand.STAY_STRAIGHT_AT_INTERSECTION:
                return String.format("Request that vehicle %d stay straight at the intersection", getVehicleID());

            case VehicleDestinationCommand.TURN_LEFT_AT_INTERSECTION:
                return String.format("Request that vehicle %d turn left at the intersection", getVehicleID());

            case VehicleDestinationCommand.TURN_RIGHT_AT_INTERSECTION:
                return String.format("Request that vehicle %d turn right at the intersection", getVehicleID());

            default:
                throw new IllegalStateException(String.format("The value \"%d\" is not a recognized vehicle destination command.", destCommand));
        }
    }

    /**
     * Gets the command to execute.
     * 
     * @return Command constant from above.
     */
    public int getDestCommand() {
        return destCommand;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof VehicleDestinationCommand) {
            if (super.equals(o)) {
                VehicleDestinationCommand vdc = (VehicleDestinationCommand)o;
                return (this.destCommand == vdc.destCommand);
            }
        }
        return false;
    }

    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(43, 31).append(destCommand).toHashCode();
    }
}