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
 * Vehicle Lane Change Commands.
 * 
 * @author jrutherford
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class VehicleLaneChangeCommand extends VehicleCommand {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 11036494428310122L;

    /** Vehicle Operations. */
    @XmlTransient
    public static final int CHANGE_LANE_LEFT = 1;

    @XmlTransient
    public static final int CHANGE_LANE_RIGHT = 2;

    /** The vehicle lane command. */
    @XmlElement
    private int laneCommand;

    /** Constructor. */
    public VehicleLaneChangeCommand() {}

    /**
     * Constructor.
     * 
     * @param vehicleID The vehicle ID.
     * @param commandID The command to perform. (Constants Above)
     */
    public VehicleLaneChangeCommand(int vehicleID, int commandID) {

        super(vehicleID);

        if (commandID != CHANGE_LANE_LEFT && commandID != CHANGE_LANE_RIGHT) {

            throw new IllegalArgumentException(String.format("The value \"%d\" is not a recognized vehicle lane change command.", commandID));
        }

        this.laneCommand = commandID;
    }

    @Override
    public String getDescription() {

        switch (laneCommand) {

            case VehicleLaneChangeCommand.CHANGE_LANE_LEFT:
                return String.format("Request that vehicle %d make a left lane change", getVehicleID());

            case VehicleLaneChangeCommand.CHANGE_LANE_RIGHT:
                return String.format("Request that vehicle %d make a right lane change", getVehicleID());

            default:
                throw new IllegalStateException(String.format("The value \"%d\" is not a recognized vehicle lane change command.", laneCommand));
        }
    }

    /**
     * Gets the command to execute.
     * 
     * @return Command constant from above.
     */
    public int getLaneCommand() {
        return laneCommand;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof VehicleLaneChangeCommand) {
            if (super.equals(o)) {
                VehicleLaneChangeCommand vlcc = (VehicleLaneChangeCommand)o;
                return (this.laneCommand == vlcc.laneCommand);
            }
        }
        return false;
    }

    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(31, 57).append(laneCommand).toHashCode();
    }

    @Override
    public String toString() {

        StringBuilder comm = new StringBuilder();

        comm.append("VehicleID = ");
        comm.append(getVehicleID());
        comm.append(", ");
        if (getLaneCommand() == CHANGE_LANE_LEFT) {
            comm.append("Command: Change_Lane_Left");
        }
        else if (getLaneCommand() == CHANGE_LANE_RIGHT) {

            comm.append("Command: Change_Lane_Right");
        }

        return comm.toString();
    }
}