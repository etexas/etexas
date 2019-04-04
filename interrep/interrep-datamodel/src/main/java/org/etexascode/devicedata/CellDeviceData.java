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

import java.util.List;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * A class containing the information for cellular devices.
 * 
 * @author ttevendale
 */
public class CellDeviceData extends AbstractEmbeddedDeviceData {

    /**
     * the id of the specific cell phone of this device
     */
    public final String cellId;

    /**
     * Constructor for this tuple
     * 
     * @param deviceRuleId The device rule ID
     * @param apps The apps for this device
     * @param vehId The vehicle id the device is on
     * @param cellId the cellular id that is in the vehicle
     */
    public CellDeviceData(long deviceRuleId, List<IConnectedVehicleApp<?>> apps, String vehId, String cellId) {
        super(deviceRuleId, apps, vehId);
        this.cellId = cellId;
    }

    /**
     * Verifies that the vehicle id and cell id is in the OBU device data
     * 
     * @return True/False the ids is in the OBU device data
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof CellDeviceData) {
            CellDeviceData add = (CellDeviceData)o;
            boolean b = vehicleId.equals(add.vehicleId);
            b &= cellId.equals(add.cellId);
            return b;
        }
        else {
            return false;
        }
    }

    /**
     * Creates a hashcode entry
     * 
     * @return The hashcode
     */
    @Override
    @CoberturaIgnore
    public int hashCode() {
        return new HashCodeBuilder(81, 907).append(vehicleId).append(cellId).hashCode();
    }

    /**
     * Verifies the vehicle id and cell id is the same as the proper id
     * 
     * @return True/False the id matches
     */
    @Override
    public boolean equalsId(IDable entity) {
        return getProperId().equals(entity.getProperId());
    }

    /**
     * Gets the proper id for the vehicle matched with cellId Note: the cellId is not unique across
     * vehicles; only unique in a vehicle Format is something like this "5 1" 5 being the vehicle ID
     * and 1 being the cell ID
     * 
     * @return vehicleId The proper id for the vehicle and cellId
     */
    @Override
    public String getProperId() {
        return String.format("%s %s", vehicleId, cellId);
    }
}
