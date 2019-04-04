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
import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * A tuple linking the device mac to the vehicle id to the apps on the device.
 * 
 * @author ablatt
 */
public class OBUDeviceData extends AbstractEmbeddedDeviceData {

    /**
     * Constructor for this tuple
     * 
     * @param deviceRuleId The device rule ID
     * @param apps The apps for this device
     * @param vehId The vehicle id the device is on
     */
    public OBUDeviceData(long deviceRuleId, List<IConnectedVehicleApp<?>> apps, String vehId) {
        super(deviceRuleId, apps, vehId);
    }

    /**
     * Verifies that the vehicle id is in the OBU device data
     * 
     * @return True/False the id is in the OBU device data
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof OBUDeviceData) {
            OBUDeviceData add = (OBUDeviceData)o;
            return vehicleId.equals(add.vehicleId);
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
    public int hashCode() {
        return new HashCodeBuilder(995, 387).append(vehicleId).hashCode();
    }

    /**
     * Verifies the vehicle id is the same as the proper id
     * 
     * @return True/False the id matches
     */
    @Override
    public boolean equalsId(IDable entity) {
        return vehicleId.equals(entity.getProperId());
    }

    /**
     * Gets the proper id for the vehicle
     * 
     * @return vehicleId The proper id for the vehicle
     */
    @Override
    public String getProperId() {
        return vehicleId;
    }
}
