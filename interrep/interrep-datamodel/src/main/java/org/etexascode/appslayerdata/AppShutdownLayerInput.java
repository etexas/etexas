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
package org.etexascode.appslayerdata;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;

/**
 * Structure containing app shutdown data
 * 
 * @author ablatt
 */
public class AppShutdownLayerInput {

    /**
     * The id of the device the app is on
     */
    public final long deviceId;

    /**
     * The app to be shut down
     */
    public final IConnectedVehicleApp app;

    /**
     * Constructor
     * 
     * @param devId The device id the app is running on
     * @param app The app to shut down
     */
    public AppShutdownLayerInput(long devId, IConnectedVehicleApp app) {
        deviceId = devId;
        this.app = app;
    }

    /**
     * Primarily used for testing. Only provides referential equality. Useful for determining that
     * apps are being moved around properly.
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof AppShutdownLayerInput) {
            AppShutdownLayerInput asli = (AppShutdownLayerInput)o;
            return (deviceId == asli.deviceId) && (app == asli.app);
        }
        else {
            return false;
        }
    }

    /**
     * Generates hashcode with device id and app id appended
     */
    @Override
    @CoberturaIgnore
    public int hashCode() {

        if (app instanceof IAppName) {
            return new HashCodeBuilder(17, 19).append(deviceId).append(((IAppName)app).getAppName()).toHashCode();
        }
        return new HashCodeBuilder(17, 19).append(deviceId).append(app.getClass().getSimpleName()).toHashCode();
    }
}
