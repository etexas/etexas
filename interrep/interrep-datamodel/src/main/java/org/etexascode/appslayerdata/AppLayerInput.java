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
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;

/**
 * The data used as the inputs to the app layer.
 * 
 * @author ablatt
 */
public class AppLayerInput {

    /** An application to execute. */
    public final IConnectedVehicleApp app;

    /** The information of the device the application is "running on". */
    public final IDeviceInfo input;

    /**
     * Constructor
     * 
     * @param app The application to run.
     * @param input The device the application is "running on".
     */
    public AppLayerInput(IConnectedVehicleApp app, IDeviceInfo input) {
        this.app = app;
        this.input = input;
    }

    /**
     * Compares applications for equality.
     * 
     * @param o The object to be compared.
     * @return True/False that the applications are equal
     */
    @Override
    public boolean equals(Object o) {
        // Note: ablatt - i can't think of a good way to compare applications for equality,
        // so I'm going to rely solely on the device info
        if (o instanceof AppLayerInput) {
            AppLayerInput ad = (AppLayerInput)o;
            return UtilsSpecialEquals.equalsPossibleNull(input, ad.input);
        }
        else {
            return false;
        }
    }

    /**
     * Creates a hashcode with the device the application is "running on" and the application to
     * run.
     * 
     * @return The hashcode
     */
    @Override
    @CoberturaIgnore
    public int hashCode() {
        if (app instanceof IAppName) {
            return new HashCodeBuilder(31, 99).append(input).append(((IAppName)app).getAppName()).hashCode();
        }
        return new HashCodeBuilder(31, 99).append(input).append(app.getClass().getSimpleName()).hashCode();
    }
}
