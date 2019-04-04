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
package org.etexascode.appslayer;

import java.util.List;

import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;

/**
 * Interface which all remote app layer implementations must extend.
 * 
 * @author jrutherford
 */
public interface IRemoteAppLayer {

    /**
     * Execute the apps for this specific time step.
     * 
     * @param apps The apps from the App layer.
     * @return The list of app layer outputs.
     */
    public List<AppLayerOutput> execApps(List<AppLayerInput> apps);
}
