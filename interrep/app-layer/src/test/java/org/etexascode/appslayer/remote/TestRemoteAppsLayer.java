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
package org.etexascode.appslayer.remote;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.appslayer.IRemoteAppLayer;
import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.interrep.datamodel.DistanceImpl;

/**
 * Test interface.
 * 
 * @author jrutherford
 */
public class TestRemoteAppsLayer implements IRemoteAppLayer {

    @Override
    public List<AppLayerOutput> execApps(List<AppLayerInput> apps) {
        List<AppLayerOutput> output = new ArrayList<AppLayerOutput>();
        String str = "";
        long num = (long)0;
        DistanceImpl location = new DistanceImpl(0, 0, 0);
        AppLayerOutput alo = new AppLayerOutput(str, num, location);
        output.add(alo);
        return output;
    }
}