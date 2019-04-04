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
package org.etexascode.apps.dcs;

import java.util.Collection;

import org.etexascode.apps.MicroscopicIntellifusionBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.InterRepInfoModel;

/**
 * The DCS Application.
 * 
 * @author jrutherford.
 */
public class DCSApp extends MicroscopicIntellifusionBaseApp {

    /** Identifier for this Application. */
    public static final String APP_NAME = "dcs-app";

    /** The DCS Algorithm. */
    private DCSAlgorithm algorithm;

    @AppConfigProperty(displayName = "Lanes", description = "Comma separated list of lane IDs.")
    private int laneID[];

    @AppConfigProperty(value = "10.0", displayName = "Stage 1 Timeout", description = "The timeout in seconds of the first stage.")
    private double stage1;

    @AppConfigProperty(value = "5.0", displayName = "Stage 2 Timeout", description = "The timeout in seconds of the second stage.")
    private double stage2;

    @Override
    public String getAppName() {
        return APP_NAME;
    }

    @Override
    public void init(String[] appConfigs) {

        String split[] = appConfigs[0].split(",");
        laneID = new int[split.length];
        for (int i = 0; i < split.length; i++) {
            laneID[i] = Integer.parseInt(split[i]);
        }
        stage1 = Double.parseDouble(appConfigs[1]);
        stage2 = Double.parseDouble(appConfigs[2]);
        intersection = Integer.parseInt(appConfigs[3]);
    }

    @Override
    public void performUpdate(InterRepInfoModel model, RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        if (algorithm == null) {
            algorithm = new DCSAlgorithm(device, model, logger, laneID);
            algorithm.configureTimeouts(stage1, stage2);
        }
        else {
            algorithm.updateManagers(model, device);
        }

        algorithm.performDetectionControlLoop(simTime);

    }
}