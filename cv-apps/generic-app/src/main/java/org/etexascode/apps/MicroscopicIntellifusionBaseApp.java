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
package org.etexascode.apps;

import java.util.Collection;

import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;

/**
 * Base app for producing a microscopic model from messages.
 * 
 * @author ablatt
 */
public abstract class MicroscopicIntellifusionBaseApp implements IRSEBaseApp, IAppName, IAppLifecycle {

    /**
     * The sim time step interval calculated using simTime
     */
    private Double timeStepInterval;

    /**
     * The driver for creating the microscopic model.
     */
    MicroscopicIntellifusionDriver hid = new MicroscopicIntellifusionDriver();

    /** The ID of the intersection to model. */
    @AppConfigProperty(value = "0", description = "The ID of the intersection model.")
    protected int intersection;

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        // if first update, timeStepInterval equals simTime-0
        if (timeStepInterval == null) {
            timeStepInterval = simTime;
        }

        InterRepInfoModel irim = hid.parseModel(messages, device.getDetectorManager(intersection), device.getLaneManager(intersection), simTime, getGeoCalcType(), timeStepInterval);
        if (irim == null) {
            return;
        }

        performUpdate(irim, device, messages, receive, simTime, logger);
    }

    /**
     * The app's update method.
     * 
     * @param model The model produced by the HybridIntellifusionDriver this time step.
     * @param device The device this app is running on.
     * @param messages The message datums received by device this time step
     * @param receive The messages with wrappers received by device this time step
     * @param simTime The simulation time for this time step
     * @param logger A logger whose logs may be easily accessed by the user at a later time
     */
    public abstract void performUpdate(InterRepInfoModel model, RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger);

    /**
     * The calculator type being used by the user
     * 
     * @return The type of calc to use
     */
    public int getGeoCalcType() {
        return UtilsLatLongConversion.GEODETIC2D;
    }

    /**
     * Getter
     * 
     * @return The HybridIntellifusionDriver.
     */
    public MicroscopicIntellifusionDriver getDriver() {
        return hid;
    }

    @Override
    public void init(String[] appConfigs) {

        intersection = Integer.parseInt(appConfigs[0]);
    }

    @Override
    public void appShutdown(AppLogger logger) {}
}
