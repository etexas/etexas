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
package org.etexascode.apps.microscopicmodel;

import java.util.Collection;

import org.etexascode.apps.MicroscopicIntellifusionBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MicroscopicModelApp extends MicroscopicIntellifusionBaseApp {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(MicroscopicModelApp.class);

    @Override
    public void performUpdate(InterRepInfoModel model, RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        LOGGER.debug("sign man", model.smi.toString());
        LOGGER.debug("det man", model.dmi.toString());
        LOGGER.debug("veh man", model.getVmi().toString());
        LOGGER.debug("lan man", model.lmi.toString());

    }

    @Override
    public String getAppName() {
        return "MicroscopicModelApp";
    }
}
