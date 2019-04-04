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
package org.etexascode.apps.cell;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.etexascode.apps.CellularDevice;
import org.etexascode.apps.ICellularBaseApp;
import org.etexascode.cellular.BasicCellMessage;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.CellMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * A cell message producing cell app.
 * 
 * @author ttevendale
 */
public class CellMessageProducerApp implements ICellularBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_CELL_MESSAGE_PRODUCER_APP = "CellMessageProducerApp";

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * The frequency at which to produce messages in seconds. SPEC states that 10Hz is the default.
     */
    @AppConfigProperty(value = "0.1", displayName = "frequency", description = "The frequency to send messages in seconds.")
    double frequency;

    /**
     * The destination mac address.
     */
    @AppConfigProperty(value = "0", displayName = "destination mac address", description = "The mac address to send messages to.")
    long destMac;

    @Override
    public void init(String[] appConfigs) {
        frequency = Double.parseDouble(appConfigs[0]);
        destMac = Long.parseLong(appConfigs[1]);
    }

    @Override
    public void performUpdate(CellularDevice device, Object[] messages, Collection<BasicMessage> receive, Double simtime, AppLogger logger) {
        if (lastTxTime != null) {
            if (simtime - lastTxTime < frequency) {
                return;
            }
        }

        lastTxTime = simtime;

        List<BasicMessage> ret = new ArrayList<BasicMessage>(1);
        BasicCellMessage bcm = new BasicCellMessage();
        bcm.setMsgID("Cell Message");

        IDistanceable location = device.getLocation();
        bcm.setLocation(String.format("X Coordinate: %f, Y Coordinate: %f", location.getX(), location.getY()));
        bcm.setMessage("I'm sending a message!!! Did you get it?");
        int size = 500;
        ret.add(new CellMessage(bcm, destMac, size));

        device.addAppMessages(ret);
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    @Override
    public String getAppName() {
        return APP_NAME_CELL_MESSAGE_PRODUCER_APP;
    }

}