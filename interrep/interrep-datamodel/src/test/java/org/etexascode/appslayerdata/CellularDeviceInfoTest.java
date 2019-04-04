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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.HashSet;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */

public class CellularDeviceInfoTest {

    IDistanceable location;

    Long mac;

    Collection<BasicMessage> messages;

    CellularDeviceInfo cellDeviceInfo;

    @Before
    public void setup() {
        location = new DistanceImpl(5, 10, 15);
        mac = (long)123;
        messages = new HashSet<BasicMessage>();
        cellDeviceInfo = new CellularDeviceInfo(location, messages, mac);
    }

    @After
    public void teardown() {
        location = null;
        mac = null;
        messages = null;
        cellDeviceInfo = null;
    }

    @Test
    public void testConstructor() {
        CellularDeviceInfo cellDeviceInfo = new CellularDeviceInfo(location, messages, mac);
        assertTrue(mac == cellDeviceInfo.getDeviceId());

        IDistanceable cellLocation = cellDeviceInfo.getLocation();
        assertTrue(location.getX() == cellLocation.getX());
        assertTrue(location.getY() == cellLocation.getY());
        assertTrue(location.getZ() == cellLocation.getZ());
    }

    @Test
    public void testEqualsIdTrue() {

        CellularDeviceInfo samePropIdCellDevice = new CellularDeviceInfo(location, messages, cellDeviceInfo.getDeviceId());
        assertTrue(cellDeviceInfo.equalsId(samePropIdCellDevice));
    }

    @Test
    public void testEqualsIdFalse() {
        CellularDeviceInfo diffPropIdCellDevice = new CellularDeviceInfo(location, messages, cellDeviceInfo.getDeviceId() + 50);
        assertFalse(cellDeviceInfo.equalsId(diffPropIdCellDevice));
    }

    @Test
    public void testGetProperId() {
        String correctProperId = String.format("CellDevice:%d", mac);
        assertTrue(correctProperId.equals(cellDeviceInfo.getProperId()));
    }
}
