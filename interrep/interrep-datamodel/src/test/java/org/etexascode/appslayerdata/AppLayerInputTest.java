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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jconnelly
 */
public class AppLayerInputTest {

    IConnectedVehicleApp<String> app = null;

    IConnectedVehicleApp<Long> app2 = null;

    IDeviceInfo input = null;

    AppLayerInput ali = null;

    int stepNum = 0;

    List<BasicMessage> messages = null;

    long id = 0;

    IDeviceData idd = null;

    @Before
    public void setup() {
        id = 9876;
        stepNum = 1;
        messages = new ArrayList<BasicMessage>(0);
        input = produceIDeviceInfo();
        app = new IConnectedVehicleApp<String>() {

            @Override
            public void performUpdate(String device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {}
        };
        app2 = new IConnectedVehicleApp<Long>() {

            @Override
            public void performUpdate(Long device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

            }
        };

        idd = produceIDeviceData();
        ali = genAppLayerInput();
    }

    @After
    public void teardown() {
        app = null;
        app2 = null;
        messages = null;
        idd = null;
        id = 0;
        stepNum = 0;
        ali = null;
        input = null;
    }

    @Test
    public void testConstructor() {
        assertEquals(ali, genAppLayerInput());
    }

    @Test
    public void testEqualsTrue() {
        AppLayerInput test = genAppLayerInput();
        assertEquals(ali, test);
    }

    @Test
    public void testEqualsFalse() {
        IDeviceInfo input2 = new OBUDeviceInfo(v2(), messages, 1234);
        AppLayerInput test2 = new AppLayerInput(app2, input2);
        assertFalse(ali.equals(test2));
        AppLayerOutput test3 = genAppLayerOutput();
        assertFalse(ali.equals(test3));
    }

    @Test
    public void testhashCode() {
        AppLayerInput test4 = genAppLayerInput();
        assertEquals(ali.hashCode(), test4.hashCode());
        AppLayerOutput test5 = genAppLayerOutput();
        assertNotSame(ali.hashCode(), test5.hashCode());
    }

    private AppLayerOutput genAppLayerOutput() {
        String str = "";
        long num = (long)0;
        DistanceImpl location = new DistanceImpl(0, 0, 0);
        return new AppLayerOutput(str, num, location);
    }

    private AppLayerInput genAppLayerInput() {
        AppLayerInput ali = new AppLayerInput(app, input);
        return ali;
    }

    private IDeviceInfo produceIDeviceInfo() {
        IDeviceInfo devinfo = new OBUDeviceInfo(v1(), messages, id);
        return devinfo;
    }

    private IVehicle v1() {
        Vehicle v1 = new Vehicle(1, 10.0, 10.0, 25.0, 25.0, 0.0);
        return v1;
    }

    private IVehicle v2() {
        Vehicle v2 = new Vehicle(2, 10.0, 10.0, 25.0, 25.0, 0.0);
        return v2;
    }

    private IDeviceData produceIDeviceData() {
        List<IConnectedVehicleApp<?>> applist = new ArrayList<IConnectedVehicleApp<?>>(1);
        applist.add(0, app);
        IDeviceData devdata = new OBUDeviceData(0, applist, "Vehicle:1");
        return devdata;
    }
}
