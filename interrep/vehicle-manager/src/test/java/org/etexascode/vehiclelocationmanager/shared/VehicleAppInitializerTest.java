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
package org.etexascode.vehiclelocationmanager.shared;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.testclasses.TestConnectedVehicleAppImpl;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author ablatt
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({ LoggerFactory.class, VehicleAppInitializer.class })
public class VehicleAppInitializerTest {

    List<Class<?>> initAppClasses = null;

    List<String[]> initAppArgs = null;

    AppInitConfig initAppConfig = null;

    int initAppIndex = 0;

    VehicleAppInitializer vai = null;

    String[] initAppExpected = null;

    int initAppsRandom1 = 10; // expected: return 1: 113 -- return 2: 8220501950349663546 -- return
                              // 3: 1773446580 - return 3 = 1913984760?

    long initAppsMac1 = 8220501950349663546L;

    int initAppsRandomExpected1 = 1913984760;

    int vehId = 42;

    String vehProperId = "Vehicle:42";

    int initAppsRandom2 = 1000; // expected: return 1: 531 -- return 2: -1594007938

    int initAppsRandomExpected2 = -1594007938;

    List<IVehicle> initAppsVehs = null;

    List<AppInitConfig> initAppsConf = null;

    static Logger mockLogger = null;

    @Before
    public void setup() {
        if (mockLogger == null) {
            mockLogger = PowerMockito.mock(Logger.class);
        }
        PowerMockito.mockStatic(LoggerFactory.class);
        PowerMockito.when(LoggerFactory.getLogger(Mockito.any(Class.class))).thenReturn(mockLogger);

        initAppExpected = new String[] { "something", "else" };
        initAppClasses = initAppAppsList();
        initAppArgs = initAppsArgsList();
        initAppConfig = new AppInitConfig(0, initAppClasses, initAppArgs, 0, 500, 0, 0);
        initAppIndex = 0;
        vai = new VehicleAppInitializer();

        initAppsVehs = new ArrayList<IVehicle>(1);
        initAppsVehs.add(GenVehicleFunctions.genVehicle(0, 0, 0, 0, 0, vehId));

        initAppsConf = new ArrayList<AppInitConfig>(1);
        initAppsConf.add(initAppConfig);
    }

    @After
    public void teardown() {
        initAppClasses = null;
        initAppArgs = null;
        initAppIndex = 0;
        initAppConfig = null;
        vai = null;
        initAppExpected = null;
        initAppsVehs = null;
        initAppsConf = null;
    }

    @Test
    public void testConstructor() {
        VehicleAppInitializer vai = new VehicleAppInitializer();
        assertEquals(1000, vai.totalRangeMax);
    }

    @Test
    public void testInitApps1() throws InstantiationException, IllegalAccessException {
        InitializedAppData iad = vai.dsrcInitApps(initAppsVehs, initAppsConf, initAppsRandom1);
        assertEquals(initAppsRandomExpected1, iad.randSeed);
        assertEquals(1, iad.initedApps.size());
        assertEquals(vehProperId, iad.initedApps.get(0).vehicleId);
        assertEquals(2, iad.initedApps.get(0).apps.size());
        assertTrue(iad.initedApps.get(0).apps.get(0) instanceof TestConnectedVehicleAppImpl);
    }

    @Test
    public void testInitConfig() throws InstantiationException, IllegalAccessException {
        List<IConnectedVehicleApp<?>> res = vai.initConfig(initAppConfig);
        assertEquals(2, res.size());
        assertTrue(res.get(0) != res.get(1));
        assertTrue(initAppExpected == ((TestConnectedVehicleAppImpl)res.get(0)).configs);
        assertTrue(initAppExpected == ((TestConnectedVehicleAppImpl)res.get(1)).configs);
    }

    @Test
    public void testInitApp() throws InstantiationException, IllegalAccessException {
        TestConnectedVehicleAppImpl res = (TestConnectedVehicleAppImpl)vai.initApp(initAppConfig, 0);
        assertTrue(initAppExpected == res.configs);
    }

    @Test
    public void testInitAppThrowsException() throws InstantiationException, IllegalAccessException {
        List<Class<?>> apps = new ArrayList<Class<?>>(1);
        apps.add(Map.class); // any interface will throw InstantiationException
        initAppConfig = new AppInitConfig(0, apps, initAppArgs, 0, 500, 0, 0);
        initAppsConf = new ArrayList<AppInitConfig>(1);
        initAppsConf.add(initAppConfig);

        Exception ex = null;
        try {
            vai.dsrcInitApps(initAppsVehs, initAppsConf, initAppsRandom1);
        }
        catch (Exception e) {
            ex = e;
        }
        assertTrue(ex != null);
    }

    private List<Class<?>> initAppAppsList() {
        List<Class<?>> ret = new ArrayList<Class<?>>(2);
        ret.add(TestConnectedVehicleAppImpl.class);
        ret.add(TestConnectedVehicleAppImpl.class);
        return ret;
    }

    private List<String[]> initAppsArgsList() {
        List<String[]> ret = new ArrayList<String[]>(2);
        ret.add(initAppExpected);
        ret.add(initAppExpected);
        return ret;
    }
}
