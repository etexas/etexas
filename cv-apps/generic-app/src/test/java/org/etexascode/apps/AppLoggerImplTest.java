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

import static org.junit.Assert.assertTrue;

import java.util.List;

import org.etexascode.devicedata.LogData;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class AppLoggerImplTest {

    long devId = 1;

    String appName;

    double time = 0.5;

    @Before
    public void setUp() throws Exception {
        appName = "appName";
    }

    @After
    public void tearDown() throws Exception {
        appName = null;
    }

    @Test
    public void testConstructor() {

        AppLoggerImpl appLogger = new AppLoggerImpl(devId, appName, time);

        assertTrue(appLogger.deviceId == devId);
        assertTrue(appLogger.appName == appName);
        assertTrue(appLogger.simTime.doubleValue() == time);
    }

    @Test
    public void testLog() {

        AppLoggerImpl appLogger = new AppLoggerImpl(devId, appName, time);

        String key = "key";
        String message = "message";
        appLogger.log(key, message);

        List<LogData> logs = appLogger.data;
        assertTrue(logs.size() == 1);

        LogData log = logs.get(0);
        assertTrue(log.getDeviceId() == devId);
        assertTrue(log.getAppName() == appName);
        assertTrue(log.getSimTime().doubleValue() == time);
        assertTrue(log.getKey() == key);
        assertTrue(log.getMessage() == message);
    }
}