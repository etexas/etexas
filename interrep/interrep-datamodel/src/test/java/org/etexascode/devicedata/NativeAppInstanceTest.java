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
package org.etexascode.devicedata;

import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ttevendale
 */
public class NativeAppInstanceTest {

    @Before
    public void setup() {}

    @After
    public void teardown() {}

    @Test
    public void testInit() {
        NativeApp nai = new NativeApp();
        String[] appConfigs = new String[4];
        appConfigs[0] = "appId";
        appConfigs[1] = "commandLine";
        appConfigs[2] = "hostName";
        appConfigs[3] = "25";
        nai.init(appConfigs);

        assertTrue(appConfigs[0].equals(nai.getAppName()));
        assertTrue(appConfigs[1].equals(nai.getCommandLine()));
        assertTrue(appConfigs[2].equals(nai.getHostName()));
        assertTrue(Integer.parseInt(appConfigs[3]) == nai.getPortNumber());
    }
}
