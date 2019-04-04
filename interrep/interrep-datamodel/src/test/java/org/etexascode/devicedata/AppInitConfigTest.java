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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * @author janway
 */
public class AppInitConfigTest {

    private static final int min = 10;

    private static final int max = 50;

    private static final int minNumDevices = 2;

    private static final int maxNumDevices = 3;

    private static AppInitConfig aic = new AppInitConfig(0, null, null, min, max, minNumDevices, maxNumDevices);;

    @Test
    public void testConstructor() {
        assertTrue(aic.appDefs == null);
        assertTrue(aic.configs == null);
        assertTrue(aic.min == min);
        assertTrue(aic.max == max);
        assertTrue(aic.minNumDevices == minNumDevices);
        assertTrue(aic.maxNumDevices == maxNumDevices);
    }

    @Test
    public void testIsInRange() {
        assertTrue(aic.isInRange(30));
        assertFalse(aic.isInRange(-40));
        assertTrue(aic.isInRange(min));
        assertTrue(aic.isInRange(max));
    }

}
