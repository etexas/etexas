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

import java.math.BigDecimal;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.junit.Test;

/**
 * @author janway
 */
public class LogDataTest {

    private static LogData ld1 = new LogData(1L, "aId1", BigDecimal.ZERO, "key1", "message1");

    private static LogData ld2 = new LogData(2L, "aId2", BigDecimal.ZERO, "key2", "message2");

    @Test
    public void testDefaultConstructor() {
        new LogData();
    }

    @Test
    public void testGetDeviceId() {
        assertTrue(ld1.getDeviceId() == 1L);
    }

    @Test
    public void testGetAppId() {
        assertTrue(ld1.getAppName().equals("aId1"));
    }

    @Test
    public void testGetSimTime() {
        assertTrue(ld1.getSimTime().equals(BigDecimal.ZERO));
    }

    @Test
    public void testGetKey() {
        assertTrue(ld1.getKey().equals("key1"));
    }

    @Test
    public void testGetMessage() {
        assertTrue(ld1.getMessage().equals("message1"));
    }

    @Test
    public void testToString() {
        assertTrue(ld1.toString().equals("Device Id: 1, App Name: aId1, Sim Time: 0.0, Key: key1, Message: message1"));
    }

    @Test
    public void testEquals() {
        assertTrue(ld1.equals(ld1));
        assertFalse(ld1.equals(ld2));
        assertFalse(ld1.equals(new Object()));
    }

    @Test
    public void testEquals2() {

        LogData logData = new LogData(ld1.getDeviceId(), ld1.getAppName(), ld1.getSimTime(), ld1.getKey(), ld1.getMessage());
        assertTrue(logData.equals(ld1));

        String diffMessage = ld1.getMessage() + "extra stuff";
        LogData logData2 = new LogData(ld1.getDeviceId(), ld1.getAppName(), ld1.getSimTime(), ld1.getKey(), diffMessage);
        assertFalse(logData2.equals(ld1));

        String diffKey = ld1.getKey() + "extra stuff";
        LogData logData3 = new LogData(ld1.getDeviceId(), ld1.getAppName(), ld1.getSimTime(), diffKey, ld1.getMessage());
        assertFalse(logData3.equals(ld1));

        BigDecimal diffTime = ld2.getSimTime();
        diffTime = diffTime.add(new BigDecimal(10));
        LogData logData4 = new LogData(ld1.getDeviceId(), ld1.getAppName(), diffTime, ld1.getKey(), ld1.getMessage());
        assertFalse(logData4.equals(ld1));

        String diffAppId = ld1.getAppName() + "extra stuff";
        LogData logData5 = new LogData(ld1.getDeviceId(), diffAppId, ld1.getSimTime(), ld1.getKey(), ld1.getMessage());
        assertFalse(logData5.equals(ld1));

        long diffDeviceId = ld1.getDeviceId() + 10;
        LogData logData6 = new LogData(diffDeviceId, ld1.getAppName(), ld1.getSimTime(), ld1.getKey(), ld1.getMessage());
        assertFalse(logData6.equals(ld1));
    }

    @Test
    public void testHashCode() {
        assertTrue(ld1.hashCode() == new HashCodeBuilder(431, 63).append(1L).hashCode());
    }

    @Test
    public void testToCsvString() {
        String execId = "execId";
        String correctCsvStr = String.format("%s,%d,%s,%s,%s,%s", execId, ld1.getDeviceId(), ld1.getAppName(), ld1.getSimTime().toString(), ld1.getKey(), ld1.getMessage());
        assertTrue(correctCsvStr.equals(ld1.toCsvString(execId)));
    }
}
