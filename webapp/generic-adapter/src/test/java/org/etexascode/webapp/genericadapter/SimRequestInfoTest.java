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

package org.etexascode.webapp.genericadapter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class SimRequestInfoTest {

    @SuppressWarnings("rawtypes")
    Class testClass = null;

    String uuid = null;

    SimRequestInfo sri = null;

    Map<String, Object> confs = null;

    @Before
    public void setUp() {
        uuid = "uuid";
        testClass = TestSimInterface.class;
        confs = new HashMap<String, Object>();
        sri = new SimRequestInfo(uuid, testClass, confs);
    }

    @After
    public void tearDown() {
        uuid = null;
        testClass = null;
        sri = null;
        confs = null;
    }

    @Test
    public void testConstructor() {
        SimRequestInfo sri = new SimRequestInfo("blahs", TestSimInterface.class, new HashMap<String, Object>());
        assertTrue(sri instanceof SimRequestInfo);
    }

    @Test
    public void testEquals1() {
        assertFalse(sri.equals(""));
    }

    @Test
    public void testEquals2() {
        SimRequestInfo sri2 = new SimRequestInfo(uuid, testClass, confs);
        assertEquals(sri, sri2);
    }

    @Test
    public void testEquals3() {
        SimRequestInfo sri2 = new SimRequestInfo(uuid + "not", testClass, confs);
        assertFalse(sri.equals(sri2));
    }

    @Test
    public void testEquals4() {
        SimRequestInfo sri2 = new SimRequestInfo(uuid, TestSimInterface2.class, confs);
        assertFalse(sri.equals(sri2));
    }

    @Test
    public void testHashCode() {
        int code = "".hashCode() + uuid.hashCode() + testClass.hashCode() + confs.hashCode();
        assertEquals(code, sri.hashCode());
    }

    @Test
    public void testGetSetUuid() {
        sri.setUuid("blahs");
        assertEquals("blahs", sri.getUuid());
    }

    @Test
    public void testGetSetSimClass() {
        sri.setSimClass(TestSimInterface2.class);
        assertEquals(TestSimInterface2.class, sri.getSimClass());
    }

    @Test
    public void testGetSetConfs() {
        Map<String, Object> testConfs = new HashMap<String, Object>();
        testConfs.put("key1", "value1");
        sri.setConfs(testConfs);
        assertEquals(testConfs, sri.getConfs());
    }

    @Test
    public void testCheckUuid1() {
        assertEquals("Generic uuid", SimRequestInfo.checkUuid(null));
    }

    @Test
    public void testCheckUuid2() {
        assertEquals("huzzah", SimRequestInfo.checkUuid("huzzah"));
    }

    @Test
    public void testCheckConfs1() {
        assertEquals(new HashMap<String, Object>(), SimRequestInfo.checkConfs(null));
    }

    @Test
    public void testCheckConfs2() {
        Map<String, Object> x1 = new HashMap<String, Object>();
        assertTrue(x1 == SimRequestInfo.checkConfs(x1));
    }

    @Test(expected = RuntimeException.class)
    public void testCheckSimClass1() {
        SimRequestInfo.checkSimClass(null);
    }

    @Test(expected = RuntimeException.class)
    public void testCheckSimClass2() {
        SimRequestInfo.checkSimClass(Object.class);
    }

    @Test
    public void testCheckSimClass3() {
        try {
            SimRequestInfo.checkSimClass(null);
        }
        catch (RuntimeException e) {
            assertEquals("SimClass cannot be null", e.getMessage());
        }
    }

    @Test
    public void testCheckSimClass4() {
        try {
            SimRequestInfo.checkSimClass(Object.class);
        }
        catch (RuntimeException e) {
            assertEquals("SimClass must implement the SimulatorInterface", e.getMessage());
        }
    }
}
