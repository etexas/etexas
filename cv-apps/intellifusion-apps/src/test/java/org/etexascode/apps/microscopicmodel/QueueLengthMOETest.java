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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.test.GenVehicleFunctions;
import org.etexascode.test.TestAppLoggerReadOutput;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class QueueLengthMOETest {

    ILaneManager queueLanMan = null;

    Map<Integer, List<IVehicle>> queues = null;

    TestAppLoggerReadOutput testLogger = null;

    Map<Integer, Double> logQueues = null;

    @Before
    public void setup() {
        queueLanMan = genLanMan();
        queues = genQueues();
        logQueues = genLogQueues();
        testLogger = new TestAppLoggerReadOutput();
    }

    @After
    public void teardown() {
        logQueues = null;
        testLogger = null;
        queueLanMan = null;
        queues = null;
    }

    @Test
    public void testConstructor() {
        QueueLengthMOE q = new QueueLengthMOE();
        assertTrue(q instanceof QueueLengthMOE);
    }

    @Test
    public void testGetQueueLengths() {
        Map<Integer, Double> actual = QueueLengthMOE.getQueueLengths(queues, queueLanMan);
        assertEquals(5.0, actual.get(0), 0.05);
        assertEquals(0.0, actual.get(1), 0.05);
    }

    @Test
    public void testWriteLogs() {
        QueueLengthMOE.writeLogs(logQueues, testLogger);
        assertTrue(containsKeyValue(testLogger, "Queue Length Lane Id = 0", "42.0"));
        assertTrue(containsKeyValue(testLogger, "Queue Length Lane Id = 1", "97.0"));
    }

    private ILaneManager genLanMan() {
        LaneManager lm = new LaneManager();

        Lane l = new Lane();
        LaneNode ln = new LaneNode();
        ln.setX(0.0);
        ln.setY(0.0);
        List<LaneNode> lst = new ArrayList<LaneNode>(1);
        lst.add(ln);
        l.setLaneGeomList(lst);
        lm.getLanes().put(0, l);
        l = new Lane();
        l.setLaneId(1);

        return lm;
    }

    private Map<Integer, List<IVehicle>> genQueues() {
        Map<Integer, List<IVehicle>> ret = new HashMap<Integer, List<IVehicle>>();

        IVehicle vi = GenVehicleFunctions.genVehicle(5, 0, 0, 0, 0, 1);
        List<IVehicle> q = new ArrayList<IVehicle>(1);
        q.add(vi);
        ret.put(0, q);
        ret.put(1, new ArrayList<IVehicle>(0));

        return ret;
    }

    private Map<Integer, Double> genLogQueues() {
        Map<Integer, Double> ret = new HashMap<Integer, Double>();

        ret.put(0, 42.0);
        ret.put(1, 97.0);

        return ret;
    }

    private boolean containsKeyValue(TestAppLoggerReadOutput logger, String key, String value) {
        for (String[] log : logger.logs) {
            if (log[0].equals(key) && log[1].equals(value)) {
                return true;
            }
        }

        return false;
    }
}
