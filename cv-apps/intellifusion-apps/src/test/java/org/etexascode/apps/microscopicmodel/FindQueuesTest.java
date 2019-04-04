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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.test.GenLaneFunctions;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 * @author cdeisher
 */
public class FindQueuesTest {

    Lane queueLane = null;

    ILane queueILane = null;

    List<IVehicle> queueThisStep1 = null;

    ILane getQueue5 = null;

    IVehicleManager toFilter = null;

    Map<Integer, List<IVehicle>> filtered = null;

    List<IVehicle> chopListSmall = null;

    List<IVehicle> chopListBig = null;

    List<IVehicle> queueQueueSpeed = null;

    ILaneManager carsInQueueLanMan = null;

    @Before
    public void setup() {
        queueLane = genQueueLane();
        queueILane = queueLane;
        queueThisStep1 = genQueueThisStep1();
        for (IVehicle v : queueThisStep1) {
            if (v instanceof Vehicle) {
                ((Vehicle)v).setSpeed(0.0);
                ((Vehicle)v).setAcceleration(0.0);
            }
        }
        toFilter = genToFilter();
        filtered = genFiltered();
        chopListSmall = genListSmall();
        chopListBig = genListBig();
        carsInQueueLanMan = genCarsInQueueLanMan();
        queueQueueSpeed = genQueueWithSpeed();
        getQueue5 = genQueueLane5();
    }

    @After
    public void teardown() {
        queueLane = null;
        queueILane = null;
        queueThisStep1 = null;
        toFilter = null;
        filtered = null;
        chopListSmall = null;
        chopListBig = null;
        carsInQueueLanMan = null;
        queueQueueSpeed = null;
        getQueue5 = null;
    }

    @Test
    public void testGetCarsInQueue() {

    }

    @Test
    public void testGetQueue1() {
        assertEquals(new ArrayList<IVehicle>(0), FindQueues.getQueue(null, null, null));
    }

    @Test
    public void testGetQueue2() {
        assertEquals(new ArrayList<IVehicle>(0), FindQueues.getQueue(queueThisStep1, null, queueILane));
    }

    @Test
    public void testGetQueue3() {
        assertEquals(chopListSmall, FindQueues.getQueue(chopListSmall, chopListSmall, queueILane));
    }

    @Test
    public void testGetQueue4() {
        assertEquals(new ArrayList<IVehicle>(0), FindQueues.getQueue(queueQueueSpeed, null, getQueue5));
    }

    @Test
    public void testFilterByLane() {
        assertEquals(filtered, FindQueues.filterByLane(toFilter));
    }

    @Test
    public void testChopList1() {
        assertEquals(chopListSmall, FindQueues.chopList(chopListBig, 3));
    }

    @Test
    public void testChopList2() {
        assertEquals(new ArrayList<IVehicle>(0), FindQueues.chopList(chopListBig, 0));
    }

    private IVehicleManager genToFilter() {
        VehicleManager vm = new VehicleManager();

        vm.addVehicle(GenVehicleFunctions.genVehicle(0.0, 1.0, 2.0, 3.0, 4, 5));
        vm.addVehicle(GenVehicleFunctions.genVehicle(6.0, 7.0, 8.0, 9.0, 4, 10));
        vm.addVehicle(GenVehicleFunctions.genVehicle(11.0, 12.0, 13.0, 14.0, 5, 15));

        LaneManager lm = new LaneManager();
        lm.setGeoCalculatorType(1);
        return vm;
    }

    private Map<Integer, List<IVehicle>> genFiltered() {
        Map<Integer, List<IVehicle>> ret = new HashMap<Integer, List<IVehicle>>();

        IVehicle vi1 = GenVehicleFunctions.genVehicle(0.0, 1.0, 2.0, 3.0, 4, 5);
        IVehicle vi2 = GenVehicleFunctions.genVehicle(6.0, 7.0, 8.0, 9.0, 4, 10);
        IVehicle vi3 = GenVehicleFunctions.genVehicle(11.0, 12.0, 13.0, 14.0, 5, 15);

        List<IVehicle> lst4 = new ArrayList<IVehicle>(2);
        lst4.add(vi1);
        lst4.add(vi2);

        List<IVehicle> lst5 = new ArrayList<IVehicle>(1);
        lst5.add(vi3);

        ret.put(4, lst4);
        ret.put(5, lst5);

        return ret;
    }

    private List<IVehicle> genListSmall() {
        List<IVehicle> ret = new LinkedList<IVehicle>();

        ret.add(GenVehicleFunctions.genVehicle(0.0, 1.0, 2.0, 3.0, 4, 5));
        ret.add(GenVehicleFunctions.genVehicle(6.0, 7.0, 8.0, 9.0, 4, 10));
        ret.add(GenVehicleFunctions.genVehicle(11.0, 12.0, 13.0, 14.0, 5, 15));

        return ret;
    }

    private List<IVehicle> genListBig() {
        List<IVehicle> ret = genListSmall();

        ret.add(GenVehicleFunctions.genVehicle(16.0, 17.0, 18.0, 19.0, 6, 20));
        ret.add(GenVehicleFunctions.genVehicle(21.0, 22.0, 23.0, 24.0, 7, 25));

        return ret;
    }

    private Lane genQueueLane() {
        return GenLaneFunctions.genLane(1, 0);
    }

    private List<IVehicle> genQueueThisStep1() {
        List<IVehicle> ret = new ArrayList<IVehicle>(3);

        ret.add(GenVehicleFunctions.genVehicle(0.0, 0.0, 2.0, 3.0, 4, 5));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 7.0, 8.0, 9.0, 4, 10));
        ret.add(GenVehicleFunctions.genVehicle(0.0, 12.0, 13.0, 14.0, 4, 15));

        return ret;
    }

    private ILaneManager genCarsInQueueLanMan() {
        LaneManager ret = new LaneManager();

        Lane l = new Lane();
        l.setLaneId(4);
        ret.getLanes().put(4, l);

        l = new Lane();
        l.setLaneId(5);
        ret.getLanes().put(5, l);

        return ret;
    }

    private List<IVehicle> genQueueWithSpeed() {
        List<IVehicle> ret = new ArrayList<IVehicle>(1);

        Vehicle v = GenVehicleFunctions.genVehicle(0.0, 0.0, 2.0, 3.0, 4, 5);
        v.setSpeed(5);
        ret.add(v);

        return ret;
    }

    private ILane genQueueLane5() {
        Lane l = new Lane();
        l.getLaneGeomList().add(new LaneNode());
        return l;
    }
}
