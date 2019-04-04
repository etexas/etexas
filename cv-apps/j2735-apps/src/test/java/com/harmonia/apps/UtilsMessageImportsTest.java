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
package com.harmonia.apps;

import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Map;

import org.etexascode.apps.MapDataUtil;
import org.etexascode.apps.UtilsMessageImports;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.SPAT;
import org.etexascode.test.GenLaneFunctions;
import org.etexascode.test.GenSignalFunctions;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class UtilsMessageImportsTest {

    LaneManager manager = GenLaneFunctions.genLaneManager();

    SignalManager actualSignalManager = GenSignalFunctions.genSignalManager();

    SignalManager expectedSignalManager = new SignalManager();

    LaneManager newLaneManager = GenLaneFunctions.genLaneManager();

    List<Lane> laneVectorFromMD = null;

    List<Lane> importedLanes = null;

    MapData middle;

    double[] latlong = { 0.0, 0.0 };

    double delta = 0.00001;

    BasicSafetyMessage bsm = null;

    BasicSafetyMessageVerbose bsmv = null;

    Vehicle v = GenVehicleFunctions.genVehicle(-1600, 26, 180, 80, 4, 2);

    Vehicle importedBSMVehicle = null;

    // VehicleInfo info = new VehicleInfo(v, 35, 35, 1);

    double simtime = 0.5;

    @Before
    public void setUp() throws Exception {
        manager.setLatitude(35);
        manager.setLongitude(35);
        double[] latLon = UtilsLatLongConversion.convertCentimeterOffsetToLatLong(v.getX(), v.getY(), manager.getLatitude(), manager.getLongitude(), manager.getGeoCalculatorType());
        manager.setElevation(0.0);
        v.setLatitude(latLon[0]);
        v.setLongitude(latLon[1]);
        v.setHeading(0.0);
        v.setSpeed(0.0);
        v.setAcceleration(0.0);
        v.setElev(0.0);

        /** Create bsm and bsmv and set lane manager latitude and longitude */
        bsm = new BasicSafetyMessage();
        BSMProducerApp.getBasicSafetyMessage(simtime, v, false, (short)0, bsm);
        bsmv = new BasicSafetyMessageVerbose();
        bsmv.setId("2      ".getBytes());
        bsmv.setElev("0.0000".getBytes());
        bsmv.setAccuracy("000000".getBytes());
        bsmv.setAngle("00".getBytes());
        bsmv.setBrakes("000".getBytes());
        BSMVerboseProducerApp.getBasicSafetyMessageVerbose(0.5, v, false, (short)0, bsmv);// ,
        // false,
        // (short)0);

        Map<Integer, Lane> lanes = newLaneManager.getLanes();
        for (int i = 0; i < 30; i++) {
            if (lanes.containsKey(i)) {
                if (lanes.get(i).getApproachId() == 0) {
                    lanes.get(i).setApproachId(5);
                }
            }
        }
        middle = MapDataProducerApp.createFormattedMapDataMessage(newLaneManager, (short)0, null, newLaneManager.getLatitude(), newLaneManager.getLongitude());
        laneVectorFromMD = MapDataUtil.getLaneVectorsFromMapData(middle);
        importedLanes = UtilsMessageImports.importLanes(middle);

        // SignalManagerInfo info = new SignalManagerInfo(actualSignalManager);
        SPATProducerApp spa = new SPATProducerApp();
        spa.getFormattedSPATMessage(0, actualSignalManager);
        SPAT message = spa.spatMessage;
        List<SignalIndication> signals = UtilsMessageImports.importSignal(message);
        // Waiting for Signal module changes
        for (SignalIndication s : signals) {
            expectedSignalManager.addSignal(s);
        }

    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testConvertBSMToVehicle() {
        v.setHeading(0.0);
        importedBSMVehicle = UtilsMessageImports.importBSM(bsm, manager, manager.getGeoCalculatorType());
        importedBSMVehicle.setVehicleID(v.getVehicleID());
        importedBSMVehicle.setAcceleration(0.0);
        assertEquals(v, importedBSMVehicle);
    }

    @Test
    public void testImportLanes() {
        assertEquals(laneVectorFromMD, importedLanes);
    }

    @Test
    public void testImportLatLong() {
        double[] d = UtilsMessageImports.importLatLong(middle);
        assertEquals(d[0], latlong[0], delta);
        assertEquals(d[1], latlong[1], delta);
    }

    @Test
    public void testImportSignal() {
        assertEquals(actualSignalManager, expectedSignalManager);
    }

}
