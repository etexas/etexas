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
package org.etexascode.test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.commons.io.FileUtils;
import org.etexascode.datalayer.IDataLayer;
import org.etexascode.datalayer.inmemory.DefaultVehicleIdComponent;
import org.etexascode.datalayer.inmemory.SingleIntersectionDataLayer;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.OBUDeviceData;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.devicedata.ReportDeviceData;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.webapp.playback.PlaybackSimulator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Test setup for message system. The class uses Playback Simulator dat. It sets configs for
 * AppConfig and creates List of AppConfigs for the device initialization, and puts apps on the
 * device. It includes gettters for the four data managers, DetectorManager,
 * LaneManager,SignalManager,and VehicleManager.
 *
 * @author bmauldon
 */
public class TestHarnessCase {

    /** Set frequency base number for apps */
    public static final String[] BMSV_frequency = new String[] { "0.1" };

    public static final String[] CSRP_frequency = new String[] { "0.2" };

    public static final String[] BMS_frequency = new String[] { "0.3" };

    public static final String[] MapData_frequency = new String[] { "0.5" };

    public static final String[] SPAT_frequency = new String[] { "0.7" };

    /** Static logger */
    private static final Logger LOGGER = LoggerFactory.getLogger(TestHarnessCase.class);

    /** Playback directory and temp file */
    File playbackFile = null;

    public static final Random random = new Random();

    public TestHarnessCase() {}

    /**
     * creates test InterRep from playback data
     * 
     * @return DetectorManager
     */
    public DetectorManager getDetectorManager() {
        DetectorManager dm = null;
        try {
            TestInterRep testIntRep = getTestInterRep(true);
            dm = testIntRep.getDetectorManager();
            // LOGGER.debug("Detector Manager :" +dm.toString() );
        }
        catch (TestException e) {
            LOGGER.debug("Can't generate test interrep");
        }

        return dm;
    }

    /**
     * creates test InterRep from playback data
     * 
     * @return LaneManager
     */
    public LaneManager getLaneManager() {
        LaneManager lm = null;
        try {
            TestInterRep testIntRep = getTestInterRep(true);
            lm = testIntRep.getLaneManager();
            // LOGGER.debug("Lane Manager :" +lm.toString() );
        }
        catch (TestException e) {
            LOGGER.debug("Can't generate test interrep");
        }
        return lm;
    }

    /**
     * creates test InterRep from playback data
     * 
     * @return SignalManager
     */
    public SignalManager getSignalManager() {
        SignalManager sm = null;
        try {
            TestInterRep testIntRep = getTestInterRep(true);
            sm = testIntRep.getSignalManager();
            // LOGGER.debug("Signal Manager :" +sm.toString() );
        }
        catch (TestException e) {

            LOGGER.debug("Can't generate test interrep");
        }
        return sm;
    }

    /**
     * @param appList The app list.
     * @param vehId The vehicle ID.
     * @return DeviceEmulatorOBU
     */
    public OBUDeviceData getTestDeviceOBU(List<IConnectedVehicleApp<?>> appList, String vehId) {
        return new OBUDeviceData(0, appList, vehId);
    }

    /**
     * @param appList The app list.
     * @param intId the integer ID.
     * @return DeviceEmulatorReport
     */
    public ReportDeviceData getTestDeviceReport(List<IConnectedVehicleApp<?>> appList, int intId) {
        return new ReportDeviceData(appList, new int[] { intId });

    }

    /**
     * Get test Roadside Equipment device
     * 
     * @param apps The apps.
     * @param points The points.
     * @param intersectionId The intersection ID.
     * @param mac The mac address.
     * @param x The x coordinate.
     * @param y The y coordinate.
     * @param z The z coordiante.
     * @return RSEDeviceData
     */
    public RSEDeviceData getTestDeviceRSE(List<IConnectedVehicleApp<?>> apps, ReferencePoint[] points, int intersectionId, long mac, int x, int y, int z) {
        return new RSEDeviceData(mac, apps, points, new int[] { intersectionId }, x, y, z);
    }

    /**
     * an InterRep object created using playback data from a previous simulation
     * 
     * @param update The update status.
     * @return TestInterRep
     * @throws TestException If a test exception occurs.
     */
    public TestInterRep getTestInterRep(boolean update) throws TestException {

        try {

            playbackFile = FileUtils.toFile(this.getClass().getResource("/playbackoutput"));
            LOGGER.debug("File exists : " + playbackFile.exists());
            PlaybackSimulator psi = new PlaybackSimulator(playbackFile);
            IDataLayer idl = new SingleIntersectionDataLayer(new ArrayList<IDeviceData>(0), null, 0, new DefaultVehicleIdComponent());
            TestInterRep pbInterRep = new TestInterRep(psi, null, idl);
            pbInterRep.getLaneManager().setGeoCalculatorType(1);
            LOGGER.debug("TestInterRep sucessfully created");
            // TODO Add SignalManager - it's null
            if (update) {
                pbInterRep.update();
                LOGGER.info(pbInterRep.getVehicleManager().toString());
            }

            return pbInterRep;

        }
        catch (NullPointerException e) {
            LOGGER.debug("Playback folders were not found. Path :{} -- this is possibly misleading, be sure to check the stack trace of the null pointer that caused this", playbackFile.getPath());
            e.printStackTrace(System.out);
            throw new TestException("The playback data folders were not found -- this is possibly misleading, be sure to check the stack trace of the null pointer that caused this", e);
        }
        catch (Exception e) {
            LOGGER.debug("Could not create TestInterRep -- this is possibly misleading, be sure to check the stack trace of the exception that caused this");
            throw new TestException("TestInterRep not created -- this is possibly misleading, be sure to check the stack trace of the exception that caused this", e);
        }
    }

    /**
     * creates test InterRep from playback data
     * 
     * @return VehicleManager
     */
    public VehicleManager getVehicleManager() {
        VehicleManager vm = null;
        try {
            TestInterRep testIntRep = getTestInterRep(true);
            vm = testIntRep.getVehicleManager();
            // LOGGER.debug("Vehicle Manager :" +vm.toString() );
        }
        catch (TestException e) {
            LOGGER.debug("Can't generate test interrep");
        }
        return vm;
    }
}
