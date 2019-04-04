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
package org.etexascode.apps.dcs;

import static org.junit.Assert.assertTrue;

import java.util.LinkedList;
import java.util.List;

import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.reflect.Whitebox;

/**
 * Tests the DCS Algorithm class.
 * 
 * @author jrutherford
 */
public class DCSAlgorithmTest {

    /** The DCS Algorithm. */
    private DCSAlgorithm algorithm;

    /** The RSE device. */
    private RSEDevice device;

    /** The interRep info model. */
    private InterRepInfoModel irim;

    private ISignalManager mockSignalManager = PowerMockito.mock(ISignalManager.class);

    private List<ISignalIndication> sigs;

    @Before
    public void setup() {
        SignalIndication sig = new SignalIndication();
        sig.setColorIndication(SignalIndication.Color.GREEN);
        sigs = new LinkedList<ISignalIndication>();
        sigs.add(sig);
        device = PowerMockito.mock(RSEDevice.class);
        irim = PowerMockito.mock(InterRepInfoModel.class);

        ILaneManager laneManager = new LaneManager();
        PowerMockito.when(irim.getLmi()).thenReturn(laneManager);
        algorithm = new DCSAlgorithm(device, irim, PowerMockito.mock(AppLogger.class), new int[] { 12 });
    }

    @Test
    public void testConstructor() {
        assertTrue(Whitebox.getInternalState(algorithm, "vehicleStatusComponent") != null);
        assertTrue(Whitebox.getInternalState(algorithm, "phaseStatusComponent") != null);
        assertTrue(Whitebox.getInternalState(algorithm, "signalController") != null);
    }

    @Test
    public void testUpdateManagers() {
        algorithm.updateManagers(irim, device);
    }

    @Test
    public void testConfigureTimeouts() {
        algorithm.configureTimeouts(20.4, 32.1);
        PhaseStatusComponent psc = Whitebox.getInternalState(algorithm, "phaseStatusComponent");
        assertTrue((Double)Whitebox.getInternalState(psc, "stage1Timeout") == 20.4);
        assertTrue((Double)Whitebox.getInternalState(psc, "stage2Timeout") == 32.1);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testPerformDetectionControlLoop() {
        VehicleStatusComponent vsc = PowerMockito.mock(VehicleStatusComponent.class);
        PhaseStatusComponent psc = PowerMockito.mock(PhaseStatusComponent.class);

        Whitebox.setInternalState(algorithm, "vehicleStatusComponent", vsc);
        Whitebox.setInternalState(algorithm, "phaseStatusComponent", psc);

        PowerMockito.doReturn(mockSignalManager).when(device).getSignalManager(0);
        Mockito.doReturn(sigs).when(mockSignalManager).getSignalsByLaneId(12);

        algorithm.performDetectionControlLoop(30.5);
        Mockito.verify(psc, Mockito.times(1)).performControl(Mockito.anyDouble(), Mockito.anyCollection());
    }
}