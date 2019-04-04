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

import java.util.ArrayList;
import java.util.List;

import org.etexascode.apps.RSEDevice;
import org.etexascode.apps.dcs.model.SignalController;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.reflect.Whitebox;

/**
 * Tests the signal controller class.
 * 
 * @author jrutherford
 */
public class SignalControllerTest {

    /** The signal controller. */
    private SignalController controller;

    /** RSE Device. */
    private RSEDevice device;

    @Before
    public void setup() {
        SignalIndication si1 = new SignalIndication();
        si1.setColorIndication(Color.GREEN);
        si1.setLaneId(17);
        si1.setStateIndication(State.STEADY);
        si1.setTimeToChange(30);
        si1.setTypeIndication(Type.BALL);
        List<SignalIndication> sigInds = new ArrayList<SignalIndication>();
        sigInds.add(si1);
        SignalManager sigMan = new SignalManager();
        sigMan.addSignals(sigInds);

        device = PowerMockito.mock(RSEDevice.class);
        PowerMockito.doReturn(sigMan).when(device).getSignalManager(0);
        controller = new SignalControllerImpl(0, device, null);
    }

    @Test
    public void testUpdateDevice() {
        RSEDevice dev2 = PowerMockito.mock(RSEDevice.class);
        controller.updateDevice(dev2);
        assertTrue(Whitebox.getInternalState(controller, "device") == dev2);
    }

    @Test
    public void testChangePhase() {
        controller.changePhase();
        Mockito.verify(device, Mockito.times(1)).addSignalCommand(Mockito.any(SignalCommand.class));
    }

    @Test
    public void testHoldPhase() {
        controller.holdPhase();
        Mockito.verify(device, Mockito.times(1)).addSignalCommand(Mockito.any(SignalCommand.class));
    }

    @Test
    public void testIsHoldingGreen() {
        assertTrue(controller.isHoldingGreen(new int[] { 17 }));

        SignalIndication si1 = new SignalIndication();
        si1.setColorIndication(Color.RED);
        si1.setLaneId(17);
        si1.setStateIndication(State.STEADY);
        si1.setTimeToChange(30);
        si1.setTypeIndication(Type.BALL);
        List<SignalIndication> sigInds = new ArrayList<SignalIndication>();
        sigInds.add(si1);
        SignalManager sigMan = new SignalManager();
        sigMan.addSignals(sigInds);

        PowerMockito.doReturn(sigMan).when(device).getSignalManager(0);

        assertTrue(!controller.isHoldingGreen(new int[] { 17 }));
    }
}