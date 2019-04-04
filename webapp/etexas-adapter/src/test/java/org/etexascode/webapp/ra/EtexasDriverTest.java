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
package org.etexascode.webapp.ra;

import static org.junit.Assert.fail;

import java.rmi.Remote;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import org.etexascode.api.eTEXAS;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ EtexasDriver.class, LocateRegistry.class, UnicastRemoteObject.class })
public class EtexasDriverTest {

    @Rule
    public ExpectedException ee = ExpectedException.none();

    private eTEXAS etexas;

    private EtexasDriver ed;

    @Before
    public void setUp() {
        ed = new EtexasDriver();
        etexas = PowerMockito.mock(eTEXAS.class);
        try {
            PowerMockito.whenNew(eTEXAS.class).withNoArguments().thenReturn(etexas);
            ed.init();
        }
        catch (Exception e) {}
    }

    @Test
    public void testMain() {
        String arg0 = "uuid";
        String arg1 = "9001";
        String[] args = { arg0, arg1 };

        Registry mockRegistry = PowerMockito.mock(Registry.class);

        try {
            PowerMockito.when(LocateRegistry.getRegistry(9001)).thenReturn(mockRegistry);
            PowerMockito.when(UnicastRemoteObject.exportObject(Mockito.any(Remote.class), Mockito.anyInt())).thenReturn(null);

            EtexasDriver.main(args);
        }
        catch (Exception e) {}
    }

    @Test
    public void testMain2() {
        String arg0 = "uuid";
        String arg1 = "9001";
        String[] args = { arg0, arg1 };

        try {
            PowerMockito.when(LocateRegistry.getRegistry(9001)).thenThrow(new RuntimeException());
            PowerMockito.when(UnicastRemoteObject.exportObject(Mockito.any(Remote.class), Mockito.anyInt())).thenReturn(null);
            EtexasDriver.main(args);
            fail("Expected to throw RuntimeException but did not.");
        }
        catch (Exception e) {
            if (!(e instanceof RuntimeException))
                fail("Expected to throw RuntimeException but did not.");
        }
    }

    @Test
    public void testInit() {
        try {
            ee.expect(RuntimeException.class);
            PowerMockito.whenNew(eTEXAS.class).withNoArguments().thenThrow(new RuntimeException());
        }
        catch (Exception e) {}

        try {
            ed.init();
        }
        catch (ClassNotFoundException e) {}
    }

    @Test
    public void testGetStaticData() {
        ed.getStaticData();
        Mockito.verify(etexas).getStaticData();
        Mockito.when(etexas.getStaticData()).thenThrow(new RuntimeException());
        ee.expect(RuntimeException.class);
        ed.getStaticData();
    }

    @Test
    public void testGetStepData() {
        ed.getStepData(5);
        Mockito.verify(etexas).getStepData(5);
        Mockito.when(etexas.getStepData(5)).thenThrow(new RuntimeException());
        ee.expect(RuntimeException.class);
        ed.getStepData(5);
    }

    @Test
    public void testClose() {
        ed.close();
    }

    @Test
    public void testAddVehicleCommand() {
        ed.addVehicleCommand(null);
        Mockito.verify(etexas).addVehicleCommand(null);
        Mockito.doThrow(new RuntimeException()).when(etexas).addVehicleCommand(null);
        ee.expect(RuntimeException.class);
        ed.addVehicleCommand(null);
    }

    @Test
    public void testAddSignalCommand() {
        ed.addSignalCommand(null);
        Mockito.verify(etexas).addSignalCommand(null);
        Mockito.doThrow(new RuntimeException()).when(etexas).addSignalCommand(null);
        ee.expect(RuntimeException.class);
        ed.addSignalCommand(null);
    }
    // TODO: ttevendale - tests fail in jenkins only, need to come back later and fix this

    // @Test
    // public void testCheckForErrorOutput() {
    // testCheckForOutputWithPath("/error-tests/error-test1", "Testing to see if an error is
    // caught", true);
    // testCheckForOutputWithPath("/error-tests/error-test2", "", true);
    // testCheckForOutputWithPath("/error-tests/error-test3", null, true);
    // try {
    //
    // assertEquals(null, ed.checkForErrorOutput(null));
    // }
    // catch (RemoteException e) {
    // fail("unexpected RemoteException");
    // }
    //
    // }
    //
    // @Test
    // public void testCheckForWarningOutput() {
    // testCheckForOutputWithPath("/warning-tests/warning-test1", "Testing to see if a warning is
    // caught", false);
    // testCheckForOutputWithPath("/warning-tests/warning-test2", "", false);
    // testCheckForOutputWithPath("/warning-tests/warning-test3", null, false);
    // try {
    //
    // assertEquals(null, ed.checkForErrorOutput(null));
    // }
    // catch (RemoteException e) {
    // fail("unexpected RemoteException");
    // }
    // }
    //
    // private void testCheckForOutputWithPath(String path, String correct, boolean error) {
    //
    // URL test = EtexasDriverTest.class.getResource(path);
    // String output = null;
    // //This will be null if the path doesn't exist
    // if (test != null) {
    // String str = test.toString();
    // //URL puts file:/ at the beginning of the path
    // str = str.substring(6);
    // try {
    //
    // if (error) {
    // output = ed.checkForErrorOutput(str);
    // }
    // else {
    // output = ed.checkForWarningOutput(str);
    // }
    // }
    // catch (RemoteException e) {
    // fail("unexpected Remote Exception");
    // }
    // }
    // assertEquals(correct, output);
    // }

}
