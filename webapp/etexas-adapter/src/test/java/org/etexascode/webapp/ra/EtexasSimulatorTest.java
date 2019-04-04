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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.etexascode.interrep.datamodel.RemoteSimulatorInterface;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;

@RunWith(PowerMockRunner.class)
public class EtexasSimulatorTest {

    /*
     * @Test public void testEtexasSimulator() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); new EtexasSimulator(mockMc); }
     * @Test public void testInvalidate() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); es.invalidate(); assertNull(Whitebox.getInternalState(es, "mc")); }
     * @Test public void testAssociateConnection() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasManagedConnection mockMc2 =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); es.associateConnection(mockMc2); assertEquals(mockMc2,
     * Whitebox.getInternalState(es, "mc")); }
     * @Test public void testClose() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); es.close(); Mockito.verify(mockMc).closeHandle(es); es.close(); }
     * @Test public void testGetStaticData() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); RemoteSimulatorInterface mockEi =
     * PowerMockito.mock(RemoteSimulatorInterface.class);
     * Mockito.when(mockMc.getSimDriver()).thenReturn(mockEi); try { es.getStaticData();
     * Mockito.verify(mockEi).getStaticData(); } catch (Exception e) { } }
     * @Test public void testGetStepData() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); RemoteSimulatorInterface mockEi =
     * PowerMockito.mock(RemoteSimulatorInterface.class);
     * Mockito.when(mockMc.getSimDriver()).thenReturn(mockEi); try { es.getStepData(5);
     * Mockito.verify(mockEi).getStepData(5); } catch (Exception e) { } }
     * @Test public void testUpdateVehicleData() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); RemoteSimulatorInterface mockEi =
     * PowerMockito.mock(RemoteSimulatorInterface.class);
     * Mockito.when(mockMc.getSimDriver()).thenReturn(mockEi); try { es.updateVehicleData(null);
     * Mockito.verify(mockEi).updateVehicleData(null); } catch (Exception e) { } }
     * @Test public void testUpdateSignalData() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); RemoteSimulatorInterface mockEi =
     * PowerMockito.mock(RemoteSimulatorInterface.class);
     * Mockito.when(mockMc.getSimDriver()).thenReturn(mockEi); try { es.updateSignalData(null);
     * Mockito.verify(mockEi).updateSignalData(null);; } catch (Exception e) { } }
     * @Test public void testAddVehicleCommand() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); RemoteSimulatorInterface mockEi =
     * PowerMockito.mock(RemoteSimulatorInterface.class);
     * Mockito.when(mockMc.getSimDriver()).thenReturn(mockEi); try { es.addVehicleCommand(null);
     * Mockito.verify(mockEi).addVehicleCommand(null); } catch (Exception e) { } }
     * @Test public void testAddSignalCommand() { EtexasManagedConnection mockMc =
     * PowerMockito.mock(EtexasManagedConnection.class); EtexasSimulator es = new
     * EtexasSimulator(mockMc); RemoteSimulatorInterface mockEi =
     * PowerMockito.mock(RemoteSimulatorInterface.class);
     * Mockito.when(mockMc.getSimDriver()).thenReturn(mockEi); try { es.addSignalCommand(null);
     * Mockito.verify(mockEi).addSignalCommand(null); } catch (Exception e) { } }
     */

}
