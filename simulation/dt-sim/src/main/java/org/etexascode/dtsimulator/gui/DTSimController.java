/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2013 Harmonia Holdings Group LLC            * ***
 *** *                                                            * ***
 *** * Permission is hereby granted to use, modify, copy, and     * ***
 *** * distribute this software and its documentation for any     * ***
 *** * purpose only without profit, provided that the above       * ***
 *** * Copyright Notice appears in all copies and that both the   * ***
 *** * Copyright Notice and this Permission Notice appears in     * ***
 *** * every copy of supporting documentation.  No title to nor   * ***
 *** * ownership of the software is transferred hereby.  The name * ***
 *** * of Harmonia Holdings Group LLC shall not be used in        * ***
 *** * advertising or publicity related to the distribution of    * ***
 *** * the software without specific, written, prior permission.  * ***
 *** * This software is provided as-delivered without expressed   * ***
 *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
 *** * makes no representation about the suitability of this      * ***
 *** * software for any purpose and accepts no responsibility for * ***
 *** * its use.                                                   * ***
 *** *                                                            * ***
 *** ************************************************************** ***
 *** *                                                            * ***
 *** * This program is free software; you can redistribute it     * ***
 *** * and/or modify it under the terms of the GNU General Public * ***
 *** * License as published by the Free Software Foundation;      * ***
 *** * either version 2 of the License, or (at your option) any   * ***
 *** * later version.                                             * ***
 *** *                                                            * ***
 *** * This program is distributed in the hope that it will be    * ***
 *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
 *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
 *** * PURPOSE.  See the GNU General Public License for more      * ***
 *** * details.                                                   * ***
 *** *                                                            * ***
 *** * You should have received a copy of the GNU General Public  * ***
 *** * License along with this program; if not, write to the Free * ***
 *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
 *** * Floor, Boston, MA 02110-1301, USA.                         * ***
 *** *                                                            * ***
 *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
 *** *                                                            * ***
 **********************************************************************/
package org.etexascode.dtsimulator.gui;
import java.rmi.RemoteException;
import java.util.List;
import org.etexascode.api.eTEXAS;
import org.etexascode.interrep.datamodel.Signal;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;

/**
 * Simulator Interface for the DT Simulator.
 * @author jrutherford
 */
public class DTSimController implements SimulatorInterface
{
	/** The eTexas Instance. */
	private eTEXAS etexas;


	/** Constructor. */
	public DTSimController(eTEXAS etexas) { this.etexas = etexas; }

	@Override
	public StaticData getStaticData() throws RemoteException {
		return etexas.getStaticData();
	}

	@Override
	public StepData getStepData(long stepNum) throws RemoteException {
		return etexas.getStepData(stepNum);
	}

	@Override
	public void updateVehicleData(List<Vehicle> toUpdate) throws RemoteException {
		etexas.updateVehicleData(toUpdate);
	}

	@Override
	public void updateSignalData(List<Signal> toUpdate) throws RemoteException {
		etexas.updateSignalData(toUpdate);
	}

	@Override
	public void close() throws RemoteException { etexas.close(); }

	@Override
	public void addVehicleCommand(VehicleCommand command) throws RemoteException {
		etexas.addVehicleCommand(command);
	}
	
	@Override
	public void addSignalCommand(SignalCommand command) throws RemoteException {
		etexas.addSignalCommand(command);
	}
}