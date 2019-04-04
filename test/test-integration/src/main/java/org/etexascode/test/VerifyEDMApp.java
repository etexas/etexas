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

import java.util.Collection;

import org.etexascode.apps.EstimatedDataModel;
import org.etexascode.apps.ReportBasePopulatedApp;
import org.etexascode.apps.ReportDevice;
import org.etexascode.apps.UtilsEquals;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This app verifies that the Estimated Data Model is close to what we'd expect
 * after transforming the datamodel into J2735 messages and back.
 * 
 * @author janway
 */
public class VerifyEDMApp extends ReportBasePopulatedApp {

	/**
	 * Static logger.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(VerifyEDMApp.class);

	/**
	 * The actual SignalManager from the previous timestep.
	 */
	private ISignalManager prevSMI = null;

	/**
	 * The actual DetectorManager from the previous timestep.
	 */
	private IDetectorManager prevDMI = null;

	/**
	 * The actual VehicleManager from the previous timestep.
	 */
	private IVehicleManager prevVMI = null;

	/**
	 * The actual LaneManager from the previous timestep.
	 */
	private ILaneManager prevLMI = null;

	@Override
	public void performUpdate(EstimatedDataModel model, ReportDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

		ISignalManager estiSMI = model.getSignalManager();
		IDetectorManager estiDMI = model.getDetectorManager();
		IVehicleManager estiVMI = model.getVehicleManager();
		ILaneManager estiLMI = model.getLaneManager();

		if (!UtilsEquals.closelyEquals(estiSMI, prevSMI)) {
			if ((estiSMI != null) && (prevSMI != null)) {
				LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiSMI.toString(), prevSMI.toString()));
			}
			logger.log("EDM Verification", "Signal Manager failed verification.");
		}
		if (!UtilsEquals.closelyEquals(estiDMI, prevDMI)) {
			if ((estiDMI != null) && (prevDMI != null)) {
				LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiDMI.toString(), prevDMI.toString()));
			}
			logger.log("EDM Verification", "Detector Manager failed verification.");
		}
		if (!UtilsEquals.closelyEquals(estiVMI, prevVMI)) {
			if ((estiVMI != null) && (prevVMI != null)) {
				LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiVMI.toString(), prevVMI.toString()));
			}
			logger.log("EDM Verification", "Vehicle Manager failed verification.");
		}
		if (!UtilsEquals.closelyEquals(estiLMI, prevLMI)) {
			if ((estiLMI != null) && (prevLMI != null)) {
				LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiLMI.toString(), prevLMI.toString()));
			}
			logger.log("EDM Verification", "Lane Manager failed verification.");
		}

		prevSMI = device.getSignalManager();
		prevDMI = device.getDetectorManager();
		prevVMI = device.getVehicleManager();
		prevLMI = device.getLaneManager();

	}

}
