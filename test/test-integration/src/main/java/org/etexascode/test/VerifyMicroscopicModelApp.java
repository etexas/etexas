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
import org.etexascode.apps.MicroscopicIntellifusionDriver;
import org.etexascode.apps.ReportBasePopulatedApp;
import org.etexascode.apps.ReportDevice;
import org.etexascode.apps.UtilsEquals;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This app verifies that the Estimated Data Model is close to what we'd expect
 * after transforming the datamodel into J2735 messages and back.
 * 
 * @author janway
 */
public class VerifyMicroscopicModelApp extends ReportBasePopulatedApp implements IAppLifecycle {

	/**
	 * Static logger for convenience.
	 */
	private final static Logger LOGGER = LoggerFactory.getLogger(VerifyMicroscopicModelApp.class);

	/**
	 * App ID.
	 */
	public static final String APP_ID_VERIFY_MICROSCOPIC_MODEL = "MicroModel-verifier";

	/**
	 * The actual SignalManager from the previous timestep.
	 */
	private ISignalManager prevSMI = null;

	/**
	 * The actual VehicleManager from the previous timestep.
	 */
	private IVehicleManager prevVMI = null;

	/**
	 * The actual LaneManager from the previous timestep.
	 */
	private ILaneManager prevLMI = null;

	/**
	 * The driver for creating the microscopic model.
	 */
	MicroscopicIntellifusionDriver hid = new MicroscopicIntellifusionDriver();

	/**
	 * The percent of vehicles with OBUs.
	 */
	@AppConfigProperty(value = "100.0", displayName = "OBU percent", description = "The percent of vehicles with OBUs.")
	private double OBUpercent;

	@Override
	public void init(String[] appConfigs) {
		OBUpercent = Double.parseDouble(appConfigs[0]);
	}

	@Override
	public void performUpdate(EstimatedDataModel model, ReportDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

		InterRepInfoModel irim = hid.parseModel(messages, device.getDetectorManager(), device.getLaneManager(), simTime, getGeoCalcType(), getTimeStep());
		if (irim == null) {
			LOGGER.info("Microscopic Model was null.");

		}

		if (Math.abs(OBUpercent - 100.0) < .0005) { // if 100% OBU

			ISignalManager estiSMI = irim.smi;
			IDetectorManager estiDMI = irim.dmi;
			IVehicleManager estiVMI = irim.vmi;
			ILaneManager estiLMI = irim.lmi;

			if (!UtilsEquals.closelyEquals(estiSMI, prevSMI)) {
				if ((estiSMI != null) && (prevSMI != null)) {
					LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiSMI.toString(), prevSMI.toString()));
					logger.log("Microscopic Model Verification - Signal Failure", String.format("%nEstimated: %s%nActual: %s", estiSMI.toString(), prevSMI.toString()));
				}
				logger.log("Microscopic Model Verification - Signal Failure Count", "Signal Manager failed verification.");
			}
			if (!UtilsEquals.closelyEquals(estiDMI, device.getDetectorManager())) {
				if ((estiDMI != null) && (device.getDetectorManager() != null)) {
					LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiDMI.toString(), device.getDetectorManager().toString()));
					logger.log("Microscopic Model Verification - Detector Failure", String.format("%nEstimated: %s%nActual: %s", estiDMI.toString(), device.getDetectorManager().toString()));
				}
				logger.log("Microscopic Model Verification - Detector Failure Count", "Detector Manager failed verification.");
			}
			if (!UtilsEquals.closelyEquals(estiVMI, prevVMI)) {
				if ((estiVMI != null) && (prevVMI != null)) {
					LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiVMI.toString(), prevVMI.toString()));
					logger.log("Microscopic Model Verification - Vehicle Failure", String.format("%nEstimated: %s%nActual: %s", estiVMI.toString(), prevVMI.toString()));
				}
				logger.log("Microscopic Model Verification - Vehicle Failure Count", "Vehicle Manager failed verification.");
			}
			if (!UtilsEquals.closelyEquals(estiLMI, prevLMI)) {
				if ((estiLMI != null) && (prevLMI != null)) {
					LOGGER.info(String.format("Not closelyEqual:%nEstimated:%n%s%nActual:%n%s", estiLMI.toString(), prevLMI.toString()));
					logger.log("Microscopic Model Verification - Lane Failure", String.format("%nEstimated: %s%nActual: %s", estiVMI.toString(), prevVMI.toString()));
				}
				logger.log("Microscopic Model Verification - Lane Failure Count", "Lane Manager failed verification.");
			}

		}
		else { // else there should be some non-DSRC vehicles. print out their info for manual comparison
			if ((prevVMI != null) && (irim.vmi != null)) {
				logger.log("Non-DSRC Vehicle Comparison",
				        String.format("%nModeled: %n%s%n%nActual (previous): %n%s%n%n Actual (current): %n%s%n", irim.vmi.toString(), prevVMI.toString(), device.getVehicleManager().toString()));
			}
		}

		prevSMI = device.getSignalManager();
		prevVMI = device.getVehicleManager();
		prevLMI = device.getLaneManager();

	}

	/**
	 * The calculator type being used by the user
	 * 
	 * @return The type of calc to use
	 */
	public int getGeoCalcType() {
		return UtilsLatLongConversion.GEODETIC2D;
	}

	/**
	 * The time step.
	 * 
	 * @return The time step in seconds.
	 */
	public double getTimeStep() {
		return 1.0;
	}

	@Override
	public void appShutdown(AppLogger logger) {
	}
}
