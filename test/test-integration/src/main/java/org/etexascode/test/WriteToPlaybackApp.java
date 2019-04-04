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

import java.io.IOException;
import java.util.Collection;

import org.etexascode.apps.EstimatedDataModel;
import org.etexascode.apps.MicroscopicIntellifusionDriver;
import org.etexascode.apps.ReportBasePopulatedApp;
import org.etexascode.apps.ReportDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.utils.UtilsArchiveOperations;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WriteToPlaybackApp extends ReportBasePopulatedApp implements IAppLifecycle {

	/**
	 * Static logger.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(WriteToPlaybackApp.class);

	String dir = System.getProperty("user.home") + "\\Test\\microscopic\\";

	/**
	 * The driver for creating the microscopic model.
	 */
	MicroscopicIntellifusionDriver hid = new MicroscopicIntellifusionDriver();

	/**
	 * The step size of the simulator
	 */
	private double stepSize = 0.0;

	@AppConfigProperty(value = "projName", displayName = "Project Name", description = "The name for the project this app creates.")
	private String projName;

	/**
	 * The first InterRepInfoModel (actual). Not written out until the second
	 * time step so we can determine step size first.
	 */
	private InterRepInfoModel first_actual = null;

	/**
	 * The first InterRepInfoModel (model). Not written out until the second
	 * time step so we can determine step size first.
	 */
	private InterRepInfoModel first_model = null;

	@Override
	public void init(String[] appConfigs) {
		projName = appConfigs[0];
	}

	@Override
	public void performUpdate(EstimatedDataModel model, ReportDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

		InterRepInfoModel actual = new InterRepInfoModel(device.getLaneManager(), device.getVehicleManager(), device.getSignalManager(), device.getDetectorManager(), new ReferencePoint[0], simTime,
		        0.5);
		InterRepInfoModel micromodel = hid.parseModel(messages, device.getDetectorManager(), device.getLaneManager(), simTime, getGeoCalcType(), 0.5);

		try {

			if (first_actual == null) {
				first_actual = actual;
				first_model = micromodel;
				return;
			}
			else if (stepSize == 0.0) {
				stepSize = simTime - first_actual.simTime;
				UtilsArchiveOperations.writeInterRepInfoToPlayback(first_actual, projName + "_actual", "", (int)(simTime / stepSize), stepSize);
				UtilsArchiveOperations.writeInterRepInfoToPlayback(first_model, projName + "_model", "", (int)(simTime / stepSize), stepSize);
			}

			if (stepSize != 0.0) {
				boolean logUpdate = (simTime.intValue() % 100) == 0;

				if (logUpdate) {
					LOGGER.info("Writing microscopic model out to playback.");
				}
				UtilsArchiveOperations.writeInterRepInfoToPlayback(micromodel, projName + "_model", "", (int)(simTime / stepSize), stepSize);
				if (logUpdate) {
					String msg = "Writing underlying model out to playback.";
					LOGGER.info(msg);
					logger.log(this.getClass().getSimpleName(), msg);
				}
				UtilsArchiveOperations.writeInterRepInfoToPlayback(actual, projName + "_actual", "", (int)(simTime / stepSize), stepSize);
				if (logUpdate) {
					LOGGER.info("Successfully wrote both models.");
				}
			}
		}
		catch (IOException e) {
			LOGGER.error("IOException in WriteToPlaybackApp, check AppLogs.");
			throw new RuntimeException(e);
		}
	}

	/**
	 * The calculator type being used by the user
	 * 
	 * @return The type of calc to use
	 */
	public int getGeoCalcType() {
		return UtilsLatLongConversion.GEODETIC2D;
	}

	@Override
	public void appShutdown(AppLogger logger) {
	}
}
