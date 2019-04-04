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

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.modules.junit4.PowerMockRunner;

/**
 * This integration test checks that creating a MapData message from a
 * LaneManager and then creating a LaneManager from the message results in two
 * similar LaneManagers.
 * 
 * @author janway
 */
@Ignore
@RunWith(PowerMockRunner.class)
public class MapDataIT {

	// Case with 2 lanes
	@Test
	public void testCombineMapData() {
		ReferencePoint[] rps = new ReferencePoint[2];
		rps[0] = new ReferencePoint(0.0, 0.0);
		rps[1] = new ReferencePoint(0.0, 0.003);

		LaneManager lm = new LaneManager();
		lm.setLatitude(rps[0].getLatitude());
		lm.setLongitude(rps[0].getLongitude());

		double[] offsets = UtilsLatLongConversion.convertLatLongToCentimeterOffset(rps[0].getLatitude(), rps[0].getLongitude(), rps[1].getLatitude(), rps[1].getLongitude(), lm.getGeoCalculatorType());

		Lane l1 = new Lane();
		l1.setLaneId(1);
		l1.getLaneGeomList().add(new LaneNode(0, 0));
		l1.getLaneGeomList().add(new LaneNode(offsets[0] / 2, 0));
		l1.getLaneGeomList().add(new LaneNode(offsets[0] / 2, 500));
		l1.getLaneGeomList().add(new LaneNode(offsets[0], 500));
		l1.setType(Lane.INBOUND);

		Lane l2 = new Lane();
		l2.setLaneId(2);
		l2.getLaneGeomList().add(new LaneNode(0, -500));
		l2.getLaneGeomList().add(new LaneNode(offsets[0] / 2, -500));
		l2.getLaneGeomList().add(new LaneNode(offsets[0] / 2, 0));
		l2.getLaneGeomList().add(new LaneNode(offsets[0], 0));
		l2.setType(Lane.INBOUND);

		lm.getLanes().put(1, l1);
		lm.getLanes().put(2, l2);

		List<LaneManager> lml = new ArrayList<LaneManager>();
		lml.add(lm);

		//		DeviceEmulatorRSE mockDevice = PowerMockito.mock(DeviceEmulatorRSE.class);
		//		AppLogger mockAppLogger = PowerMockito.mock(AppLogger.class);
		//
		//		PowerMockito.when(mockDevice.getLaneManagerInfo()).thenReturn(lmi);
		//		PowerMockito.when(mockDevice.getReferencePoints()).thenReturn(rps);
		//
		//		MapDataProducerApp app = new MapDataProducerApp();
		/*
		 * List<MessageRequest> objMsgs = app.performUpdate(mockDevice, null,
		 * null, 5.5, mockAppLogger); List<MapData> msgs = new
		 * LinkedList<MapData>(); for (Object obj : objMsgs) { if (obj
		 * instanceof MapData) { msgs.add((MapData)obj); } } LaneManager
		 * actualLM = UtilsMessageImports.parseMapDataMessage(msgs.get(0),
		 * lm.getGeoCalculatorType()); System.out.println(lm.toString());
		 * System.out.println(actualLM.toString());
		 * assertTrue(UtilsEquals.closelyEquals(lm, actualLM));
		 */
	}
}
