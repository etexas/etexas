package com.harmonia.interrep.wave;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.LinkedList;
import java.util.List;

import org.etexascode.devicedata.WSMConfirm;
import org.etexascode.devicedata.WSMIndication;
import org.etexascode.devicedata.WSMRequest;
import org.etexascode.devicedata.WaveShortMessageSAP;
import org.etexascode.devicedata.WaveShortMessageSAPImpl;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
/**
 * tests for WaveShortMessageSAPImpl in com.harmonia.interrep.wave
 * @author bmauldon
 *
 */
public class WaveShortMessageSAPImplTest {
	/**
	 * Create WaveShortMessageSAPImpl object
	 */
	WaveShortMessageSAPImpl wsmSAP= new WaveShortMessageSAPImpl();
	/**
	 * Create WSMRequest object and List
	 */
	WSMRequest request= new WSMRequest();
	private final List<WSMRequest> pendingRequests = new LinkedList<WSMRequest>();

	WSMConfirm accepted=WSMConfirm.WSM_CONFIRM_ACCEPTED;
	/**
	 * macId gives the maximum address allowed, errorId has a negative address (not allowed)
	 * zer0 and macID define the boundaries for allowed id numbers
	 */
	long macId=WaveShortMessageSAP.BROADCAST_MAC;
	long errorId=-12394890;
	long zero=0;

	/**
	 * create WSMIndication object and List
	 */
	WSMIndication indication =new WSMIndication(null, macId);
	private final List<WSMIndication> pendingIndications = new LinkedList<WSMIndication>();

	/**
	 * byte arrays, macBadByte has an unallowed byte, macShort is not equal to 6
	 */
	byte[] mac=new byte[] {(byte) 0xFF,(byte) 0xFF,(byte) 0xFF,(byte) 0xFF, (byte) 0xFF,(byte) 0xFF};
	byte[] macBadByte =new byte[] {(byte) 0xFF,(byte) 0x80,(byte) 0xFF,(byte) 0xFF, (byte) 0xFF,(byte) 0xFF};
	byte[] macShort=new byte [] {(byte) 0xFF,(byte) 0xFF};
	@Before
	public void setUp() throws Exception {

		// Populate the WSMRequest bean fields
		request.setChannelIdentifier(3);
		request.setData(null);
		request.setDataRate(2);
		request.setLength(9);
		request.setPeerMACAddress(macId);
		request.setProviderServiceIdentifier(macShort);
		request.setTransmitPowerLevel(5);
		request.setUserPriority(3);
		request.setWaveElementID(2);
		request.setWsmExpiryTime(10);
		request.setWsmpHeaderExtensions((byte)0x80);
		// no setter for pendingIndications
		wsmSAP.getPendingIndications().add(indication);

	}

	@After
	public void tearDown() throws Exception {
		pendingRequests.clear();
		pendingIndications.clear();
	}

	@Test
	public void testGetPendingIndications() {

		assertEquals(indication,wsmSAP.getPendingIndications().get(0));

	}

	@Test
	public void testGetPendingRequests() {
		WSMConfirm status=wsmSAP.send( request);
		assertEquals(request,wsmSAP.getPendingRequests().get(0));
	}

	@Test
	public void testIsBroadcastMac() {

		assertFalse(WaveShortMessageSAPImpl.isBroadcastMac(macBadByte));
		assertFalse(WaveShortMessageSAPImpl.isBroadcastMac(macShort));
		assertTrue(WaveShortMessageSAPImpl.isBroadcastMac(mac));
	}

	@Test
	public void testIsValidMac() {
		assertFalse(WaveShortMessageSAPImpl.isValidMac(errorId));
		assertFalse(WaveShortMessageSAPImpl.isValidMac(macId+1));
		assertTrue(WaveShortMessageSAPImpl.isValidMac(macId));
		assertTrue(WaveShortMessageSAPImpl.isValidMac(zero));
	}

	@Test
	public void testReceive() {
		WSMIndication[] wsmi= wsmSAP.receive();
		WSMIndication wsmI=wsmi[0];
		assertEquals(indication, wsmI);
	}

	@Test
	public void testSend() {
		WSMConfirm status=wsmSAP.send( request);
		assertEquals(accepted,status);
		assertEquals(request,wsmSAP.getPendingRequests().get(0));
	}

}
