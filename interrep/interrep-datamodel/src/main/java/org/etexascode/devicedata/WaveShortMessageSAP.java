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

package org.etexascode.devicedata;

/**
 * Java interface that emulates a service access point for IEEE WAVE 1609.3 This SAP provides the
 * ability to send and receive WSMP.
 * 
 * @author bbadillo
 */
public interface WaveShortMessageSAP {

    /**
     * A constant to represent the broadcast MAC address for WAVE.
     */
    public static final long BROADCAST_MAC = 0xFFFFFFFFFFFFL;

    /**
     * Send a WSMRequest.
     * 
     * @param request An SAP primitive that contains the address and data.
     * @return An SAP primitive that contains the send status.
     */
    public WSMConfirm send(WSMRequest request);

    /**
     * Receive all queued messages for the requesting CV App.
     * 
     * @return An array of received messages.
     */
    public BasicMessage[] receive();
}
