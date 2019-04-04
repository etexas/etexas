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

import java.util.LinkedList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Java object emulating a service access point for IEEE WAVE 1609.3 This SAP provides the ability
 * to send and receive WSMP.
 * 
 * @author bbadillo
 */
public class WaveShortMessageSAPImpl implements WaveShortMessageSAP {

    /**
     * private static final logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(WaveShortMessageSAPImpl.class);

    /**
     * Check to see if address is a broadcast address.
     * 
     * @param mac A 6 byte array representing a mac address.
     * @return True if the mac address is a broadcast address.
     */
    static public boolean isBroadcastMac(byte mac[]) {
        if (mac.length == 6) {
            for (byte octet : mac) {
                LOGGER.debug("Should be -1: {}", Integer.valueOf(octet));
                if (octet != (byte)0xFF) {
                    return false;
                }
            }
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Checks if a MAC address is valid.
     * 
     * @param mac The MAC to check.
     * @return True/False.
     */
    static public boolean isValidMac(long mac) {
        if (mac < 0 || mac > WaveShortMessageSAP.BROADCAST_MAC) {
            return false;
        }

        return true;
    }

    /**
     * A list of message requests that will be sent on the next time step.
     */
    private final List<WSMRequest> pendingRequests = new LinkedList<WSMRequest>();

    /**
     * A list of message indications (received messages) that will be received on the current time
     * step.
     */
    private final List<BasicMessage> pendingIndications = new LinkedList<BasicMessage>();

    /**
     * Getter for pending indications
     * 
     * @return List of {@link BasicMessage}
     */
    public List<BasicMessage> getPendingIndications() {
        return pendingIndications;
    }

    /**
     * Getter for pending requests
     * 
     * @return List of {@link WSMRequest}
     */
    public List<WSMRequest> getPendingRequests() {
        return pendingRequests;
    }

    /**
     * Retrieves the pending indications as an array list
     * 
     * @return indications The indications retrieved
     */
    @Override
    public BasicMessage[] receive() {
        BasicMessage[] indications = pendingIndications.toArray(new DSRCMessage[pendingIndications.size()]);
        return indications;
    }

    /**
     * Confirms that pending requests were received
     * 
     * @return WMS Confirmation
     */
    @Override
    public WSMConfirm send(WSMRequest request) {
        pendingRequests.add(request);
        return WSMConfirm.WSM_CONFIRM_ACCEPTED;
    }
}
