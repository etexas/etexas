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

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The cell message class.
 * 
 * @author ttevendale
 */
public class CellMessage extends BasicMessage {

    /** Serial ID. */
    private static final long serialVersionUID = -1794885599822018085L;

    /**
     * Sets the message with data object provided associated to a destination MAC address.
     * 
     * @param data The message data.
     * @param peerMAC The destination MAC address.
     */
    public CellMessage(Object data, long peerMAC) {

        super(data, peerMAC);
    }

    /**
     * Sets the message with data object provided associated to a destination MAC address.
     * 
     * @param data The message data.
     * @param peerMAC The destination MAC address.
     * @param size the size of the message.
     */
    public CellMessage(Object data, long peerMAC, int size) {

        super(data, peerMAC, size);
    }

    /**
     * Sets the message with data object provided associated to a destination MAC address.
     * 
     * @param data The message data.
     * @param peerMAC The destination MAC address.
     * @param size the size of the message.
     * @param originMAC The origin MAC address.
     */
    public CellMessage(Object data, long peerMAC, int size, long originMAC) {

        super(data, peerMAC, size, originMAC);
    }

    @Override
    public BasicMessage copy() {

        return new CellMessage(this.getData(), this.getPeerMACAddress(), this.getSize(), this.getOriginMACAddress());
    }

    @Override
    public boolean equals(Object object) {

        if (object instanceof CellMessage) {

            return super.equals(object);
        }
        else {

            return false;
        }
    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(11, 193).appendSuper(super.hashCode()).hashCode();
    }
}