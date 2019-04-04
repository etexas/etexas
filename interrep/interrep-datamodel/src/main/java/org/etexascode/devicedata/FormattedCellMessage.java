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
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IFormattedMessage;

/**
 * A class containing the information for a formatted cell message.
 * 
 * @author ttevendale
 */
public class FormattedCellMessage extends CellMessage implements IFormattedMessage {

    /** Serial ID. */
    private static final long serialVersionUID = -2774811758986107118L;

    /** The formatted data. */
    private final Object formattedData;

    /**
     * Sets the WSM indication with a raw data object, a formatted data object, the associated
     * destination MAC address and the size of the message.
     * 
     * @param rawData The raw message data.
     * @param formattedData The formatted message data.
     * @param peerMAC The destination MAC address.
     * @param size The size of the message.
     */
    public FormattedCellMessage(Object rawData, Object formattedData, long peerMAC, int size) {

        super(rawData, peerMAC, size);
        this.formattedData = formattedData;
    }

    /**
     * Sets the WSM indication with a raw data object, a formatted data object, the associated
     * destination MAC address, the size of the message, and the origin MAC address.
     * 
     * @param rawData The raw message data.
     * @param formattedData The formatted message data.
     * @param peerMAC The destination MAC address.
     * @param size The size of the message.
     * @param originMac The origin MAC address.
     */
    public FormattedCellMessage(Object rawData, Object formattedData, long peerMAC, int size, long originMac) {

        super(rawData, peerMAC, size, originMac);
        this.formattedData = formattedData;
    }

    @Override
    public BasicMessage copy() {

        return new FormattedCellMessage(this.getData(), this.getFormattedData(), this.getPeerMACAddress(), this.getSize(), this.getOriginMACAddress());
    }

    /**
     * Getter for the formatted data.
     * 
     * @return The formatted data.
     */
    @CoberturaIgnore
    public Object getFormattedData() {

        return formattedData;
    }

    @Override
    public boolean equals(Object object) {

        if (object instanceof FormattedCellMessage) {

            FormattedCellMessage formattedCellMessage = (FormattedCellMessage)object;
            return super.equals(formattedCellMessage)
                    && formattedData.equals(formattedCellMessage.formattedData);
        }
        else {

            return false;
        }
    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(79, 139).appendSuper(super.hashCode()).append(formattedData.hashCode()).hashCode();
    }
}