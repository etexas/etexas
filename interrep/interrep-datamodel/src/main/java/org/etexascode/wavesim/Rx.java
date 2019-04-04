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
package org.etexascode.wavesim;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;

/**
 * The output of the wave simulator
 * 
 * @author ablatt
 */
public class Rx {

    /**
     * The address of the node receiving the messages in the map.
     */
    public final long mac;

    /**
     * Map of timesteps when messages will arrive, keyed by message id.
     */
    public final Map<String, Integer> messages;

    /**
     * Constructor
     * 
     * @param mac The mac of the node
     */
    public Rx(long mac) {
        this.mac = mac;
        messages = new HashMap<String, Integer>();
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Rx) {
            Rx nm = (Rx)o;
            if (nm.mac == mac) {
                return messages.equals(nm.messages);
            }
        }

        return false;
    }

    @Override
    @CoberturaIgnore
    public int hashCode() {
        return new HashCodeBuilder(329, 91).append(mac).hashCode();
    }

    @Override
    @CoberturaIgnore
    public String toString() {
        StringBuilder sb = new StringBuilder("mac = ");
        sb.append(mac);
        sb.append("\n");
        sb.append(UtilsStringOnModel.addMap(messages, ""));
        return sb.toString();
    }
}
