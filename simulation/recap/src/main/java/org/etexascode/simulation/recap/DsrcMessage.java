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
package org.etexascode.simulation.recap;

import java.util.EventObject;

/**
 * Defines the information of interest in a DSRC message. All values should be checked before use,
 * as no field is guaranteed to exist. Furthermore, the corrupted status of each message should be
 * checked to ensure that no invalid messages are processed.
 * 
 * @author emyers
 */
public class DsrcMessage extends EventObject {

    /**
     * The serial version ID.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The information packet.
     */
    private DsrcPacket packet;

    /**
     * Creates a new <code>DsrcMessage</code> with the given source DSRC unit and packet
     * information.
     * 
     * @throws NullPointerException if a <code>null</code> value is given for the source DSRC unit
     *         or packet information
     */
    DsrcMessage(DsrcUnit source, DsrcPacket packet) {

        // set the source
        super(source);

        // if no packet information is given
        if (packet == null) {

            // throw a NullPointerException to indicate the error
            throw new NullPointerException("no packet information exists");
        }

        // set the packet information
        this.packet = packet;
    }

    /**
     * Returns the name of the source DSRC unit.
     * 
     * @return the name of the source DSRC unit
     */
    public String getUnitName() {

        return ((DsrcUnit)source).getName();
    }

    /**
     * Returns the message corruption status.
     * 
     * @return the message corruption status
     */
    public boolean isCorrupt() {

        return packet.isMalformed();
    }

    /**
     * Returns the epoch message time (s).
     * 
     * @return the epoch message time (s), or <code>null</code> if no time could be parsed from the
     *         packet information
     */
    public Double getTime() {

        try {

            // parse the time value
            return Double.parseDouble(packet.getTime());
        }
        catch (Exception exception) {

            // return that no time value could be parsed
            return null;
        }
    }

    /**
     * Returns the PSID (hex).
     * 
     * @return the PSID (hex), or <code>null</code> if no PSID could be parsed from the packet
     *         information
     */
    public String getPsid() {

        // return the PSID (or null if none could be parsed)
        String psid = packet.getPsid();
        return (psid == null || psid.isEmpty()) ? null : psid;
    }

    /**
     * Returns the transmission status.
     * 
     * @return the transmission status, or <code>null</code> if no transmission status could be
     *         parsed from the packet information
     */
    public Boolean isTx() {

        String isTx = packet.isTx();
        return (isTx == null || isTx.isEmpty()) ? null : isTx.equals("01000000");
    }

    /**
     * Return the wave short message.
     * 
     * @return the wave short message, or <code>null</code> if no wave short message could be parsed
     *         from the packet information
     */
    public String getWsm() {

        String wsm = packet.getWsm();
        return (wsm == null || wsm.isEmpty()) ? null : wsm;
    }
}