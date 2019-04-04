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

/**
 * Defines the information of interest in a DSRC message packet. Packet information values may be
 * <code>null</code> or empty. As such, all values should be checked prior to use. The formation
 * status of the packet should also be checked to ensure that no malformed packets are processed.
 * 
 * @author emyers
 */
class DsrcPacket {

    /**
     * The packet formation status.
     */
    private boolean isMalformed;

    /**
     * The epoch time value.
     */
    private String time;

    /**
     * The PSID (WSMs only).
     */
    private String psid;

    /**
     * The transmission status value.
     */
    private String isTx;

    /**
     * The wave short message (WSM).
     */
    private String wsm;

    /**
     * Creates a new <code>DsrcPacket</code> with empty values.
     */
    DsrcPacket() {

        time = "";
        psid = "";
        isTx = "";
        wsm = "";
    }

    /**
     * Returns the packet formation status.
     * 
     * @return the packet formation status
     */
    boolean isMalformed() {

        return isMalformed;
    }

    /**
     * Sets the packet formation status.
     * 
     * @param isMalformed the packet formation status
     */
    void setMalformed(boolean isMalformed) {

        this.isMalformed = isMalformed;
    }

    /**
     * Returns the epoch time value.
     * 
     * @return the epoch time value
     */
    String getTime() {

        return time;
    }

    /**
     * Sets the epoch time value.
     * 
     * @param time the epoch time value
     */
    void setTime(String time) {

        this.time = time;
    }

    /**
     * Returns the PSID value.
     * 
     * @return the PSID value
     */
    String getPsid() {

        return psid;
    }

    /**
     * Sets the PSID value.
     * 
     * @param psid the PSID value
     */
    void setPsid(String psid) {

        this.psid = psid;
    }

    /**
     * Returns the transmission status value.
     * 
     * @return the transmission status value
     */
    String isTx() {

        return isTx;
    }

    /**
     * Sets the transmission status value.
     * 
     * @param isTx the transmission status value
     */
    void setTx(String isTx) {

        this.isTx = isTx;
    }

    /**
     * Returns the wave short message value.
     * 
     * @return the wave short message value
     */
    String getWsm() {

        return wsm;
    }

    /**
     * Sets the wave short message value.
     * 
     * @param wsm the wave short message value
     */
    void setWsm(String wsm) {

        this.wsm = wsm;
    }
}