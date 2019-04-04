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
 * Java object emulating a service primitive for IEEE WAVE 1609.3 The parameters of the primitive
 * are as follows: WSM-WaveShortMessage.confirm ( ResultCode )
 * 
 * @author bbadillo
 */
public class WSMConfirm {

    public static final int ACCEPTED = 1;

    public static final int REJECTED_LENGTH_EXCEEDED = 2;

    public static final int REJECTED_UNSPECIFIED = 3;

    public static final WSMConfirm WSM_CONFIRM_ACCEPTED = new WSMConfirm(ACCEPTED);

    public static final WSMConfirm WSM_CONFIRM_REJECTED_LENGTH_EXCEEDED = new WSMConfirm(REJECTED_LENGTH_EXCEEDED);

    public static final WSMConfirm WSM_CONFIRM_REJECTED_UNSPECIFIED = new WSMConfirm(REJECTED_UNSPECIFIED);

    private WSMConfirm(int resultCode) {
        this.resultCode = resultCode;
    }

    /**
     * Name: ResultCode Type: Enumerated Valid Range: Accepted, Rejected max length exceeded,
     * Rejected unspecified Description: Indicates the result of the associated request.
     */
    private int resultCode;

    public int getResultCode() {
        return resultCode;
    }
}
