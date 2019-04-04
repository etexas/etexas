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
 * Java object emulating a service primitive for IEEE WAVE 1609.3 The parameters of the
 * WME-WSMService.request primitive are as follows: WME-WSMService.request ( Local Service Index
 * Action ProviderServiceIdentifier ) On receipt, the WME generates a WME-WSMService.confirm
 * indicating whether the request is accepted. On acceptance, the received WAVE Short Messages will
 * be sent to the requesting higher layer entity.
 * 
 * @author bbadillo
 */
public class WMEWaveServiceRequest {

    /**
     * Local service index codes
     */
    public static final int ADD = 1;

    public static final int DELETE = 2;

    /**
     * Cache for the action of the request
     */
    private int action;

    /**
     * Cache for the provider service identifier
     */
    private byte[] providerServiceIdentifier;

    /**
     * Gets the provider service identifier for the WSM service request
     * 
     * @return providerServiceIdentifier The provider service identifier
     */
    public byte[] getProviderServiceIdentifier() {
        return providerServiceIdentifier == null ? null : providerServiceIdentifier.clone();
    }

    /**
     * Sets the provider service identifier for the WSM service request
     * 
     * @param providerServiceIdentifier The provider service identifier
     */
    public void setProviderServiceIdentifier(byte[] providerServiceIdentifier) {
        this.providerServiceIdentifier = providerServiceIdentifier == null ? null : providerServiceIdentifier.clone();
    }

    /**
     * Gets the action for the WSM service request
     * 
     * @return action The action for the WSM service request
     */
    public int getAction() {
        return action;
    }

    /**
     * Sets the action for the WSM service request
     * 
     * @param action The action to set
     */
    public void setAction(int action) {
        this.action = action;
    }
}
