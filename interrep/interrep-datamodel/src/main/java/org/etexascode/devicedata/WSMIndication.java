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
 * WSM-WaveShortMessage.indication primitive are as follows: WSM-WaveShortMessage.indication (
 * WsmpVersion Channel Number, DataRate, Transmit Power Used, ProviderServiceIdentifier, User
 * Priority, Length, Data, Peer MAC address ) The WSM-WaveShortMessage.indication primitive is
 * generated by the WSMP to deliver WSM information to a higher layer entity indicated in the MIB
 * WsmServiceRequestTable.
 * 
 * @author bbadillo
 */
public class WSMIndication {
    // THESE FIELDS ARE BEING COMMENTED OUT DUE TO FINDBUGS
    // PLEASE RENENABLE AS NEEDED.
    // private int wsmpVersion;
    // private int channelNumber;
    // private int dataRate;
    // private int transmitPowerUsed;
    // private byte [] providerServiceIdentifier;
    // private int userPriority;
    // private int length;

}
