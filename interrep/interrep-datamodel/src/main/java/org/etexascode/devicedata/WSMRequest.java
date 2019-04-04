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

import java.io.Serializable;

/**
 * Java object emulating a service primitive for IEEE WAVE 1609.3 The parameters of the
 * WSM-WaveShortMessage.request primitive are as follows: WSM-WaveShortMessage.request ( Channel
 * Identifier, DataRate, Transmit Power Level, ProviderServiceIdentifier, User Priority,
 * WsmExpiryTime, Length, Data, Peer MAC address, WSMP header extensions, WAVE Element ID ) Upon
 * receipt of the WSM-WaveShortMessage.request primitive, the WSMP delivers the WAVE Short Message
 * to LLC.
 * 
 * @author bbadillo
 */
public class WSMRequest implements Serializable {

    /** Serial ID */
    private static final long serialVersionUID = -6548268650818613163L;

    /** WSM request channel identifier container */
    private int channelIdentifier;

    /** WSM request data rate container */
    private int dataRate;

    /** WSM request transmit power level container */
    private int transmitPowerLevel;

    /** WSM request provider service identifier as a byte array container */
    private byte[] providerServiceIdentifier;

    /** WSM request user priority container */
    private int userPriority;

    /** WSM request expiry time container */
    private int wsmExpiryTime;

    /** WSM request length container */
    private int length;

    /** WSM request message data container */
    private Object data;

    /** WSM request destination MAC address container */
    private long peerMACAddress;

    /** WSM request header extension container */
    private byte wsmpHeaderExtensions;

    /** WSM request wave element id container */
    private int waveElementID;

    /**
     * Gets the channel identifier of the WSM request
     * 
     * @return channelIdentifier The channel identifier of the WSM request
     */
    public int getChannelIdentifier() {
        return channelIdentifier;
    }

    /**
     * Sets the WSM request channel identifier
     * 
     * @param channelIdentifier The channel identifier to set
     */
    public void setChannelIdentifier(int channelIdentifier) {
        this.channelIdentifier = channelIdentifier;
    }

    /**
     * Get the data rate for the WSM request
     * 
     * @return dataRate The data rate for the WSM request
     */
    public int getDataRate() {
        return dataRate;
    }

    /**
     * Sets the data rate for the WSM request
     * 
     * @param dataRate The data rate to set
     */
    public void setDataRate(int dataRate) {
        this.dataRate = dataRate;
    }

    /**
     * Gets the transmit power level for the WSM request
     * 
     * @return transmitPowerLevel The transmit power level of the WSM request
     */
    public int getTransmitPowerLevel() {
        return transmitPowerLevel;
    }

    /**
     * Sets the transmit power level for the WSM request
     * 
     * @param transmitPowerLevel The transmit power level to set
     */
    public void setTransmitPowerLevel(int transmitPowerLevel) {
        this.transmitPowerLevel = transmitPowerLevel;
    }

    /**
     * Gets the provider service identifier for the WSM request
     * 
     * @return providerServiceIdentifier The provider service identifier for the WSM request
     */
    public byte[] getProviderServiceIdentifier() {
        return providerServiceIdentifier == null ? null : providerServiceIdentifier.clone();
    }

    /**
     * Sets the provider service identifier for the WSM request
     * 
     * @param providerServiceIdentifier The provider service identifier to set
     */
    public void setProviderServiceIdentifier(byte[] providerServiceIdentifier) {
        this.providerServiceIdentifier = providerServiceIdentifier == null ? null : providerServiceIdentifier.clone();
    }

    /**
     * Gets the user priority for the WSM request
     * 
     * @return userPriority The user priority of the WSM request
     */
    public int getUserPriority() {
        return userPriority;
    }

    /**
     * Sets the user priority for the WSM request
     * 
     * @param userPriority The user priority to set
     */
    public void setUserPriority(int userPriority) {
        this.userPriority = userPriority;
    }

    /**
     * Gets the expiry time for the WSM request
     * 
     * @return wsmExpiryTime The expiry time of the WSM request
     */
    public int getWsmExpiryTime() {
        return wsmExpiryTime;
    }

    /**
     * Sets the expiry time for the WSM request
     * 
     * @param wsmExpiryTime The WSM expiry time to set
     */
    public void setWsmExpiryTime(int wsmExpiryTime) {
        this.wsmExpiryTime = wsmExpiryTime;
    }

    /**
     * Gets the length of the WSM request
     * 
     * @return length The length of the WSM request
     */
    public int getLength() {
        return length;
    }

    /**
     * Sets the length of the WSM request
     * 
     * @param length The length to set to the WSM request
     */
    public void setLength(int length) {
        this.length = length;
    }

    /**
     * Gets the message data of the WSM request
     * 
     * @return data The message data of the WSM request
     */
    public Object getData() {
        return data;
    }

    /**
     * Sets the message data of the WSM request
     * 
     * @param data The message data to set to the WSM request
     */
    public void setData(Object data) {
        this.data = data;
    }

    /**
     * Gets the destination MAC address of the WSM request
     * 
     * @return peerMACAddress The destination MAC address of the WSM request
     */
    public long getPeerMACAddress() {
        return peerMACAddress;
    }

    /**
     * Sets the destination MAC address of the WSM request
     * 
     * @param peerMACAddress The destination MAC address to set
     */
    public void setPeerMACAddress(long peerMACAddress) {
        this.peerMACAddress = peerMACAddress;
    }

    /**
     * Gets the header extensions for the WSM request
     * 
     * @return wsmpHeaderExtensions The header extensions for the WSM request
     */
    public byte getWsmpHeaderExtensions() {
        return wsmpHeaderExtensions;
    }

    /**
     * Sets the header extensions for the WSM request
     * 
     * @param wsmpHeaderExtensions The header extensions to set to the WSM request
     */
    public void setWsmpHeaderExtensions(byte wsmpHeaderExtensions) {
        this.wsmpHeaderExtensions = wsmpHeaderExtensions;
    }

    /**
     * Gets the wave element id for the WSM request
     * 
     * @return waveElementID The wave element id of the WSM request
     */
    public int getWaveElementID() {
        return waveElementID;
    }

    /**
     * Sets the wave element id for the WSM request
     * 
     * @param waveElementID The wave element to set to the WSM request
     */
    public void setWaveElementID(int waveElementID) {
        this.waveElementID = waveElementID;
    }
}
