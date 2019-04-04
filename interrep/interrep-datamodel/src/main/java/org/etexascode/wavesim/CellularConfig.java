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

import java.io.Serializable;

/**
 * The Cellular Configurations.
 *
 * @author ttevendale
 */
public class CellularConfig implements Serializable {

    /**
     * Serial ID.
     */
    private static final long serialVersionUID = -5314058338150089320L;

    /**
     * a constant for the number of configurations in this class. NOTE: This will need to be updated
     * if configurations are added to this class
     */
    public final static int NUMBER_OF_CELLULAR_CONFIGURATIONS = 8;

    /**
     * "Uplink Transmission Bandwidth Configuration in number of Resource Blocks" possible values:
     * 6, 15, 25, 50, 75, 100.
     */
    int uplinkBandwidth;

    /**
     * "Uplink E-UTRA Absolute Radio Frequency Channel Number (EARFCN) " "as per 3GPP 36.101 Section
     * 5.7.3. " possible values: 1800 - 24599
     */
    int uplinkCarrierFrequency;

    /**
     * "Downlink Transmission Bandwidth Configuration in number of Resource Blocks" possible values:
     * 6, 15, 25, 50, 75, 100.
     */
    int downlinkBandwidth;

    /**
     * "Downlink E-UTRA Absolute Radio Frequency Channel Number (EARFCN) " "as per 3GPP 36.101
     * Section 5.7.3. " possible values: 0 - 6599
     */
    int downlinkCarrierFrequency;

    /**
     * "Transmission power in dBm".
     */
    double cellTxPower;

    /**
     * "Loss (dB) in the Signal-to-Noise-Ratio due to " "non-idealities in the receiver.
     */
    double cellNoiseFigure;

    /** "Transmission power in dBm". */
    double cellTowerTxPower;

    /**
     * "Loss (dB) in the Signal-to-Noise-Ratio due to " "non-idealities in the receiver.
     */
    double cellTowerNoiseFigure;

    /**
     * Gets the uplink bandwidth.
     *
     * @return the uplink bandwidth
     */
    public int getUplinkBandwidth() {
        return uplinkBandwidth;
    }

    /**
     * Sets the uplink bandwidth.
     *
     * @param uplinkBandwidth the new uplink bandwidth
     */
    public void setUplinkBandwidth(int uplinkBandwidth) {
        this.uplinkBandwidth = uplinkBandwidth;
    }

    /**
     * Gets the uplink carrier frequency.
     *
     * @return the uplink carrier frequency
     */
    public int getUplinkCarrierFrequency() {
        return uplinkCarrierFrequency;
    }

    /**
     * Sets the uplink carrier frequency.
     *
     * @param uplinkCarrierFrequency the new uplink carrier frequency
     */
    public void setUplinkCarrierFrequency(int uplinkCarrierFrequency) {
        this.uplinkCarrierFrequency = uplinkCarrierFrequency;
    }

    /**
     * Gets the downlink bandwidth.
     *
     * @return the downlink bandwidth
     */
    public int getDownlinkBandwidth() {
        return downlinkBandwidth;
    }

    /**
     * Sets the downlink bandwidth.
     *
     * @param downlinkBandwidth the new downlink bandwidth
     */
    public void setDownlinkBandwidth(int downlinkBandwidth) {
        this.downlinkBandwidth = downlinkBandwidth;
    }

    /**
     * Gets the downlink carrier frequency.
     *
     * @return the downlink carrier frequency
     */
    public int getDownlinkCarrierFrequency() {
        return downlinkCarrierFrequency;
    }

    /**
     * Sets the downlink carrier frequency.
     *
     * @param downlinkCarrierFrequency the new downlink carrier frequency
     */
    public void setDownlinkCarrierFrequency(int downlinkCarrierFrequency) {
        this.downlinkCarrierFrequency = downlinkCarrierFrequency;
    }

    /**
     * Gets the cell tx power.
     *
     * @return the cell tx power
     */
    public double getCellTxPower() {
        return cellTxPower;
    }

    /**
     * Sets the cell tx power.
     *
     * @param cellTxPower the new cell tx power
     */
    public void setCellTxPower(double cellTxPower) {
        this.cellTxPower = cellTxPower;
    }

    /**
     * Gets the cell noise figure.
     *
     * @return the cell noise figure
     */
    public double getCellNoiseFigure() {
        return cellNoiseFigure;
    }

    /**
     * Sets the cell noise figure.
     *
     * @param cellNoiseFigure the new cell noise figure
     */
    public void setCellNoiseFigure(double cellNoiseFigure) {
        this.cellNoiseFigure = cellNoiseFigure;
    }

    /**
     * Gets the cell tower tx power.
     *
     * @return the cell tower tx power
     */
    public double getCellTowerTxPower() {
        return cellTowerTxPower;
    }

    /**
     * Sets the cell tower tx power.
     *
     * @param cellTowerTxPower the new cell tower tx power
     */
    public void setCellTowerTxPower(double cellTowerTxPower) {
        this.cellTowerTxPower = cellTowerTxPower;
    }

    /**
     * Gets the cell tower noise figure.
     *
     * @return the cell tower noise figure
     */
    public double getCellTowerNoiseFigure() {
        return cellTowerNoiseFigure;
    }

    /**
     * Sets the cell tower noise figure.
     *
     * @param cellTowerNoiseFigure the new cell tower noise figure
     */
    public void setCellTowerNoiseFigure(double cellTowerNoiseFigure) {
        this.cellTowerNoiseFigure = cellTowerNoiseFigure;
    }

}