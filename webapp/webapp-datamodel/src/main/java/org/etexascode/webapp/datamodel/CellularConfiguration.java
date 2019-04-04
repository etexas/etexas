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
package org.etexascode.webapp.datamodel;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A cellular communications configuration.
 * 
 * @author ttevendale
 * @author emyers
 */
@Entity
@Table(name = "cellular_configurations")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class CellularConfiguration extends AbstractEntity implements ICopyable<CellularConfiguration> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /**
     * The uplink transmission bandwidth (resource blocks) for this cellular configuration. The
     * possible values are {6, 15, 25, 50, 75, 100}.
     */
    @Column(name = "uplink_bandwidth")
    private int uplinkBandwidth;

    /**
     * The downlink transmission bandwidth (resource blocks) for this cellular configuration. The
     * possible values are {6, 15, 25, 50, 75, 100}.
     */
    @Column(name = "downlink_bandwidth")
    private int downlinkBandwidth;

    /**
     * The uplink E-UTRA Absolute Radio Frequency Channel Number (EARFCN) for this cellular
     * configuration (3GPP 36.101 Section 5.7.3). The possible values are [18000 - 24599].
     */
    @Column(name = "uplink_frequency")
    private int uplinkCarrierFrequency;

    /**
     * The downlink E-UTRA Absolute Radio Frequency Channel Number (EARFCN) for this cellular
     * configuration (3GPP 36.101 Section 5.7.3). The possible values are [0 - 6599].
     */
    @Column(name = "downlink_frequency")
    private int downlinkCarrierFrequency;

    /**
     * The loss (dB) in the signal-to-noise ratio for cell towers in this cellular configuration.
     */
    @Column(name = "cell_tower_noise")
    private double cellTowerNoise;

    /**
     * The transmission power (dBm) for cell towers in this cellular configuration.
     */
    @Column(name = "cell_tower_power")
    private double cellTowerPower;

    /**
     * The loss (dB) in the signal-to-noise ratio for cellular devices in this cellular
     * configuration.
     */
    @Column(name = "cellular_device_noise")
    private double cellularDeviceNoise;

    /**
     * The transmission power (dBm) for cellular devices in this cellular configuration.
     */
    @Column(name = "cellular_device_power")
    private double cellularDevicePower;

    /**
     * Creates a new <code>CellularConfiguration</code> with the NS3 default values.
     */
    public CellularConfiguration() {

        uplinkBandwidth = 25;
        downlinkBandwidth = 25;
        uplinkCarrierFrequency = 18100;
        downlinkCarrierFrequency = 100;
        cellTowerNoise = 5;
        cellTowerPower = 30;
        cellularDeviceNoise = 9;
        cellularDevicePower = 10;
    }

    /**
     * Returns the uplink transmission bandwidth (resource blocks) for this cellular configuration.
     * 
     * @return The integer uplink transmission bandwidth (resource blocks) for this cellular
     *         configuration.
     */
    public int getUplinkBandwidth() {

        return uplinkBandwidth;
    }

    /**
     * Sets the uplink transmission bandwidth (resource blocks) for this cellular configuration.
     * 
     * @param uplinkBandwidth The integer uplink transmission bandwidth (resource blocks) to set.
     */
    public void setUplinkBandwidth(int uplinkBandwidth) {

        this.uplinkBandwidth = uplinkBandwidth;
    }

    /**
     * Returns the downlink transmission bandwidth (resource blocks) for this cellular
     * configuration.
     * 
     * @return The integer downlink transmission bandwidth (resource blocks) for this cellular
     *         configuration.
     */
    public int getDownlinkBandwidth() {

        return downlinkBandwidth;
    }

    /**
     * Sets the downlink transmission bandwidth (resource blocks) for this cellular configuration.
     * 
     * @param downlinkBandwidth The integer downlink transmission bandwidth (resource blocks) to
     *        set.
     */
    public void setDownlinkBandwidth(int downlinkBandwidth) {

        this.downlinkBandwidth = downlinkBandwidth;
    }

    /**
     * Returns the uplink EARFCN for this cellular configuration.
     * 
     * @return The integer uplink EARFCN for this cellular configuration.
     */
    public int getUplinkCarrierFrequency() {

        return uplinkCarrierFrequency;
    }

    /**
     * Sets the uplink EARFCN for this cellular configuration.
     * 
     * @param uplinkCarrierFrequency The integer uplink EARFCN to set.
     */
    public void setUplinkCarrierFrequency(int uplinkCarrierFrequency) {

        this.uplinkCarrierFrequency = uplinkCarrierFrequency;
    }

    /**
     * Returns the downlink EARFCN for this cellular configuration.
     * 
     * @return The integer downlink EARFCN for this cellular configuration.
     */
    public int getDownlinkCarrierFrequency() {

        return downlinkCarrierFrequency;
    }

    /**
     * Sets the downlink EARFCN for this cellular configuration.
     * 
     * @param downlinkCarrierFrequency The integer downlink EARFCN to set.
     */
    public void setDownlinkCarrierFrequency(int downlinkCarrierFrequency) {

        this.downlinkCarrierFrequency = downlinkCarrierFrequency;
    }

    /**
     * Returns the loss (dB) in the signal-to-noise ratio for cell towers in this cellular
     * configuration.
     * 
     * @return The double loss (dB) in the signal-to-noise ratio for cell towers in this cellular
     *         configuration.
     */
    public double getCellTowerNoise() {

        return cellTowerNoise;
    }

    /**
     * Sets the loss (dB) in the signal-to-noise ratio for cell towers in this cellular
     * configuration.
     * 
     * @param cellTowerNoise The double loss (dB) in the signal-to-noise ratio for cell towers to
     *        set.
     */
    public void setCellTowerNoise(double cellTowerNoise) {

        this.cellTowerNoise = cellTowerNoise;
    }

    /**
     * Returns the transmission power (dBm) for cell towers in this cellular configuration.
     * 
     * @return The double transmission power (dBm) for cell towers in this cellular configuration.
     */
    public double getCellTowerPower() {

        return cellTowerPower;
    }

    /**
     * Sets the transmission power (dBm) for cell towers in this cellular configuration.
     * 
     * @param cellTowerPower The double transmission power (dBm) for cell towers to set.
     */
    public void setCellTowerPower(double cellTowerPower) {

        this.cellTowerPower = cellTowerPower;
    }

    /**
     * Returns the loss (dB) in the signal-to-noise ratio for cellular devices in this cellular
     * configuration.
     * 
     * @return The double loss (dB) in the signal-to-noise ratio for cellular devices in this
     *         cellular configuration.
     */
    public double getCellularDeviceNoise() {

        return cellularDeviceNoise;
    }

    /**
     * Sets the loss (dB) in the signal-to-noise ratio for cellular devices in this cellular
     * configuration.
     * 
     * @param cellularDeviceNoise The double loss (dB) in the signal-to-noise ratio for cellular
     *        devices to set.
     */
    public void setCellularDeviceNoise(double cellularDeviceNoise) {

        this.cellularDeviceNoise = cellularDeviceNoise;
    }

    /**
     * Returns the transmission power (dBm) for cellular devices in this cellular configuration.
     * 
     * @return The double transmission power (dBm) for cellular devices in this cellular
     *         configuration.
     */
    public double getCellularDevicePower() {

        return cellularDevicePower;
    }

    /**
     * Sets the transmission power (dBm) for cellular devices in this cellular configuration.
     * 
     * @param cellularDevicePower The double transmission power (dBm) for cellular devices to set.
     */
    public void setCellularDevicePower(double cellularDevicePower) {

        this.cellularDevicePower = cellularDevicePower;
    }

    @Override
    public CellularConfiguration copy() {

        CellularConfiguration cellConfig = new CellularConfiguration();
        cellConfig.setCellTowerNoise(this.getCellTowerNoise());
        cellConfig.setCellTowerPower(this.getCellTowerPower());
        cellConfig.setCellularDeviceNoise(this.getCellularDeviceNoise());
        cellConfig.setCellularDevicePower(this.getCellularDevicePower());
        cellConfig.setDownlinkBandwidth(this.getDownlinkBandwidth());
        cellConfig.setDownlinkCarrierFrequency(this.getDownlinkCarrierFrequency());
        cellConfig.setUplinkBandwidth(this.getUplinkBandwidth());
        cellConfig.setUplinkCarrierFrequency(this.getUplinkCarrierFrequency());

        return cellConfig;
    }
}