package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditCellularOptionsModal.BandwidthValues;

/**
 * Entity class representing a configuration of cellular options
 *
 * @author llaroussini
 */
public class CellularOptionsConfiguration extends ETexasBaseEntity {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = -2618026887281099332L;

    /**
     * Default Constructor.
     */
    public CellularOptionsConfiguration() {
        super();
        this.entityType = ETexasEntityType.ADVANCED_CELL_OPTION_CONFIGURATION;
    }

    /**
     * Uplink Bandwidth
     */
    private BandwidthValues uplinkBandwidth;

    /**
     * Uplink Carrier Frequency
     */
    private int uplinkCarrierFrequency;

    /**
     * Downlink Bandwidth
     */
    private BandwidthValues downlinkBandwidth;

    /**
     * Downlink Carrier Frequency
     */
    private int downlinkCarrierFrequency;

    /**
     * Cell Power
     */
    private int cellPower;

    /**
     * Cell Noise
     */
    private int cellNoise;

    /**
     * Cell Tower Power
     */
    private int cellTowerPower;

    /**
     * Cell Tower Noise
     */
    private int cellTowerNoise;

    /**
     * The simulation with which this configuration is associated
     */
    private UUID simulation;

    /**
     * Gets the uplink bandwidth
     *
     * @return the uplink bandwidth
     */
    public BandwidthValues getUplinkBandwidth() {
        return this.uplinkBandwidth;
    }

    /**
     * Sets the uplink bandwidth
     *
     * @param uplinkBandwidth the uplink bandwidth to set
     */
    public void setUplinkBandwith(BandwidthValues uplinkBandwidth) {
        this.uplinkBandwidth = uplinkBandwidth;
    }

    /**
     * Gets the uplink carrier frequency
     *
     * @return the uplink carrier frequency
     */
    public int getUplinkCarrierFrequency() {
        return this.uplinkCarrierFrequency;
    }

    /**
     * Sets the uplink carrier frequency
     *
     * @param uplinkCarrierFrequency the uplink carrier frequency to set
     */
    public void setUplinkCarrierFrequency(int uplinkCarrierFrequency) {
        this.uplinkCarrierFrequency = uplinkCarrierFrequency;
    }

    /**
     * Gets the downlink bandwidth
     *
     * @return the downlink bandwidth
     */
    public BandwidthValues getDownlinkBandwidth() {
        return this.downlinkBandwidth;
    }

    /**
     * Sets the downlink bandwidth
     *
     * @param downlinkBandwidth the downlink bandwidth to set
     */
    public void setDownlinkBandwith(BandwidthValues downlinkBandwidth) {
        this.downlinkBandwidth = downlinkBandwidth;
    }

    /**
     * Gets the downlink carrier frequency
     *
     * @return the downlink carrier frequency
     */
    public int getDownlinkCarrierFrequency() {
        return this.downlinkCarrierFrequency;
    }

    /**
     * Sets the downlink carrier frequency
     *
     * @param downlinkCarrierFrequency the downlink carrier frequency to set
     */
    public void setDownlinkCarrierFrequency(int downlinkCarrierFrequency) {
        this.downlinkCarrierFrequency = downlinkCarrierFrequency;
    }

    /**
     * Gets the cell power
     *
     * @return the cell power
     */
    public int getCellPower() {
        return this.cellPower;
    }

    /**
     * Sets the cell power
     *
     * @param cellPower the cell power to set
     */
    public void setCellPower(int cellPower) {
        this.cellPower = cellPower;
    }

    /**
     * Gets the cell noise
     *
     * @return the cell noise
     */
    public int getCellNoise() {
        return this.cellNoise;
    }

    /**
     * Sets the cell noise
     *
     * @param cellNoise the cell noise to set
     */
    public void setCellNoise(int cellNoise) {
        this.cellNoise = cellNoise;
    }

    /**
     * Gets the cell tower power
     *
     * @return the cell tower power
     */
    public int getCellTowerPower() {
        return this.cellTowerPower;
    }

    /**
     * Sets the cell tower power
     *
     * @param cellTowerPower the cell tower power to set
     */
    public void setCellTowerPower(int cellTowerPower) {
        this.cellTowerPower = cellTowerPower;
    }

    /**
     * Gets the cell tower noise
     *
     * @return the cell tower noise
     */
    public int getCellTowerNoise() {
        return this.cellTowerNoise;
    }

    /**
     * Sets the cell tower noise
     *
     * @param cellTowerNoise the cell tower noise to set
     */
    public void setCellTowerNoise(int cellTowerNoise) {
        this.cellTowerNoise = cellTowerNoise;
    }

    /**
     * Gets the simulation UUID with which this configuration is associated
     *
     * @return the UUID of the simulation with which this configuration is
     *         associated
     */
    public UUID getSimulationID() {
        return this.simulation;
    }

    /**
     * Gets the simulation with which this configuration is associated
     *
     * @return the simulation with which this configuration is associated
     */
    public Simulation getSimulation() {
        return ETexasEntityManager.getEntity(getSimulationID(), Simulation.class);

    }

    /**
     * Sets the simulation for this configuration
     *
     * @param sim the simulation to set
     */
    public void setSimulation(Simulation sim) {
        UUID simUUID = sim.getUuid();
        this.simulation = simUUID;
    }
}
