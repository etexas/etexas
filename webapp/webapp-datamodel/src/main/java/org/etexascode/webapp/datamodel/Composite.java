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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.WaveSimType;
import org.etexascode.webapp.datamodel.device.Device;
import org.etexascode.webapp.datamodel.device.DeviceProfile;
import org.etexascode.webapp.datamodel.device.ReportDevice;
import org.etexascode.webapp.datamodel.topography.TopographyFeature;
import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A group of connected simulations that can be executed as a logical unit.
 * 
 * @author emyers
 * @author ttevendale
 */
@Entity
@Table(name = "composites")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Composite extends AbstractEntity implements ICopyable<Composite> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this composite. */
    private String name;

    /** The latitude (DD) for this composite. */
    @XmlTransient
    private double latitude;

    /** The longitude (DD) for this composite. */
    @XmlTransient
    private double longitude;

    /** The geographic calculator for this composite. */
    @XmlTransient
    private int geographicCalculator = UtilsLatLongConversion.GEODETIC2D;

    /** The propagation loss model for this composite. */
    @XmlTransient
    private PropagationLossModel propagationLossModel = PropagationLossModel.URBAN;

    /** The communications model for this composite. */
    @XmlTransient
    @Enumerated(EnumType.STRING)
    private WaveSimType communicationsModel = WaveSimType.IDEALIZED;

    /** The executable status for this composite. */
    @Column(name = "executable")
    private boolean isExecutable = true;

    /** The cell towers for this composite. */
    @XmlTransient
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<CellTower> cellTowers = new ArrayList<CellTower>();

    /** The executions for this composite. */
    @XmlTransient
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Execution> executions = new ArrayList<Execution>();

    /** The simulations for this composite. */
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private List<Simulation> simulations = new ArrayList<Simulation>();

    /** The report device for this composite. */
    @XmlTransient
    @JoinColumn(name = "report_device")
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private ReportDevice reportDevice = new ReportDevice();

    /** The devices for this composite. */
    @XmlTransient
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Device> devices = new ArrayList<Device>();

    /** The device profiles for this composite. */
    @XmlTransient
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<DeviceProfile> deviceProfiles = new ArrayList<DeviceProfile>();

    /** The lane mappings for this composite. */
    @XmlTransient
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<LaneMapping> laneMappings = new ArrayList<LaneMapping>();

    /** The topography features for this composite. */
    @XmlTransient
    @JoinColumn(name = "composite")
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<TopographyFeature> topographyFeatures = new ArrayList<TopographyFeature>();

    // TODO emyers - this should be lazy loaded

    /** The cellular configuration for this composite. */
    @XmlTransient
    @JoinColumn(name = "cellular_configuration")
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private CellularConfiguration cellularConfiguration = new CellularConfiguration();

    /**
     * Returns the name of this composite.
     * 
     * @return The string name of this composite.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this composite.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the executable status for this composite.
     * 
     * @return The boolean executable status for this composite.
     */
    public boolean isExecutable() {

        return isExecutable;
    }

    /**
     * Sets the executable status for this composite.
     * 
     * @param isExecutable The boolean executable status to set.
     */
    public void setExecutable(boolean isExecutable) {

        this.isExecutable = isExecutable;
    }

    /**
     * Returns the cell towers for this composite.
     * 
     * @return The list of cell towers for this composite.
     */
    public List<CellTower> getCellTowers() {

        return cellTowers;
    }

    /**
     * Sets the cell towers for this composite.
     * 
     * @param cellTowers The list of cell towers to set.
     */
    public void setCellTowers(List<CellTower> cellTowers) {

        this.cellTowers = cellTowers;
    }

    /**
     * Returns the executions for this composite.
     * 
     * @return The list of executions for this composite.
     */
    public List<Execution> getExecutions() {

        return executions;
    }

    /**
     * Sets the executions for this composite.
     * 
     * @param executions The list of executions to set.
     */
    public void setExecutions(List<Execution> executions) {

        this.executions = executions;
    }

    /**
     * Returns the simulations for this composite.
     * 
     * @return The list of simulations for this composite.
     */
    public List<Simulation> getSimulations() {

        return simulations;
    }

    /**
     * Sets the simulations for this composite.
     * 
     * @param simulations The list of simulations to set.
     */
    public void setSimulations(List<Simulation> simulations) {

        this.simulations = simulations;
    }

    /**
     * Returns the report device for this composite.
     * 
     * @return The report device for this composite.
     */
    public ReportDevice getReportDevice() {

        return reportDevice;
    }

    /**
     * Returns the devices for this composite.
     * 
     * @return The list of devices for this composite.
     */
    public List<Device> getDevices() {

        return devices;
    }

    /**
     * Sets the devices for this composite.
     * 
     * @param devices The list of devices to set.
     */
    public void setDevices(List<Device> devices) {

        this.devices = devices;
    }

    /**
     * Returns the device profiles for this composite.
     * 
     * @return The list of device profiles for this composite.
     */
    public List<DeviceProfile> getDeviceProfiles() {

        return deviceProfiles;
    }

    /**
     * Sets the device profiles for this composite.
     * 
     * @param deviceProfiles The list of device profiles to set.
     */
    public void setDeviceProfiles(List<DeviceProfile> deviceProfiles) {

        this.deviceProfiles = deviceProfiles;
    }

    /**
     * Returns the lane mappings for this composite.
     * 
     * @return The list of lane mappings for this composite.
     */
    public List<LaneMapping> getLaneMappings() {

        return laneMappings;
    }

    /**
     * Sets the lane mappings for this composite.
     * 
     * @param laneMappings The list of lane mappings to set.
     */
    public void setLaneMappings(List<LaneMapping> laneMappings) {

        this.laneMappings = laneMappings;
    }

    /**
     * Returns the topography features for this composite.
     * 
     * @return The list of topography features for this composite.
     */
    public List<TopographyFeature> getTopographyFeatures() {

        return topographyFeatures;
    }

    /**
     * Sets the topography features for this composite.
     * 
     * @param topographyFeatures The list of topography features to set.
     */
    public void setTopographyFeatures(List<TopographyFeature> topographyFeatures) {

        this.topographyFeatures = topographyFeatures;
    }

    /**
     * Returns the cellular configuration for this composite.
     * 
     * @return The cellular configuration for this composite.
     */
    public CellularConfiguration getCellularConfiguration() {

        return cellularConfiguration;
    }

    /**
     * Sets the cellular configuration for this composite.
     * 
     * @param cellularConfiguration The cellular configuration to set.
     */
    public void setCellularConfiguration(CellularConfiguration cellularConfiguration) {

        this.cellularConfiguration = cellularConfiguration;
    }

    /**
     * Returns the latitude (DD) for this composite.
     * 
     * @return The double latitude (DD) for this composite.
     */
    public double getLatitude() {

        return latitude;
    }

    /**
     * Sets the latitude (DD) for this composite.
     * 
     * @param latitude The double latitude (DD) to set.
     */
    public void setLatitude(double latitude) {

        this.latitude = latitude;
    }

    /**
     * Returns the longitude (DD) for this composite.
     * 
     * @return The double longitude (DD) for this composite.
     */
    public double getLongitude() {

        return longitude;
    }

    /**
     * Sets the longitude (DD) for this composite.
     * 
     * @param longitude The double longitude (DD) to set.
     */
    public void setLongitude(double longitude) {

        this.longitude = longitude;
    }

    /**
     * Returns the geographic calculator for this composite.
     * 
     * @return The integer geographic calculator for this composite.
     */
    public int getGeographicCalculator() {

        return geographicCalculator;
    }

    /**
     * Sets the geographic calculator for this composite.
     * 
     * @param geographicCalculator The integer geographic calculator to set.
     */
    public void setGeographicCalculator(int geographicCalculator) {

        this.geographicCalculator = geographicCalculator;
    }

    /**
     * Getter for the propagation loss model.
     * 
     * @return The propagation loss model.
     */
    public PropagationLossModel getPropagationLossModel() {

        return propagationLossModel;
    }

    /**
     * Setter for the propagation loss model.
     * 
     * @param propagationLossModel The propagation loss model for this execution.
     */
    public void setPropagationLossModel(PropagationLossModel propagationLossModel) {

        this.propagationLossModel = propagationLossModel;
    }

    /**
     * Returns the communications model for this composite.
     * 
     * @return The WAVE communications model for this composite.
     */
    public WaveSimType getCommunicationsModel() {

        return communicationsModel;
    }

    /**
     * Sets the communications model for this composite.
     * 
     * @param communicationsModel The WAVE communications model to set.
     */
    public void setCommunicationsModel(WaveSimType communicationsModel) {

        this.communicationsModel = communicationsModel;
    }

    /**
     * Returns the maximum number of steps for this composite.
     * 
     * @return The long maximum number of steps for this composite.
     */
    public long getMaximumSteps() {

        // TODO: emyers - this should be calculated based on all simulations
        return simulations.get(0).getMaximumSteps();
    }

    /**
     * Returns the step size for this composite.
     * 
     * @return The double step size for this composite.
     */
    public double getStepSize() {

        // TODO: emyers - this should be calculated based on all simulations
        return simulations.get(0).getStepSize();
    }

    /**
     * The method to copy this composite
     * 
     * @return The copied composite
     */
    @Override
    public Composite copy() {

        Composite composite = new Composite();
        composite.setCellularConfiguration(this.getCellularConfiguration().copy());
        composite.setCommunicationsModel(this.getCommunicationsModel());

        List<CellTower> cellTowerCopies = new ArrayList<CellTower>(cellTowers.size());

        for (CellTower cellTower : cellTowers) {

            cellTowerCopies.add(cellTower.copy());
        }

        composite.setCellTowers(cellTowerCopies);

        List<Device> deviceCopies = new ArrayList<Device>(devices.size());

        for (Device device : devices) {

            deviceCopies.add(device.copy());
        }

        composite.setDevices(deviceCopies);

        List<DeviceProfile> deviceProfileCopies = new ArrayList<DeviceProfile>(deviceProfiles.size());

        for (DeviceProfile deviceProfile : deviceProfiles) {

            deviceProfileCopies.add(deviceProfile.copy());
        }

        composite.setDeviceProfiles(deviceProfileCopies);

        List<TopographyFeature> topographyFeatureCopies = new ArrayList<TopographyFeature>(topographyFeatures.size());

        for (TopographyFeature feature : topographyFeatures) {

            topographyFeatureCopies.add(feature.copy());
        }

        composite.setTopographyFeatures(topographyFeatureCopies);
        composite.setGeographicCalculator(this.getGeographicCalculator());
        composite.setPropagationLossModel(this.getPropagationLossModel());
        composite.setLatitude(this.getLatitude());
        composite.setLongitude(this.getLongitude());

        List<Simulation> simulationCopies = new ArrayList<Simulation>(simulations.size());

        for (Simulation simulation : simulations) {

            simulationCopies.add(simulation.copy());
        }

        composite.setSimulations(simulationCopies);

        return composite;
    }
}