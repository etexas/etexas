package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexasSimulation (has subtypes for Uploaded and
 * Template)
 *
 * @author cbulloss
 */
public class Simulation extends ETexasBaseEntity {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -8021833733583674000L;

    /**
     * Default Constructor.
     */
    public Simulation() {
        super();
        this.entityType = ETexasEntityType.SIMULATION;
    }

    /**
     * Enumeration of simulation foundation (either from template or upload)
     *
     * @author llaroussini
     */
    public enum SimulationFoundation {
        /**
         * Template-based simulation type
         */
        TEMPLATE,
        /**
         * Upload-based simulation type
         */
        UPLOAD;
    }

    /**
     * Enumeration of simulation types (model-type simulation is using)
     *
     * @author llaroussini
     */
    public enum SimulationType {
        /**
         * ETEXAS simulation type
         */
        ETEXAS("TEXAS"),
        /**
         * Upload-based simulation type
         */
        PLAYBACK("Playback");

        /**
         * The label of the simulation type as it appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        SimulationType(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the Simulation Type as it is displayed
         * in the Web UI
         *
         * @return The label of the Simulation Type
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * The simulation name
     */
    private String name;

    /**
     * The simulation foundation
     */
    private SimulationFoundation simFoundation;

    /**
     * The simulation type
     */
    private SimulationType simType;

    /**
     * The user which created or will create this simulation
     */
    private UUID user;

    /**
     * The list of RSE Devices associated with this simulation
     */
    private List<UUID> rseDevices;

    /**
     * The list of OBU Devices associated with this simulation
     */
    private List<UUID> obuDevices;

    /**
     * The list of cellular Devices associated with this simulation
     */
    private List<UUID> cellDevices;

    /**
     * The list of fixed cellular devices associated with this simulation
     */
    private List<UUID> fixedCellDevices;

    /**
     * The list of executions associated with this simulation
     */
    private List<UUID> executions;

    /**
     * The list of lanes associated with this simulation
     */
    private List<UUID> lanes;

    /**
     * The list of Detectors associated with this simulation
     */
    private List<UUID> detectors = new ArrayList<UUID>();

    /**
     * Gets the simulation name
     *
     * @return the simulation name
     */
    public String getName() {
        if (this.name == null) {
            this.setName("");
        }
        return this.name;
    }

    /**
     * Sets the simulation name
     *
     * @param name the simulation name to set
     */
    public void setName(String name) {
        if (name == null) {
            this.name = "";
        }
        else {
            this.name = name;
        }
    }

    /**
     * Gets the simulation foundation (template or upload)
     *
     * @return the simulation foundation
     */
    public SimulationFoundation getSimFoundation() {
        return this.simFoundation;
    }

    /**
     * Sets the foundation of the simulation
     *
     * @param simFoundation the foundation of the simulation to set
     */
    public void setSimFoundation(SimulationFoundation simFoundation) {
        this.simFoundation = simFoundation;
    }

    /**
     * Gets the simulation type (TEXAS or Playback)
     *
     * @return the simulation type
     */
    public SimulationType getSimType() {
        return this.simType;
    }

    /**
     * Sets the type of simulation
     *
     * @param simType the type of simulation to set
     */
    public void setSimType(SimulationType simType) {
        this.simType = simType;
    }

    /**
     * Gets the UUID list for RSE devices for this simulation
     *
     * @return list of UUID's for RSE devices
     */
    public List<UUID> getRSEDeviceIds() {
        return this.rseDevices;
    }

    /**
     * Gets the simulation's RSE devices
     *
     * @return simulation's RSE devices
     */
    public List<RSEDevice> getRSEDevices() {
        List<RSEDevice> rseList = new ArrayList<RSEDevice>();
        if (this.rseDevices == null) {
            return rseList;
        }
        for (UUID id : this.rseDevices) {
            rseList.add(ETexasEntityManager.getEntity(id, RSEDevice.class));
        }
        return rseList;
    }

    /**
     * Sets the device list for this simulation
     *
     * @param devices the list of devices to set
     */
    public void setRSEDevices(List<RSEDevice> devices) {
        List<UUID> uuidList = new ArrayList<UUID>(devices.size());
        for (RSEDevice device : devices) {
            UUID rseUUID = device.getUuid();
            uuidList.add(rseUUID);
        }
        this.rseDevices = uuidList;
    }

    /**
     * Adds a device to this simulation's list of devices
     *
     * @param device the device to add
     */
    public void addRSEDevice(RSEDevice device) {
        if (this.getRSEDevices() == null) {
            this.setRSEDevices(new ArrayList<RSEDevice>(1));
        }
        this.getRSEDevices().add(device);
        if (device.getSimulation() == null || !device.getSimulation().equals(this)) {
            device.setSimulation(this);
        }
    }

    /**
     * Gets the UUID list for OBU devices for this simulation
     *
     * @return list of UUID's for OBU devices
     */
    public List<UUID> getOBUDeviceIds() {
        return this.obuDevices;
    }

    /**
     * Gets the simulation's OBU devices
     *
     * @return simulation's OBU devices
     */
    public List<OBUDevice> getOBUDevices() {
        if (this.obuDevices == null) {
            return null;
        }
        List<OBUDevice> obuList = new ArrayList<OBUDevice>(obuDevices.size());
        for (UUID id : this.obuDevices) {
            obuList.add(ETexasEntityManager.getEntity(id, OBUDevice.class));
        }
        return obuList;
    }

    /**
     * Sets the OBU device list for this simulation
     *
     * @param obuDevices the list of OBU devices to set
     */
    public void setOBUDevices(List<OBUDevice> devices) {
        List<UUID> uuidList = new ArrayList<UUID>(devices.size());
        for (OBUDevice device : devices) {
            UUID obuUUID = device.getUuid();
            uuidList.add(obuUUID);
        }
        this.obuDevices = uuidList;
    }

    /**
     * Adds a OBU device to this simulation's list of devices
     *
     * @param obuDevice the device to add
     */
    public void addOBUDevice(OBUDevice obuDevice) {
        if (this.getOBUDevices() == null) {
            this.setOBUDevices(new ArrayList<OBUDevice>(1));
        }
        this.getOBUDevices().add(obuDevice);
        if (obuDevice.getSimulation() == null || !obuDevice.getSimulation().equals(this)) {
            obuDevice.setSimulation(this);
        }
    }

    /**
     * Gets the UUID list for cellular devices for this simulation
     *
     * @return list of UUID's for cellular devices
     */
    public List<UUID> getCellularDeviceIds() {
        return this.cellDevices;
    }

    /**
     * Gets the simulation's cellular devices
     *
     * @return simulation's cellular devices
     */
    public List<CellularDevice> getCellularDevices() {
        if (this.cellDevices == null) {
            return null;
        }
        List<CellularDevice> cellList = new ArrayList<CellularDevice>(cellDevices.size());
        for (UUID id : this.cellDevices) {
            cellList.add(ETexasEntityManager.getEntity(id, CellularDevice.class));
        }
        return cellList;
    }

    /**
     * Sets the cellular device list for this simulation
     *
     * @param cellDevices the list of cellular devices to set
     */
    public void setCellularDevices(List<CellularDevice> devices) {
        List<UUID> uuidList = new ArrayList<UUID>(devices.size());
        for (CellularDevice device : devices) {
            UUID cellUUID = device.getUuid();
            uuidList.add(cellUUID);
        }
        this.cellDevices = uuidList;
    }

    /**
     * Adds a cellular device to this simulation's list of devices
     *
     * @param cellDevice the device to add
     */
    public void addCellularDevice(CellularDevice cellDevice) {
        if (this.getCellularDevices() == null) {
            this.setCellularDevices(new ArrayList<CellularDevice>(1));
        }
        this.getCellularDevices().add(cellDevice);
        if (cellDevice.getSimulation() == null || !cellDevice.getSimulation().equals(this)) {
            cellDevice.setSimulation(this);
        }
    }

    /**
     * Gets the UUID list for fixed cellular devices for this simulation
     *
     * @return list of UUID's for fixed cellular devices
     */
    public List<UUID> getFixedCellularDeviceIds() {
        return this.fixedCellDevices;
    }

    /**
     * Gets the simulation's fixed cellular devices
     *
     * @return simulation's fixed cellular devices
     */
    public List<FixedCellularDevice> getFixedCellularDevices() {
        List<FixedCellularDevice> fixedCellList = new ArrayList<FixedCellularDevice>();
        if (this.fixedCellDevices == null) {
            return fixedCellList;
        }
        for (UUID id : this.fixedCellDevices) {
            fixedCellList.add(ETexasEntityManager.getEntity(id, FixedCellularDevice.class));
        }
        return fixedCellList;
    }

    /**
     * Sets the fixed cellular device list for this simulation
     *
     * @param fixedCellDevices the list of fixed cellular devices to set
     */
    public void setFixedCellularDevices(List<FixedCellularDevice> devices) {
        List<UUID> uuidList = new ArrayList<UUID>(devices.size());
        for (FixedCellularDevice device : devices) {
            UUID cellUUID = device.getUuid();
            uuidList.add(cellUUID);
        }
        this.fixedCellDevices = uuidList;
    }

    /**
     * Adds a fixed cellular device to this simulation's list of devices
     *
     * @param fixedCellDevice the device to add
     */
    public void addFixedCellularDevice(FixedCellularDevice fixedCellDevice) {
        if (this.getFixedCellularDevices() == null) {
            this.setFixedCellularDevices(new ArrayList<FixedCellularDevice>(1));
        }
        this.getFixedCellularDevices().add(fixedCellDevice);
        if (fixedCellDevice.getSimulation() == null || !fixedCellDevice.getSimulation().equals(this)) {
            fixedCellDevice.setSimulation(this);
        }
    }

    /**
     * Gets the UUID for the user with which this simulation is associated
     *
     * @return the UUID for the user with which this simulation is associated
     *         (the user which has created or will create the simulation)
     */
    public UUID getUserId() {
        return this.user;
    }

    /**
     * Gets the user with which this simulation is associated
     *
     * @return the user with which this simulation is associated (the user which
     *         has created or will create the simulation)
     */
    public ETexasUser getUser() {
        return ETexasEntityManager.getEntity(getUserId(), ETexasUser.class);
    }

    /**
     * Sets ths user for this simulation
     *
     * @param user the user which has or will create this simulation
     */
    public void setUser(ETexasUser user) {
        UUID userUUID = user.getUuid();
        this.user = userUUID;
    }

    /**
     * Gets the UUID list of executions for this simulation
     *
     * @return the UUID executions list
     */
    public List<UUID> getExecutionIds() {
        return this.executions;
    }

    /**
     * Gets the simulation's associated executions
     *
     * @return simulation's associated executions
     */
    public List<Execution> getExecutions() {
        List<Execution> executionList = new ArrayList<Execution>(executions.size());
        for (UUID id : this.executions) {
            executionList.add(ETexasEntityManager.getEntity(id, Execution.class));
        }
        return executionList;
    }

    /**
     * Sets the execution list for this simulation
     *
     * @param executions the list of executions to set
     */
    public void setExecutions(List<Execution> executions) {
        List<UUID> uuidList = new ArrayList<UUID>(executions.size());
        for (Execution execution : executions) {
            UUID executionUUID = execution.getUuid();
            uuidList.add(executionUUID);
        }
        this.executions = uuidList;
    }

    /**
     * Adds an execution to this simulation's list of executions
     *
     * @param execution the execution to add
     */
    public void addExecution(Execution execution) {
        if (this.getExecutions() == null) {
            this.setExecutions(new ArrayList<Execution>(1));
        }
        this.getExecutions().add(execution);
        if (execution.getSimulation() == null || !execution.getSimulation().equals(this)) {
            execution.setSimulation(this);
        }
    }

    /**
     * Gets the UUID list of lanes for this simulation
     *
     * @return the UUID lanes list
     */
    public List<UUID> getLaneIds() {
        return this.lanes;
    }

    /**
     * Gets the simulation's associated lanes
     *
     * @return simulation's associated lanes
     */
    public List<Lane> getLanes() {
        List<Lane> lanesList = new ArrayList<Lane>(lanes.size());
        for (UUID id : this.lanes) {
            lanesList.add(ETexasEntityManager.getEntity(id, Lane.class));
        }
        return lanesList;
    }

    /**
     * Sets the lanes list for this simulation
     *
     * @param lanes the list of lanes to set
     */
    public void setLanes(List<Lane> lanes) {
        List<UUID> uuidList = new ArrayList<UUID>(lanes.size());
        for (Lane lane : lanes) {
            UUID laneUUID = lane.getUuid();
            uuidList.add(laneUUID);
        }
        this.lanes = uuidList;
    }

    /**
     * Adds a lane to this simulation's list of lanes
     *
     * @param lane the lane to add
     */
    public void addLane(Lane lane) {
        if (this.getLanes() == null) {
            this.setLanes(new ArrayList<Lane>(1));
        }
        this.getLanes().add(lane);
        if (lane.getSimulation() == null || !lane.getSimulation().equals(this)) {
            lane.setSimulation(this);
        }
    }

    /**
     * Gets the UUID list of detectors for this simulation
     *
     * @return the UUID detectors list
     */
    public List<UUID> getDetectorIds() {
        return this.detectors;
    }

    /**
     * Gets the simulation's associated detectors
     *
     * @return simulation's associated detectors
     */
    public List<Detector> getDetectors() {
        List<Detector> detectorList = new ArrayList<Detector>(detectors.size());
        for (UUID id : this.detectors) {
            detectorList.add(ETexasEntityManager.getEntity(id, Detector.class));
        }
        return detectorList;
    }

    /**
     * Sets the detector list for this simulation
     *
     * @param detectors -the list of detectors to set
     */
    public void setDetectors(List<Detector> detectors) {
        List<UUID> detectorList = new ArrayList<UUID>(detectors.size());
        for (Detector detector : detectors) {
            UUID detectorUUID = detector.getUuid();
            detectorList.add(detectorUUID);
        }
        this.detectors = detectorList;
    }

    /**
     * Adds a detector to this simulation's list of detectors
     *
     * @param detector the detector to add
     */
    public void addDetector(Detector detector) {
        if (this.getDetectors() == null) {
            this.setDetectors(new ArrayList<Detector>(1));
        }
        this.getDetectors().add(detector);
        if (detector.getSimulation() == null || !detector.getSimulation().equals(this)) {
            detector.setSimulation(this);
        }
    }
}
