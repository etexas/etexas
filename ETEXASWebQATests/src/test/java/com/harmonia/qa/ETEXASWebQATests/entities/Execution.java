package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas Execution
 *
 * @author llaroussini
 */
public class Execution extends ETexasBaseEntity {

    /**
     * Auto generate serial UID
     */
    private static final long serialVersionUID = 3855980534812061545L;

    /**
     * Default Constructor.
     */
    public Execution() {
        super();
        this.entityType = ETexasEntityType.EXECUTION;
    }

    /**
     * Enumeration of statuses
     *
     * @author llaroussini
     */
    public enum Status {
        /**
         * Not Started status
         */
        NOT_STARTED("No Started"),
        /**
         * In Progress status
         */
        IN_PROGRESS("In Progress"),
        /**
         * Error status
         */
        ERROR("Error"),
        /**
         * Completed status
         */
        COMPLETED("Completed");

        /**
         * The label of the status as it appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        Status(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the Status as it is displayed in the
         * Web UI
         *
         * @return The label of the Status
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * The simulation associated with the execution
     */
    private UUID simulation;

    /**
     * The execution status
     */
    private Status status;

    /**
     * The execution name (set by the system when execution is created)
     */
    private String name;

    /**
     * The lane change commands associated with the execution
     */
    private List<UUID> laneCommands;

    /**
     * The speed change commands associated with the execution
     */
    private List<UUID> speedCommands;

    /**
     * The signal change commands associated with the execution
     */
    private List<UUID> signalCommands;

    /**
     * The vehicle injection commands associated with the execution
     */
    private List<UUID> vehicleInjectionCommands;

    /**
     * Gets the simulation UUID with which this execution is associated
     *
     * @return the UUID of the simulation with which this execution is
     *         associated
     */
    public UUID getSimulationID() {
        return this.simulation;
    }

    /**
     * Gets the simulation with which this execution is associated
     *
     * @return the simulation with which this execution is associated
     */
    public Simulation getSimulation() {
        return ETexasEntityManager.getEntity(getSimulationID(), Simulation.class);

    }

    /**
     * Sets the simulation for this execution
     *
     * @param sim the simulation to set
     */
    public void setSimulation(Simulation sim) {
        UUID simUUID = sim.getUuid();
        this.simulation = simUUID;
    }

    /**
     * Gets the execution's status
     *
     * @return the status
     */
    public Status getStatus() {
        return this.status;
    }

    /**
     * Sets the execution's status
     *
     * @param status the status to set
     */
    public void setStatus(Status status) {
        this.status = status;
    }

    /**
     * Gets the execution's name
     *
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Sets the execution's name
     *
     * @param status the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the UUID list of lane change commands for this execution
     *
     * @return the UUID lane change command list
     */
    public List<UUID> getLaneChangeCommandIds() {
        return this.laneCommands;
    }

    /**
     * Gets the execution's associated speed commands
     *
     * @return execution's associated speed commands
     */
    public List<SpeedCommand> getSpeedCommands() {
        if (speedCommands == null) {
            return null;
        }
        else {
            List<SpeedCommand> speedCommandList = new ArrayList<SpeedCommand>(speedCommands.size());
            for (UUID id : this.speedCommands) {
                speedCommandList.add(ETexasEntityManager.getEntity(id, SpeedCommand.class));
            }
            return speedCommandList;
        }
    }

    /**
     * Gets the execution's associated lane change commands
     *
     * @return execution's associated lane change commands
     */
    public List<LaneChangeCommand> getLaneChangeCommands() {
        List<LaneChangeCommand> laneCommandList = new ArrayList<LaneChangeCommand>(laneCommands.size());
        for (UUID id : this.laneCommands) {
            laneCommandList.add(ETexasEntityManager.getEntity(id, LaneChangeCommand.class));
        }
        return laneCommandList;
    }

    /**
     * Sets the speed command list for this execution
     *
     * @param speedCommands the list of speed commands to set
     */
    public void setSpeedCommands(List<SpeedCommand> speedCommands) {
        List<UUID> uuidList = new ArrayList<UUID>(speedCommands.size());
        for (SpeedCommand speedCommand : speedCommands) {
            UUID speedCommandUUID = speedCommand.getUuid();
            uuidList.add(speedCommandUUID);
        }
        this.speedCommands = uuidList;
    }

    /**
     * Sets the lane change command list for this execution
     *
     * @param laneCommands the list of lane change commands to set
     */
    public void setLaneChangeCommands(List<LaneChangeCommand> laneCommands) {
        List<UUID> uuidList = new ArrayList<UUID>(laneCommands.size());
        for (LaneChangeCommand laneCommand : laneCommands) {
            UUID laneCommandUUID = laneCommand.getUuid();
            uuidList.add(laneCommandUUID);
        }
        this.laneCommands = uuidList;
    }

    /**
     * Adds a speed command to this execution's list of lanes
     *
     * @param speedCommand the speed command to add
     */
    public void addSpeedCommand(SpeedCommand speedCommand) {
        if (this.getSpeedCommands() == null) {
            this.setSpeedCommands(new ArrayList<SpeedCommand>(1));
        }
        this.getSpeedCommands().add(speedCommand);
        if (speedCommand.getExecution() == null || !speedCommand.getExecution().equals(this)) {
            speedCommand.setExecution(this);
        }
    }

    /**
     * Adds a lane change command to this execution's list if lane commands
     *
     * @param laneCommand the lance change command to add
     */
    public void addLaneChangeCommand(LaneChangeCommand laneCommand) {
        if (this.getLaneChangeCommands() == null) {
            this.setLaneChangeCommands(new ArrayList<LaneChangeCommand>(1));
        }
        this.getLaneChangeCommands().add(laneCommand);
        if (laneCommand.getExecution() == null || !laneCommand.getExecution().equals(this)) {
            laneCommand.setExecution(this);
        }
    }

    /**
     * Gets the execution's associated signal change commands
     *
     * @return execution's associated signal change commands
     */
    public List<SignalChangeCommand> getSignalChangeCommands() {
        List<SignalChangeCommand> signalCommandList = new ArrayList<SignalChangeCommand>(signalCommands.size());
        for (UUID id : this.signalCommands) {
            signalCommandList.add(ETexasEntityManager.getEntity(id, SignalChangeCommand.class));
        }
        return signalCommandList;
    }

    /**
     * Sets the signal change command list for this execution
     *
     * @param signalCommands the list of signal change commands to set
     */
    public void setSignalChangeCommands(List<SignalChangeCommand> signalCommands) {
        List<UUID> uuidList = new ArrayList<UUID>(signalCommands.size());
        for (SignalChangeCommand signalCommand : signalCommands) {
            UUID signalCommandUUID = signalCommand.getUuid();
            uuidList.add(signalCommandUUID);
        }
        this.signalCommands = uuidList;
    }

    /**
     * Adds a signal change command to this execution's list of lanes
     *
     * @param signalCommand the signal change command to add
     */
    public void addSignalChangeCommand(SignalChangeCommand signalCommand) {
        if (this.getSignalChangeCommands() == null) {
            this.setSignalChangeCommands(new ArrayList<SignalChangeCommand>(1));
        }
        this.getSignalChangeCommands().add(signalCommand);
        if (signalCommand.getExecution() == null || !signalCommand.getExecution().equals(this)) {
            signalCommand.setExecution(this);
        }
    }

    /**
     * Gets the execution's associated vehicle injection commands
     *
     * @return execution's associated vehicle injection commands
     */
    public List<VehicleInjectionCommand> getVehicleInjectionCommands() {
        List<VehicleInjectionCommand> vehicleCommandList = new ArrayList<VehicleInjectionCommand>(vehicleInjectionCommands.size());
        for (UUID id : this.vehicleInjectionCommands) {
            vehicleCommandList.add(ETexasEntityManager.getEntity(id, VehicleInjectionCommand.class));
        }
        return vehicleCommandList;

    }

    /**
     * Sets the vehicle injection command list for this execution
     *
     * @param vehicleCommands the list of vehicle injection commands to set
     */
    public void setVehicleInjectionCommands(List<VehicleInjectionCommand> vehicleCommands) {
        List<UUID> uuidList = new ArrayList<UUID>(vehicleCommands.size());
        for (VehicleInjectionCommand vehicleInjectionCommand : vehicleCommands) {
            UUID vehicleCommandUUID = vehicleInjectionCommand.getUuid();
            uuidList.add(vehicleCommandUUID);
        }
        this.vehicleInjectionCommands = uuidList;
    }

    /**
     * Adds a vehicle injection command to this execution's list of lanes
     *
     * @param vehicleInjectionCommand the vehicle injection comamnd to add
     */
    public void addVehicleInjectionCommand(VehicleInjectionCommand vehicleInjectionCommand) {
        if (this.getVehicleInjectionCommands() == null) {
            this.setVehicleInjectionCommands(new ArrayList<VehicleInjectionCommand>(1));
        }
        this.getVehicleInjectionCommands().add(vehicleInjectionCommand);
        if (vehicleInjectionCommand.getExecution() == null || !vehicleInjectionCommand.getExecution().equals(this)) {
            vehicleInjectionCommand.setExecution(this);
        }
    }

}
