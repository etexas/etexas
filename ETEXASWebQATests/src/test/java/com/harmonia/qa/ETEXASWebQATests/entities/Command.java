package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.CommandType;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas command
 *
 * @author llaroussini
 */
public class Command extends ETexasBaseEntity {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = -1391121253665862642L;

    /**
     * Default Constructor.
     */
    public Command() {
        super();
        this.entityType = ETexasEntityType.COMMAND;
    }

    /**
     * The command type
     */
    private CommandType type;

    /**
     * The sim time the command is injected
     */
    private String simTime;

    /**
     * The execution in which the command is included
     */
    private UUID execution;

    /**
     * Gets the command type
     *
     * @return the command type
     */
    public CommandType getCommandType() {
        return this.type;
    }

    /**
     * Sets the command type
     *
     * @param commandType -the command type to set
     */
    public void setCommandType(CommandType commandType) {
        this.type = commandType;
    }

    /**
     * Gets the sim time the command was injected
     *
     * @return the sim time
     */
    public String getSimTime() {
        return this.simTime;
    }

    /**
     * Sets the sim time
     *
     * @param simTime -the sim time the command was injected
     */
    public void setSimTime(String simTime) {
        this.simTime = simTime;
    }

    /**
     * Gets the UUID for the execution with which this command is associated
     *
     * @return the UUID for the execution with which this command is associated
     */
    public UUID getExecutionID() {
        return this.execution;
    }

    /**
     * Gets the execution with which this command is associated
     *
     * @return the execution with which this command is associated
     */
    public Execution getExecution() {
        return ETexasEntityManager.getEntity(getExecutionID(), Execution.class);
    }

    /**
     * Sets the execution for this command
     *
     * @param execution the execution to associate with the command
     */
    public void setExecution(Execution execution) {
        UUID execUUID = execution.getUuid();
        this.execution = execUUID;
    }

}
