package com.harmonia.qa.ETEXASWebQATests.entities;

import com.harmonia.qa.ETEXASWebQATests.enums.CommandType;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.enums.SignalChangeCommandType;

/**
 * Entity class representing an eTexas signal change command
 *
 * @author llaroussini
 */
public class SignalChangeCommand extends Command {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = -2102112051410413292L;

    /**
     * Default Constructor.
     */
    public SignalChangeCommand() {
        super();
        this.entityType = ETexasEntityType.SIGNAL_CHANGE_COMMAND;
        this.setCommandType(CommandType.SIGNAL_CHANGE);
    }

    /**
     * The time associated with the command
     */
    private String time;

    /**
     * The signal change command
     */
    private SignalChangeCommandType signalCommandType;

    /**
     * Gets the time
     *
     * @return the time associated with the command
     */
    public String getTime() {
        return this.time;
    }

    /**
     * Sets the time associated with the command
     *
     * @param time -the time to set
     */
    public void setTime(String time) {
        this.time = time;
    }

    /**
     * Gets the signal change command type
     *
     * @return the signal change command type
     */
    public SignalChangeCommandType getSignalChangeCommandType() {
        return this.signalCommandType;
    }

    /**
     * Sets the signal change command type
     *
     * @param signalCommandType -the signal change command type to set
     */
    public void setSignalChangeCommandType(SignalChangeCommandType signalCommandType) {
        this.signalCommandType = signalCommandType;
    }

}
