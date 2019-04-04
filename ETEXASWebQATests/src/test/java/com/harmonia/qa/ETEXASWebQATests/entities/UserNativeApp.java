package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas user native app
 *
 * @author llaroussini
 */
public class UserNativeApp extends EmbeddedApp {

    /**
     * Auto generated serial ID
     */
    private static final long serialVersionUID = -7452277205272825341L;

    /**
     * Default Constructor.
     */
    public UserNativeApp() {
        super();
        this.entityType = ETexasEntityType.USER_NATIVE_APP;
    }

    /**
     * The user associated with this native app
     */
    private UUID user;

    /**
     * The app host
     */
    private String host;

    /**
     * The app port
     */
    private String port;

    /**
     * The command line associated with the app
     */
    private String commandLine;

    /**
     * The Native App ID
     */
    private String id;

    /**
     * Gets the app's host
     *
     * @return the apps' host
     */
    public String getHost() {
        return this.host;
    }

    /**
     * Sets the app's host
     *
     * @param host -the app host to set
     */
    public void setHost(String host) {
        this.host = host;
    }

    /**
     * Gets the app's port
     *
     * @return the apps' port
     */
    public String getPort() {
        return this.port;
    }

    /**
     * Sets the app's port
     *
     * @param port -the app port to set
     */
    public void setPort(String port) {
        this.port = port;
    }

    /**
     * Getter for the app's command line
     *
     * @return the app's command line
     */
    public String getCommandLine() {
        return commandLine;
    }

    /**
     * Setter for the app's command line
     *
     * @param commandLine -the command line for launching the app
     */
    public void setCommandLine(String commandLine) {
        this.commandLine = commandLine;
    }

    /**
     * Gets the Native App ID (auto-created upon creation of Native App in UI)
     *
     * @return the Native App ID
     */
    public String getID() {
        return this.id;
    }

    /**
     * Sets the Native App ID
     *
     * @param id the id (as string) to set
     */
    public void setID(String id) {
        this.id = id;
    }

    /**
     * Gets the UUID for the user with which this app is associated
     *
     * @return the UUID for the user with which this app is associated
     */
    public UUID getUserID() {
        return this.user;
    }

    /**
     * Gets the user with which this app is associated
     *
     * @return the user with which this app is associated
     */
    public ETexasUser getUser() {
        return ETexasEntityManager.getEntity(getUserID(), ETexasUser.class);
    }

    /**
     * Sets the user for this app
     *
     * @param user the user to associate with the app
     */
    public void setUser(ETexasUser user) {
        UUID userUUID = user.getUuid();
        this.user = userUUID;
    }
}
