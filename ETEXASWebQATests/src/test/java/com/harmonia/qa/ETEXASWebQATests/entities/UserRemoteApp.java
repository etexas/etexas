package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas user remote app
 *
 * @author llaroussini
 * @author rsmith
 */
public class UserRemoteApp extends EmbeddedApp {

    /**
     * Auto-generated serial ID
     */
    private static final long serialVersionUID = -2566092211442576447L;

    /**
     * Default Constructor.
     */
    public UserRemoteApp() {
        super();
        this.entityType = ETexasEntityType.USER_REMOTE_APP;
    }

    /**
     * The user associated with this remote app
     */
    private UUID user;

    /**
     * The Remote App ID
     */
    private String id;

    /**
     * Gets the UUID for the user with which this app is associated
     *
     * @return the UUID for the user with which this app is associated
     */
    public UUID getUserID() {
        return this.user;
    }

    /**
     * Gets the Remote App ID (auto-created upon creation of Remote App in UI)
     *
     * @return the Remote App ID
     */
    public String getID() {
        return this.id;
    }

    /**
     * Sets the Remote App ID
     *
     * @param id the id (as string) to set
     */
    public void setID(String id) {
        this.id = id;
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
