package com.harmonia.qa.ETEXASWebQATests.entities;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas user jar app
 *
 * @author llaroussini
 */
public class UserJarApp extends EmbeddedApp {

    /**
     * Auto-generated UUID
     */
    private static final long serialVersionUID = -2240132614378638057L;

    /**
     * Default Constructor.
     */
    public UserJarApp() {
        super();
        this.entityType = ETexasEntityType.USER_JAR_APP;
    }

    /**
     * The user associated with this jar app
     */
    private UUID user;

    /**
     * The app file
     */
    private List<File> files;

    /**
     * The JAR App ID
     */
    private String id;

    /**
     * Gets the app jar files
     *
     * @return the app jar files
     */
    public List<File> getFiles() {
        return this.files;
    }

    /**
     * Sets the app jar files
     *
     * @param files -the files to set
     */
    public void setFiles(List<File> files) {
        this.files = files;
    }

    /**
     * Adds a file to app jar files
     *
     * @param file -the file to add
     */
    public void addFile(File file) {
        if (this.getFiles() == null) {
            this.setFiles(new ArrayList<File>(1));
        }
        this.getFiles().add(file);
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

    /**
     * Gets the JAR App ID (auto-created upon creation of JAR App in UI)
     *
     * @return the JAR App ID
     */
    public String getID() {
        return this.id;
    }

    /**
     * Sets the JAR App ID
     *
     * @param id the id (as string) to set
     */
    public void setID(String id) {
        this.id = id;
    }
}
