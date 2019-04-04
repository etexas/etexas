package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas user
 *
 * @author cbulloss
 */
public class ETexasUser extends ETexasBaseEntity {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -985614620388725964L;

    /**
     * The user's username for logging in to the application
     */
    private String username;

    /**
     * The user's password used for logging in to the application.
     */
    private String password;

    /**
     * The user's email address
     */
    private String emailAddress;

    /**
     * The user's last name
     */
    private String firstName;

    /**
     * The user's first name
     */
    private String lastName;

    /**
     * The user's organization
     */
    private String organization;

    /**
     * Token associated with a user after a successful login
     */
    private String token;

    /**
     * The list of simulations with the user has created (or will create)
     */
    private List<Simulation> simulations;//Note: this will need to be changed to UUID reference if we implement the entity manager or serialization

    /**
     * The list of JAR apps associated with this user
     */
    private List<UUID> jarApps = Collections.<UUID> emptyList();

    /**
     * The list of remote apps associated with this user
     */
    private List<UUID> remoteApps = Collections.<UUID> emptyList();

    /**
     * The list of native apps associated with this user
     */
    private List<UUID> nativeApps = Collections.<UUID> emptyList();

    /**
     * Default Constructor.
     */
    public ETexasUser() {
        super();
        this.entityType = ETexasEntityType.USER;
    }

    /**
     * Gets the user's username
     *
     * @return the user's username
     */
    public String getUsername() {
        if (this.username == null) {
            this.setUsername("");
        }
        return this.username;
    }

    /**
     * Sets the user's username
     *
     * @param username the username to set
     */
    public void setUsername(String username) {
        if (username == null) {
            this.username = "";
        }
        else {
            this.username = username;
        }
    }

    /**
     * Gets the user's password
     *
     * @return the user's password
     */
    public String getPassword() {
        if (this.password == null) {
            this.setPassword("");
        }
        return this.password;
    }

    /**
     * Sets the user's password
     *
     * @param password the value to set as password
     */
    public void setPassword(String password) {
        if (password == null) {
            this.password = "";
        }
        else {
            this.password = password;
        }
    }

    /**
     * Gets the use's email address
     *
     * @return the user's email address
     */
    public String getEmailAddress() {
        if (this.emailAddress == null) {
            this.setEmailAddress("");
        }
        return this.emailAddress;
    }

    /**
     * Set's the user's email address
     *
     * @param emailAddress the email address to set
     */
    public void setEmailAddress(String emailAddress) {
        if (emailAddress == null) {
            this.emailAddress = "";
        }
        else {
            this.emailAddress = emailAddress;
        }
    }

    /**
     * Gets the user's first name
     *
     * @return the user's first name
     */
    public String getFirstName() {
        if (this.firstName == null) {
            this.setFirstName("");
        }
        return this.firstName;
    }

    /**
     * Set's the user's first name
     *
     * @param firstName the first name to set
     */
    public void setFirstName(String firstName) {
        if (firstName == null) {
            this.firstName = "";
        }
        else {
            this.firstName = firstName;
        }
    }

    /**
     * Gets the user's last name
     *
     * @return the user's last name
     */
    public String getLastName() {
        if (this.lastName == null) {
            this.setLastName("");
        }
        return this.lastName;
    }

    /**
     * Sets the user's last name
     *
     * @param lastName the last name to set
     */
    public void setLastName(String lastName) {
        if (lastName == null) {
            this.lastName = "";
        }
        else {
            this.lastName = lastName;
        }
    }

    /**
     * Get's the user's organization set at registration
     *
     * @return the organization set for the user
     */
    public String getOrganization() {
        if (this.organization == null) {
            this.setOrganization("");
        }
        return this.organization;
    }

    /**
     * Sets the user's organization
     *
     * @param organization the name of the organization to set
     */
    public void setOrganization(String organization) {
        if (organization == null) {
            this.organization = "";
        }
        else {
            this.organization = organization;
        }
    }

    /**
     * Gets the list of simulations which this user has or will create
     *
     * @return the simulations which the user has or will create
     */
    public List<Simulation> getSimulations() {
        return this.simulations;
    }

    /**
     * Sets the user's list of simulations which they have or will create
     *
     * @param simulations the simulations to set which the user has or will
     *        create
     */
    public void setSimulations(List<Simulation> simulations) {
        this.simulations = simulations;
    }

    /**
     * Adds a simulation to this user's list
     *
     * @param simulation the simulation to add to this user's list
     */
    public void addSimulation(Simulation simulation) {
        this.getSimulations().add(simulation);
    }

    /**
     * Getter for user token
     *
     * @return String representation of the token
     */
    public String getToken() {
        return token;
    }

    /**
     * Setter for user token
     *
     * @param token
     */
    public void setToken(String token) {
        this.token = token;
    }

    /**
     * Gets the UUID list of JAR apps for this user
     *
     * @return the UUID JAR apps list
     */
    public List<UUID> getJARAppIds() {
        return this.jarApps;
    }

    /**
     * Gets the user's associated JAR apps
     *
     * @return user's associated JAR apps
     */
    public List<UserJarApp> getJarApps() {
        List<UserJarApp> jarList = new ArrayList<UserJarApp>(jarApps.size());
        for (UUID app : this.jarApps) {
            jarList.add(ETexasEntityManager.getEntity(app, UserJarApp.class));
        }
        return jarList;
    }

    /**
     * Sets the jar app list for this simulation
     *
     * @param jarApps the list of jar apps to set
     */
    public void setJARApps(List<UserJarApp> jarApps) {
        List<UUID> uuidList = new ArrayList<UUID>(jarApps.size());
        for (UserJarApp app : jarApps) {
            UUID jarUUID = app.getUuid();
            uuidList.add(jarUUID);
        }
        this.jarApps = uuidList;
    }

    /**
     * Adds a JAR app to this user's list of jar apps
     *
     * @param jarApp the JAR app to add
     */
    public void addJARApp(UserJarApp jarApp) {
        if (this.getJarApps() == null) {
            this.setJARApps(new ArrayList<UserJarApp>());
        }
        this.getJarApps().add(jarApp);
        if (this.equals(jarApp.getUser())) {
            jarApp.setUser(this);
        }
    }

    /**
     * Gets the UUID list of Remote apps for this user
     *
     * @return the UUID Remote apps list
     */
    public List<UUID> getRemoteAppIds() {
        return this.remoteApps;
    }

    /**
     * Gets the user's associated Remote apps
     *
     * @return user's associated Remote apps
     */
    public List<UserRemoteApp> getRemoteApps() {
        List<UserRemoteApp> remoteList = new ArrayList<UserRemoteApp>(remoteApps.size());
        for (UUID app : this.remoteApps) {
            remoteList.add(ETexasEntityManager.getEntity(app, UserRemoteApp.class));
        }
        return remoteList;
    }

    /**
     * Sets the remote app list for this simulation
     *
     * @param remoteApps the list of remote apps to set
     */
    public void setRemoteApps(List<UserRemoteApp> remoteApps) {
        List<UUID> uuidList = new ArrayList<UUID>(remoteApps.size());
        for (UserRemoteApp app : remoteApps) {
            UUID remoteUUID = app.getUuid();
            uuidList.add(remoteUUID);
        }
        this.remoteApps = uuidList;
    }

    /**
     * Adds a Remote app to this user's list of remote apps
     *
     * @param remoteApp the Remote app to add
     */
    public void addRemoteApp(UserRemoteApp remoteApp) {
        if (this.getRemoteApps() == null) {
            this.setRemoteApps(new ArrayList<UserRemoteApp>());
        }
        this.getRemoteApps().add(remoteApp);
        if (this.equals(remoteApp.getUser())) {
            remoteApp.setUser(this);
        }
    }

    /**
     * Gets the UUID list of native apps for this user
     *
     * @return the UUID native apps list
     */
    public List<UUID> getNativeAppIds() {
        return this.nativeApps;
    }

    /**
     * Gets the user's associated native apps
     *
     * @return user's associated native apps
     */
    public List<UserNativeApp> getNativeApps() {
        List<UserNativeApp> nativeList = new ArrayList<UserNativeApp>(nativeApps.size());
        for (UUID app : this.nativeApps) {
            nativeList.add(ETexasEntityManager.getEntity(app, UserNativeApp.class));
        }
        return nativeList;
    }

    /**
     * Sets the native app list for this simulation
     *
     * @param nativeApps the list of native apps to set
     */
    public void setNativeApps(List<UserNativeApp> nativeApps) {
        List<UUID> uuidList = new ArrayList<UUID>(nativeApps.size());
        for (UserNativeApp app : nativeApps) {
            UUID nativeUUID = app.getUuid();
            uuidList.add(nativeUUID);
        }
        this.nativeApps = uuidList;
    }

    /**
     * Adds a native app to this user's list of native apps
     *
     * @param nativeApp the native app to add
     */
    public void addNativeApp(UserNativeApp nativeApp) {
        if (this.getNativeApps() == null) {
            this.setNativeApps(new ArrayList<UserNativeApp>());
        }
        this.getNativeApps().add(nativeApp);
        if (this.equals(nativeApp.getUser())) {
            nativeApp.setUser(this);
        }
    }
}
