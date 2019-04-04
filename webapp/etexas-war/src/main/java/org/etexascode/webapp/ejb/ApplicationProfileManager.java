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
package org.etexascode.webapp.ejb;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.datamodel.FileData;
import org.etexascode.webapp.datamodel.User;
import org.etexascode.webapp.datamodel.application.ApplicationProfile;
import org.etexascode.webapp.datamodel.application.JarApplicationProfile;
import org.etexascode.webapp.datamodel.application.NativeApplicationProfile;
import org.etexascode.webapp.datamodel.application.RemoteApplicationProfile;
import org.etexascode.webapp.datamodel.device.DeviceType;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The EJB to manage application database transactions.
 * 
 * @author bbadillo
 * @author janway
 * @author bmauldon
 * @author emyers
 */
@Stateless
public class ApplicationProfileManager {

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /** The user transaction manager. */
    @Inject
    private UserManager userManager;

    /**
     * Returns the application profiles for the specified user.
     * 
     * @param userId The long ID of the user.
     * @return A list of application profiles for the specified user.
     * @throws WebAppException If the application profiles cannot be retrieved.
     */
    public List<ApplicationProfile> getApplicationProfiles(Long userId) throws WebAppException {

        try {

            userManager.getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Application Profiles Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.APPLICATION_PROFILES_FROM_USER_ID, ApplicationProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .getResultList();
    }

    /**
     * Removes the specified application profile from the specified user.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @throws WebAppException If the application profile cannot be removed.
     */
    public void removeApplicationProfile(Long userId, Long applicationProfileId) throws WebAppException {

        try {

            removeApplicationProfiles(userId, Arrays.asList(applicationProfileId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Application Profile Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified application profiles from the specified user.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileIds The list of IDs of the application profiles.
     * @throws WebAppException If the application profiles cannot be removed.
     */
    public void removeApplicationProfiles(Long userId, List<Long> applicationProfileIds) throws WebAppException {

        List<ApplicationProfile> applicationProfiles;
        String exceptionTitle = "Remove Application Profiles Failure";

        try {

            applicationProfiles = buildApplicationProfileList(getApplicationProfiles(userId), applicationProfileIds);
            validateEmbeddedStatus(applicationProfiles);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ArrayList<String> removedJarFiles = new ArrayList<>();
        for (ApplicationProfile applicationProfile : applicationProfiles) {

            if (applicationProfile instanceof JarApplicationProfile) {

                String fileName = ((JarApplicationProfile)applicationProfile).getFileName();

                if (!removedJarFiles.contains(fileName)) {

                    try {

                        removeJarApplicationProfiles(userId, fileName);
                    }
                    catch (WebAppException exception) {

                        throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
                    }

                    removedJarFiles.add(fileName);
                }
            }
            else {

                entityManager.remove(applicationProfile);
            }
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of application profiles from the source list with the specified IDs.
     * 
     * @param applicationProfileList The list of source application profiles.
     * @param applicationProfileIds The list of application profile IDs to include.
     * @return A list of application profiles from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<ApplicationProfile> buildApplicationProfileList(List<ApplicationProfile> applicationProfileList, List<Long> applicationProfileIds) throws WebAppException {

        HashMap<Long, ApplicationProfile> applicationProfileMap = new HashMap<>();

        for (ApplicationProfile applicationProfile : applicationProfileList) {

            if (applicationProfileIds.contains(applicationProfile.getId())) {

                applicationProfileMap.put(applicationProfile.getId(), applicationProfile);
            }
        }

        ArrayList<ApplicationProfile> applicationProfiles = new ArrayList<>(applicationProfileMap.values());

        for (Long id : applicationProfileIds) {

            ApplicationProfile applicationProfile = applicationProfileMap.get(id);

            if (applicationProfile == null) {

                String message = String.format("No application profile with ID \"%d\" could be found.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (applicationProfileMap.containsKey(id)) {

                    message = "The same application profile ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Application Profile List Failure", message, status);
            }

            applicationProfileMap.put(id, null);
        }

        return applicationProfiles;
    }

    /**
     * Removes the JAR applications from the specified user.
     * 
     * @param userId The long ID of the user.
     * @param fileName The string name of the file.
     * @throws WebAppException If the JAR applications cannot be removed.
     */
    private void removeJarApplicationProfiles(Long userId, String fileName) throws WebAppException {

        entityManager.createNamedQuery(Query.REMOVE_FILE_DATA_FROM_APPLICATIONS)
                .setParameter(QueryParameter.FILE_NAME, fileName)
                .executeUpdate();

        try {

            for (JarApplicationProfile applicationProfile : getJarApplicationProfiles(userId, fileName)) {

                entityManager.remove(applicationProfile);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove JAR Application Profiles Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Returns the specified application profile.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @return The application profile with the specified attributes.
     * @throws WebAppException If no such application profile exists.
     */
    public ApplicationProfile getApplicationProfile(Long userId, Long applicationProfileId) throws WebAppException {

        String exceptionTitle = "Get Application Profile Failure";

        try {

            userManager.getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<ApplicationProfile> applicationProfiles = entityManager.createNamedQuery(Query.APPLICATION_PROFILES_FROM_USER_ID_APPLICATION_PROFILE_ID, ApplicationProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.APPLICATION_PROFILE_ID, applicationProfileId)
                .getResultList();

        if (applicationProfiles.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No application profile with ID \"%d\" could be found.", applicationProfileId), Response.Status.NOT_FOUND);
        }

        return applicationProfiles.get(0);
    }

    /**
     * Adds a JAR of application profiles to the specified user.
     * 
     * @param userId The long ID of the user.
     * @param applicationFileName The string file name to set.
     * @param applicationFileData The bytes of file data.
     * @param isEmbedded The embedded status to set.
     * @return The list of application profiles that were added.
     * @throws WebAppException If no application profiles can be added.
     */
    public List<JarApplicationProfile> addJarApplicationProfiles(Long userId, String applicationFileName, byte[] applicationFileData, boolean isEmbedded) throws WebAppException {

        User user;
        List<JarApplicationProfile> applicationProfiles;
        String exceptionTitle = "Add JAR Application Profiles Failure";

        try {

            user = userManager.getUser(userId);
            validateApplicationProfilesFileName(userId, applicationFileName);

            applicationProfiles = ApplicationProcessor.extractApplications(applicationFileData);

            if (applicationProfiles.isEmpty()) {

                throw new IOException();
            }

            for (JarApplicationProfile applicationProfile : applicationProfiles) {

                validateApplicationProfileName(userId, applicationProfile.getName());
            }
        }
        catch (IOException exception) {

            throw new WebAppException(exceptionTitle, "Application profile data could not be extracted from the uploaded file.");
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        FileData fileData = new FileData();
        fileData.setData(applicationFileData);

        for (JarApplicationProfile applicationProfile : applicationProfiles) {

            applicationProfile.setFileName(applicationFileName);
            applicationProfile.setFileData(fileData);
            applicationProfile.setEmbedded(isEmbedded);
            user.getApplicationProfiles().add(applicationProfile);
        }

        return applicationProfiles;
    }

    /**
     * Returns the specified JAR application profile.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @return The JAR application profile with the specified attributes.
     * @throws WebAppException If no such JAR application profile exists.
     */
    public JarApplicationProfile getJarApplicationProfile(Long userId, Long applicationProfileId) throws WebAppException {

        ApplicationProfile applicationProfile;
        String exceptionTitle = "Get JAR Application Profile Failure";

        try {

            applicationProfile = getApplicationProfile(userId, applicationProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(applicationProfile instanceof JarApplicationProfile)) {

            throw new WebAppException(exceptionTitle, String.format("The application profile with ID \"%d\" is not a JAR application profile.", applicationProfileId));
        }

        return (JarApplicationProfile)applicationProfile;
    }

    /**
     * Returns the JAR application profiles for the specified file.
     * 
     * @param userId The long ID of the user.
     * @param applicationFileName The string name of the file.
     * @return A list of JAR application profiles for the specified file.
     * @throws WebAppException If the JAR application profiles cannot be retrieved.
     */
    public List<JarApplicationProfile> getJarApplicationProfiles(Long userId, String applicationFileName) throws WebAppException {

        try {

            userManager.getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get JAR Application Profiles Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.APPLICATION_PROFILES_FROM_USER_ID_FILE_NAME, JarApplicationProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.FILE_NAME, applicationFileName)
                .getResultList();
    }

    /**
     * Updates the specified JAR application profile(s).
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @param applicationFileName The string file name to set.
     * @param applicationProfileName The string application profile name to set.
     * @throws WebAppException If the JAR application profile(s) cannot be updated.
     */
    public void updateJarApplicationProfiles(Long userId, Long applicationProfileId, String applicationFileName, String applicationProfileName) throws WebAppException {

        JarApplicationProfile jarApplicationProfile;
        String exceptionTitle = "Update JAR Application Profiles Failure";

        try {

            jarApplicationProfile = getJarApplicationProfile(userId, applicationProfileId);
            applicationFileName = (applicationFileName != null) ? applicationFileName : jarApplicationProfile.getFileName();
            applicationProfileName = (applicationProfileName != null) ? applicationProfileName : jarApplicationProfile.getName();
            validateEmbeddedStatus(Arrays.asList((ApplicationProfile)jarApplicationProfile));

            if (!jarApplicationProfile.getFileName().equalsIgnoreCase(applicationFileName)) {

                validateApplicationProfilesFileName(userId, applicationFileName);
            }

            if (!jarApplicationProfile.getName().equalsIgnoreCase(applicationProfileName)) {

                validateApplicationProfileName(userId, applicationProfileName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        jarApplicationProfile.setName(applicationProfileName);

        for (JarApplicationProfile applicationProfile : getJarApplicationProfiles(userId, jarApplicationProfile.getFileName())) {

            applicationProfile.setFileName(applicationFileName);
        }
    }

    /**
     * Validates the specified application profiles file name.
     * 
     * @param userId The long ID of the user.
     * @param fileName The string application profiles file name to validate.
     * @throws WebAppException If the application profiles file name is not unique.
     */
    public void validateApplicationProfilesFileName(Long userId, String fileName) throws WebAppException {

        List<ApplicationProfile> applicationProfiles = entityManager.createNamedQuery(Query.APPLICATION_PROFILES_FROM_USER_ID_FILE_NAME, ApplicationProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.FILE_NAME, fileName)
                .getResultList();

        if (!applicationProfiles.isEmpty()) {

            throw new WebAppException("Validate Application Profiles File Name Failure", String.format("A JAR application profiles file with the name \"%s\" already exists.", fileName));
        }
    }

    /**
     * Adds a new native application profile to the specified user.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileName The application profile name to set.
     * @param commandLine The string command line to set.
     * @param hostAddress The string host address to set.
     * @param portNumber The integer port number to set.
     * @param deviceType The device type to set.
     * @return The new native application profile that was added.
     * @throws WebAppException If a new native application profile cannot be added.
     */
    public NativeApplicationProfile addNativeApplicationProfile(Long userId, String applicationProfileName, String commandLine, String hostAddress, Integer portNumber, String deviceType)
            throws WebAppException {

        User user;

        try {

            user = userManager.getUser(userId);
            validateApplicationProfileName(userId, applicationProfileName);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Add Native Application Profile Failure", exception.getMessage(), exception.getStatus());
        }

        NativeApplicationProfile nativeApplicationProfile = new NativeApplicationProfile();
        nativeApplicationProfile.setName(applicationProfileName);
        nativeApplicationProfile.setCommandLine(commandLine);
        nativeApplicationProfile.setHostAddress(hostAddress);
        nativeApplicationProfile.setPortNumber(portNumber);
        nativeApplicationProfile.setDeviceType(DeviceType.valueOfName(deviceType));
        user.getApplicationProfiles().add(nativeApplicationProfile);

        return nativeApplicationProfile;
    }

    /**
     * Returns the specified native application profile.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @return The native application profile with the specified attributes.
     * @throws WebAppException If no such native application profile exists.
     */
    public NativeApplicationProfile getNativeApplicationProfile(Long userId, Long applicationProfileId) throws WebAppException {

        ApplicationProfile applicationProfile;
        String exceptionTitle = "Get Native Application Profile Failure";

        try {

            applicationProfile = getApplicationProfile(userId, applicationProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(applicationProfile instanceof NativeApplicationProfile)) {

            throw new WebAppException(exceptionTitle, String.format("The application profile with ID \"%d\" is not a native application profile.", applicationProfileId));
        }

        return (NativeApplicationProfile)applicationProfile;
    }

    /**
     * Updates the specified native application profile.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @param applicationProfileName The string application profile name to set.
     * @param commandLine The string command line to set.
     * @param hostAddress The string host address to set.
     * @param portNumber The integer port number to set.
     * @throws WebAppException If the native application profile cannot be updated.
     */
    public void updateNativeApplicationProfile(Long userId, Long applicationProfileId, String applicationProfileName, String commandLine, String hostAddress, Integer portNumber)
            throws WebAppException {

        NativeApplicationProfile nativeApplicationProfile;

        try {

            nativeApplicationProfile = getNativeApplicationProfile(userId, applicationProfileId);
            applicationProfileName = (applicationProfileName != null) ? applicationProfileName : nativeApplicationProfile.getName();
            commandLine = (commandLine != null) ? commandLine : nativeApplicationProfile.getCommandLine();
            hostAddress = (hostAddress != null) ? hostAddress : nativeApplicationProfile.getHostAddress();
            portNumber = (portNumber != null) ? portNumber : nativeApplicationProfile.getPortNumber();
            validateEmbeddedStatus(Arrays.asList((ApplicationProfile)nativeApplicationProfile));

            if (!nativeApplicationProfile.getName().equalsIgnoreCase(applicationProfileName)) {

                validateApplicationProfileName(userId, applicationProfileName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException("Update Native Application Profile Failure", exception.getMessage(), exception.getStatus());
        }

        nativeApplicationProfile.setName(applicationProfileName);
        nativeApplicationProfile.setCommandLine(commandLine);
        nativeApplicationProfile.setHostAddress(hostAddress);
        nativeApplicationProfile.setPortNumber(portNumber);
    }

    /**
     * Adds a new remote application profile to the specified user.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileName The string application profile name to set.
     * @param deviceType The string device type to set.
     * @return The new remote application profile that was added.
     * @throws WebAppException If a new remote application profile cannot be added.
     */
    public RemoteApplicationProfile addRemoteApplicationProfile(Long userId, String applicationProfileName, String deviceType) throws WebAppException {

        User user;

        try {

            user = userManager.getUser(userId);
            validateApplicationProfileName(userId, applicationProfileName);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Add Remote Application Profile Failure", exception.getMessage(), exception.getStatus());
        }

        RemoteApplicationProfile remoteApplicationProfile = new RemoteApplicationProfile();
        remoteApplicationProfile.setName(applicationProfileName);
        remoteApplicationProfile.setDeviceType(DeviceType.valueOfName(deviceType));
        user.getApplicationProfiles().add(remoteApplicationProfile);

        return remoteApplicationProfile;
    }

    /**
     * Returns the specified remote application profile.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @return The remote application profile with the specified attributes.
     * @throws WebAppException If no such remote application profile exists.
     */
    public RemoteApplicationProfile getRemoteApplicationProfile(Long userId, Long applicationProfileId) throws WebAppException {

        ApplicationProfile applicationProfile;
        String exceptionTitle = "Get Remote Application Profile Failure";

        try {

            applicationProfile = getApplicationProfile(userId, applicationProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(applicationProfile instanceof RemoteApplicationProfile)) {

            throw new WebAppException(exceptionTitle, String.format("The application profile with ID \"%d\" is not a remote application profile.", applicationProfileId));
        }

        return (RemoteApplicationProfile)applicationProfile;
    }

    /**
     * Updates the specified remote application profile.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileId The long ID of the application profile.
     * @param applicationProfileName The string application profile name to set.
     * @throws WebAppException If the remote application profile cannot be updated.
     */
    public void updateRemoteApplicationProfile(Long userId, Long applicationProfileId, String applicationProfileName) throws WebAppException {

        RemoteApplicationProfile remoteApplicationProfile;

        try {

            remoteApplicationProfile = getRemoteApplicationProfile(userId, applicationProfileId);
            applicationProfileName = (applicationProfileName != null) ? applicationProfileName : remoteApplicationProfile.getName();
            validateEmbeddedStatus(Arrays.asList((ApplicationProfile)remoteApplicationProfile));

            if (!remoteApplicationProfile.getName().equalsIgnoreCase(applicationProfileName)) {

                validateApplicationProfileName(userId, applicationProfileName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException("Update Remote Application Profile Failure", exception.getMessage(), exception.getStatus());
        }

        remoteApplicationProfile.setName(applicationProfileName);
    }

    /**
     * Validates the specified application profile name.
     * 
     * @param userId The long ID of the user.
     * @param applicationProfileName The string application profile name to validate.
     * @throws WebAppException If the application profile name is not unique.
     */
    public void validateApplicationProfileName(Long userId, String applicationProfileName) throws WebAppException {

        List<ApplicationProfile> applicationProfiles = entityManager.createNamedQuery(Query.APPLICATION_PROFILES_FROM_USER_ID_APPLICATION_PROFILE_NAME, ApplicationProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.APPLICATION_PROFILE_NAME, applicationProfileName)
                .getResultList();

        if (!applicationProfiles.isEmpty()) {

            throw new WebAppException("Validate Application Profile Name Failure", String.format("An application profile with the name \"%s\" already exists.", applicationProfileName));
        }
    }

    /**
     * Validates the embedded status of the specified application profiles.
     * 
     * @param applicationProfiles The list of application profiles to validate.
     * @throws WebAppException If any application profile is embedded.
     */
    public void validateEmbeddedStatus(List<ApplicationProfile> applicationProfiles) throws WebAppException {

        for (ApplicationProfile applicationProfile : applicationProfiles) {

            if (applicationProfile.isEmbedded()) {

                throw new WebAppException("Validate Embedded Status Failure",
                        String.format("The application profile with ID \"%d\" is embedded and cannot be modified or removed.", applicationProfile.getId()));
            }
        }
    }
}