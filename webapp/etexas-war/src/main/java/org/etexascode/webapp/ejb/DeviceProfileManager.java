/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.webapp.ejb;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.Response;

import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.application.Application;
import org.etexascode.webapp.datamodel.application.ApplicationParameter;
import org.etexascode.webapp.datamodel.device.CellularDeviceProfile;
import org.etexascode.webapp.datamodel.device.DeviceProfile;
import org.etexascode.webapp.datamodel.device.ObuDeviceProfile;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The EJB to manage device profile transactions.
 * 
 * @author emyers
 */
@Stateless
public class DeviceProfileManager {

    /** The application transaction manager. */
    @Inject
    private ApplicationProfileManager applicationProfileManager;

    /** The composite transaction manager. */
    @Inject
    private CompositeManager compositeManager;

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /**
     * Returns the device profiles for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of device profiles for the specified composite.
     * @throws WebAppException If the device profiles cannot be retrieved.
     */
    public List<DeviceProfile> getDeviceProfiles(Long userId, Long compositeId) throws WebAppException {

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Device Profiles Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.DEVICE_PROFILES_FROM_USER_ID_COMPOSITE_ID, DeviceProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Removes the specified device profile from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @throws WebAppException If the device profile cannot be removed.
     */
    public void removeDeviceProfile(Long userId, Long compositeId, Long deviceProfileId) throws WebAppException {

        try {

            removeDeviceProfiles(userId, compositeId, Arrays.asList(deviceProfileId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Device Profile Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified device profiles from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileIds The list of IDs of the device profiles.
     * @throws WebAppException If the device profiles cannot be removed.
     */
    public void removeDeviceProfiles(Long userId, Long compositeId, List<Long> deviceProfileIds) throws WebAppException {

        List<DeviceProfile> deviceProfiles;
        String exceptionTitle = "Remove Device Profiles Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Device profiles may not be removed from composites with existing executions.");
            }

            deviceProfiles = buildDeviceProfileList(getDeviceProfiles(userId, compositeId), deviceProfileIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (DeviceProfile deviceProfile : deviceProfiles) {

            entityManager.remove(deviceProfile);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of device profiles from the source list with the specified IDs.
     * 
     * @param deviceProfileList The list of source device profiles.
     * @param deviceProfileIds The list of device profile IDs to include.
     * @return A list of device profiles from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<DeviceProfile> buildDeviceProfileList(List<DeviceProfile> deviceProfileList, List<Long> deviceProfileIds) throws WebAppException {

        HashMap<Long, DeviceProfile> deviceProfileMap = new HashMap<>();

        for (DeviceProfile deviceProfile : deviceProfileList) {

            if (deviceProfileIds.contains(deviceProfile.getId())) {

                deviceProfileMap.put(deviceProfile.getId(), deviceProfile);
            }
        }

        ArrayList<DeviceProfile> deviceProfiles = new ArrayList<>(deviceProfileMap.values());

        for (Long id : deviceProfileIds) {

            DeviceProfile deviceProfile = deviceProfileMap.get(id);

            if (deviceProfile == null) {

                String message = String.format("No device profile with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (deviceProfileMap.containsKey(id)) {

                    message = "The same device profile ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Device Profile List Failure", message, status);
            }

            deviceProfileMap.put(id, null);
        }

        return deviceProfiles;
    }

    /**
     * Returns the specified device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return The device profile with the specified attributes.
     * @throws WebAppException If no such device profile exists.
     */
    public DeviceProfile getDeviceProfile(Long userId, Long compositeId, Long deviceProfileId) throws WebAppException {

        String exceptionTitle = "Get Device Profile Failure";

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<DeviceProfile> deviceProfiles = entityManager.createNamedQuery(Query.DEVICE_PROFILES_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID, DeviceProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_PROFILE_ID, deviceProfileId)
                .getResultList();

        if (deviceProfiles.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No device profile with ID \"%d\" could be found in the composite.", deviceProfileId), Response.Status.NOT_FOUND);
        }

        return deviceProfiles.get(0);
    }

    /**
     * Adds a new cellular device profile to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileName The string device profile name to set.
     * @param minDevices The integer minimum number of devices per affected vehicle to set.
     * @param maxDevices The integer maximum number of devices per affected vehicle to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @return The new cellular device profile that was added.
     * @throws WebAppException If a new cellular device profile cannot be added.
     */
    public CellularDeviceProfile addCellularDeviceProfile(Long userId, Long compositeId, String deviceProfileName, Integer minDevices, Integer maxDevices, Double percentage) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Cellular Device Profile Failure";

        if (minDevices > maxDevices) {

            throw new WebAppException(exceptionTitle, "The minimum number of devices per affected vehicle cannot exceed the maximum number of devices per affected vehicle.");
        }

        try {

            composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Cellular device profiles may not be added to composites with existing executions.");
            }

            validateDeviceProfileName(userId, compositeId, deviceProfileName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        double totalPercentage = percentage;
        for (DeviceProfile deviceProfile : composite.getDeviceProfiles()) {
            if (deviceProfile instanceof CellularDeviceProfile) {
                totalPercentage += deviceProfile.getPercentage();
            }
        }

        if (totalPercentage > 100) {

            throw new WebAppException(exceptionTitle, "The total percentage for all cellular device profiles may not exceed 100 percent.");
        }

        CellularDeviceProfile cellularDeviceProfile = new CellularDeviceProfile();
        cellularDeviceProfile.setName(deviceProfileName);
        cellularDeviceProfile.setMinDevices(minDevices);
        cellularDeviceProfile.setMaxDevices(maxDevices);
        cellularDeviceProfile.setPercentage(percentage);
        composite.getDeviceProfiles().add(cellularDeviceProfile);

        return cellularDeviceProfile;
    }

    /**
     * Returns the specified cellular device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return The cellular device profile with the specified attributes.
     * @throws WebAppException If no such cellular device profile exists.
     */
    public CellularDeviceProfile getCellularDeviceProfile(Long userId, Long compositeId, Long deviceProfileId) throws WebAppException {

        DeviceProfile deviceProfile;
        String exceptionTitle = "Get Cellular Device Profile Failure";

        try {

            deviceProfile = getDeviceProfile(userId, compositeId, deviceProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(deviceProfile instanceof CellularDeviceProfile)) {

            throw new WebAppException(exceptionTitle, String.format("The device profile with ID \"%d\" is not a cellular device profile.", deviceProfileId));
        }

        return (CellularDeviceProfile)deviceProfile;
    }

    /**
     * Updates the specified cellular device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param deviceProfileName The string device profile name to set.
     * @param minDevices The integer minimum number of devices per affected vehicle to set.
     * @param maxDevices The integer maximum number of devices per affected vehicle to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @throws WebAppException If the cellular device profile cannot be updated.
     */
    public void updateCellularDeviceProfile(Long userId, Long compositeId, Long deviceProfileId, String deviceProfileName, Integer minDevices, Integer maxDevices, Double percentage)
            throws WebAppException {

        Composite composite;
        CellularDeviceProfile cellularDeviceProfile;
        String exceptionTitle = "Update Cellular Device Profile Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            cellularDeviceProfile = getCellularDeviceProfile(userId, compositeId, deviceProfileId);
            deviceProfileName = (deviceProfileName != null) ? deviceProfileName : cellularDeviceProfile.getName();
            minDevices = (minDevices != null) ? minDevices : cellularDeviceProfile.getMinDevices();
            maxDevices = (maxDevices != null) ? maxDevices : cellularDeviceProfile.getMaxDevices();
            percentage = (percentage != null) ? percentage : cellularDeviceProfile.getPercentage();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Cellular device profiles may not be modified for composites with existing executions.");
            }

            if (minDevices > maxDevices) {

                throw new WebAppException(exceptionTitle, "The minimum number of devices per affected vehicle cannot exceed the maximum number of devices per affected vehicle.");
            }

            if (!cellularDeviceProfile.getName().equalsIgnoreCase(deviceProfileName)) {

                validateDeviceProfileName(userId, compositeId, deviceProfileName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        double totalPercentage = percentage;
        for (DeviceProfile deviceProfile : composite.getDeviceProfiles()) {
            if (deviceProfile instanceof CellularDeviceProfile && !deviceProfile.getId().equals(deviceProfileId)) {
                totalPercentage += deviceProfile.getPercentage();
            }
        }

        if (totalPercentage > 100) {

            throw new WebAppException(exceptionTitle, "The total percentage for all cellular device profiles may not exceed 100 percent.");
        }

        cellularDeviceProfile.setName(deviceProfileName);
        cellularDeviceProfile.setMinDevices(minDevices);
        cellularDeviceProfile.setMaxDevices(maxDevices);
        cellularDeviceProfile.setPercentage(percentage);
    }

    /**
     * Adds a new OBU device profile to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileName The string device profile name to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @return The new OBU device profile that was added.
     * @throws WebAppException If a new OBU device profile cannot be added.
     */
    public ObuDeviceProfile addObuDeviceProfile(Long userId, Long compositeId, String deviceProfileName, Double percentage) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add OBU Device Profile Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "OBU device profiles may not be added to composites with existing executions.");
            }

            validateDeviceProfileName(userId, compositeId, deviceProfileName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        double totalPercentage = percentage;
        for (DeviceProfile deviceProfile : composite.getDeviceProfiles()) {
            if (deviceProfile instanceof ObuDeviceProfile) {
                totalPercentage += deviceProfile.getPercentage();
            }
        }

        if (totalPercentage > 100) {

            throw new WebAppException(exceptionTitle, "The total percentage for all OBU device profiles may not exceed 100 percent.");
        }

        ObuDeviceProfile obuDeviceProfile = new ObuDeviceProfile();
        obuDeviceProfile.setName(deviceProfileName);
        obuDeviceProfile.setPercentage(percentage);
        composite.getDeviceProfiles().add(obuDeviceProfile);

        return obuDeviceProfile;
    }

    /**
     * Returns the specified OBU device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return The OBU device profile with the specified attributes.
     * @throws WebAppException If no such OBU device profile exists.
     */
    public ObuDeviceProfile getObuDeviceProfile(Long userId, Long compositeId, Long deviceProfileId) throws WebAppException {

        DeviceProfile deviceProfile;
        String exceptionTitle = "Get OBU Device Profile Failure";

        try {

            deviceProfile = getDeviceProfile(userId, compositeId, deviceProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(deviceProfile instanceof ObuDeviceProfile)) {

            throw new WebAppException(exceptionTitle, String.format("The device profile with ID \"%d\" is not an OBU device profile.", deviceProfileId));
        }

        return (ObuDeviceProfile)deviceProfile;
    }

    /**
     * Updates the specified OBU device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param deviceProfileName The string device profile name to set.
     * @param percentage The double percentage of affected vehicles to set.
     * @throws WebAppException If the OBU device profile cannot be updated.
     */
    public void updateObuDeviceProfile(Long userId, Long compositeId, Long deviceProfileId, String deviceProfileName, Double percentage) throws WebAppException {

        Composite composite;
        ObuDeviceProfile obuDeviceProfile;
        String exceptionTitle = "Update OBU Device Profile Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);
            obuDeviceProfile = getObuDeviceProfile(userId, compositeId, deviceProfileId);
            deviceProfileName = (deviceProfileName != null) ? deviceProfileName : obuDeviceProfile.getName();
            percentage = (percentage != null) ? percentage : obuDeviceProfile.getPercentage();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "OBU device profiles may not be modified for composites with existing executions.");
            }

            if (!obuDeviceProfile.getName().equalsIgnoreCase(deviceProfileName)) {

                validateDeviceProfileName(userId, compositeId, deviceProfileName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        double totalPercentage = percentage;
        for (DeviceProfile deviceProfile : composite.getDeviceProfiles()) {
            if (deviceProfile instanceof ObuDeviceProfile && !deviceProfile.getId().equals(deviceProfileId)) {
                totalPercentage += deviceProfile.getPercentage();
            }
        }

        if (totalPercentage > 100) {

            throw new WebAppException(exceptionTitle, "The total percentage for all OBU device profiles may not exceed 100 percent.");
        }

        obuDeviceProfile.setName(deviceProfileName);
        obuDeviceProfile.setPercentage(percentage);
    }

    /**
     * Validates the specified device profile name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileName The string device profile name to validate.
     * @throws WebAppException If the device profile name is not unique.
     */
    public void validateDeviceProfileName(Long userId, Long compositeId, String deviceProfileName) throws WebAppException {

        List<DeviceProfile> deviceProfiles = entityManager.createNamedQuery(Query.DEVICE_PROFILES_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_NAME, DeviceProfile.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_PROFILE_NAME, deviceProfileName)
                .getResultList();

        if (!deviceProfiles.isEmpty()) {

            throw new WebAppException("Validate Device Profile Name Failure", String.format("A device profile with the name \"%s\" already exists in the composite.", deviceProfileName));
        }
    }

    /**
     * Returns the applications for the specified device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @return A list of applications for the specified device profile.
     * @throws WebAppException If the applications cannot be retrieved.
     */
    public List<Application> getDeviceProfileApplications(Long userId, Long compositeId, Long deviceProfileId) throws WebAppException {

        try {

            getDeviceProfile(userId, compositeId, deviceProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Device Profile Applications Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_PROFILE_ID, deviceProfileId)
                .getResultList();
    }

    /**
     * Adds a new application to the specified device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationProfileId The long ID of the application profile.
     * @return The new application that was added.
     * @throws WebAppException If a new application cannot be added.
     */
    public Application addDeviceProfileApplication(Long userId, Long compositeId, Long deviceProfileId, Long applicationProfileId) throws WebAppException {

        Application application;
        DeviceProfile deviceProfile;
        String exceptionTitle = "Add Device Profile Application Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            deviceProfile = getDeviceProfile(userId, compositeId, deviceProfileId);
            application = applicationProfileManager.getApplicationProfile(userId, applicationProfileId).createApplication();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Applications may not be added to device profiles for composites with existing executions.");
            }

            validateApplicationName(userId, compositeId, deviceProfileId, application.getName());
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!deviceProfile.canHost(application)) {

            throw new WebAppException(exceptionTitle,
                    String.format("The \"%s\" application may not be hosted on %s device profiles.", application.getName(), deviceProfile.getType().getName()));
        }

        deviceProfile.getApplications().add(application);

        return application;
    }

    /**
     * Removes the specified application from the specified device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @throws WebAppException If the application cannot be removed.
     */
    public void removeDeviceProfileApplication(Long userId, Long compositeId, Long deviceProfileId, Long applicationId) throws WebAppException {

        try {

            removeDeviceProfileApplications(userId, compositeId, deviceProfileId, Arrays.asList(applicationId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Device Profile Application Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified applications from the specified device profile.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationIds The list of IDs of the applications.
     * @throws WebAppException If the applications cannot be removed.
     */
    public void removeDeviceProfileApplications(Long userId, Long compositeId, Long deviceProfileId, List<Long> applicationIds) throws WebAppException {

        List<Application> applications;
        String exceptionTitle = "Remove Device Profile Applications Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Applications may not be removed from device profiles for composites with existing executions.");
            }

            applications = buildDeviceProfileApplicationList(getDeviceProfileApplications(userId, compositeId, deviceProfileId), applicationIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        DeviceProfile deviceProfile = getDeviceProfile(userId, compositeId, deviceProfileId);

        for (Application application : applications) {

            deviceProfile.getApplications().remove(application);
            entityManager.remove(application);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of applications from the source list with the specified IDs.
     * 
     * @param applicationList The list of source applications.
     * @param applicationIds The list of application IDs to include.
     * @return A list of applications from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<Application> buildDeviceProfileApplicationList(List<Application> applicationList, List<Long> applicationIds) throws WebAppException {

        HashMap<Long, Application> applicationMap = new HashMap<>();

        for (Application application : applicationList) {

            if (applicationIds.contains(application.getId())) {

                applicationMap.put(application.getId(), application);
            }
        }

        ArrayList<Application> applications = new ArrayList<>(applicationMap.values());

        for (Long id : applicationIds) {

            Application application = applicationMap.get(id);

            if (application == null) {

                String message = String.format("No application with ID \"%d\" could be found on the device profile.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (applicationMap.containsKey(id)) {

                    message = "The same device profile application ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Device Application List Failure", message, status);
            }

            applicationMap.put(id, null);
        }

        return applications;
    }

    /**
     * Returns the specified application.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @return The application with the specified attributes.
     * @throws WebAppException If no such application exists.
     */
    public Application getDeviceProfileApplication(Long userId, Long compositeId, Long deviceProfileId, Long applicationId) throws WebAppException {

        String exceptionTitle = "Get Device Profile Application Failure";

        try {

            getDeviceProfile(userId, compositeId, deviceProfileId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Application> applications = entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID_APPLICATION_ID, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_PROFILE_ID, deviceProfileId)
                .setParameter(QueryParameter.APPLICATION_ID, applicationId)
                .getResultList();

        if (applications.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No application with ID \"%d\" could be found on the device profile.", applicationId), Response.Status.NOT_FOUND);
        }

        return applications.get(0);
    }

    /**
     * Validates the specified application name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationName The string application name to validate.
     * @throws WebAppException If the application name is not unique.
     */
    public void validateApplicationName(Long userId, Long compositeId, Long deviceProfileId, String applicationName) throws WebAppException {

        List<Application> applications = entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID_APPLICATION_NAME, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_PROFILE_ID, deviceProfileId)
                .setParameter(QueryParameter.APPLICATION_NAME, applicationName)
                .getResultList();

        if (!applications.isEmpty()) {

            throw new WebAppException("Validate Application Failure", String.format("An application with the name \"%s\" is already hosted on the device profile.", applicationName));
        }
    }

    /**
     * Returns the specified application parameter.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @return The application parameter with the specified attributes.
     * @throws WebAppException If no such application parameter exists.
     */
    public ApplicationParameter getDeviceProfileApplicationParameter(Long userId, Long compositeId, Long deviceProfileId, Long applicationId, Long applicationParameterId) throws WebAppException {

        String exceptionTitle = "Get Device Profile Application Parameter Failure";

        try {

            getDeviceProfileApplication(userId, compositeId, deviceProfileId, applicationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<ApplicationParameter> parameters = entityManager.createNamedQuery(
                Query.APPLICATION_PARAMETERS_FROM_USER_ID_COMPOSITE_ID_DEVICE_PROFILE_ID_APPLICATION_ID_APPLICATION_PARAMETER_ID, ApplicationParameter.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_PROFILE_ID, deviceProfileId)
                .setParameter(QueryParameter.APPLICATION_ID, applicationId)
                .setParameter(QueryParameter.APPLICATION_PARAMETER_ID, applicationParameterId)
                .getResultList();

        if (parameters.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No parameter with ID \"%d\" could be found for the application.", applicationParameterId), Response.Status.NOT_FOUND);
        }

        return parameters.get(0);
    }

    /**
     * Updates the specified application parameter.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceProfileId The long ID of the device profile.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @param value The string value to set.
     * @throws WebAppException If the application parameter cannot be updated.
     */
    public void updateDeviceProfileApplicationParameter(Long userId, Long compositeId, Long deviceProfileId, Long applicationId, Long applicationParameterId, String value) throws WebAppException {

        ApplicationParameter parameter;
        String exceptionTitle = "Update Device Profile Application Parameter Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            parameter = getDeviceProfileApplicationParameter(userId, compositeId, deviceProfileId, applicationId, applicationParameterId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Device profile application parameters may not be modified for composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        parameter.setValue(value);
    }
}
