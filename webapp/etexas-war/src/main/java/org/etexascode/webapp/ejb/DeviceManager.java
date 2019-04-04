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
import org.etexascode.webapp.datamodel.device.Device;
import org.etexascode.webapp.datamodel.device.FixedCellularDevice;
import org.etexascode.webapp.datamodel.device.RseDevice;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The EJB to manage device transactions.
 * 
 * @author emyers
 */
@Stateless
public class DeviceManager {

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
     * Returns the devices for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of devices for the specified composite.
     * @throws WebAppException If the devices cannot be retrieved.
     */
    public List<Device> getDevices(Long userId, Long compositeId) throws WebAppException {

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Devices Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.DEVICES_FROM_USER_ID_COMPOSITE_ID, Device.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Removes the specified device from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @throws WebAppException If the device cannot be removed.
     */
    public void removeDevice(Long userId, Long compositeId, Long deviceId) throws WebAppException {

        try {

            removeDevices(userId, compositeId, Arrays.asList(deviceId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Device Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified devices from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceIds The list of IDs of the devices.
     * @throws WebAppException If the devices cannot be removed.
     */
    public void removeDevices(Long userId, Long compositeId, List<Long> deviceIds) throws WebAppException {

        List<Device> devices;
        String exceptionTitle = "Remove Devices Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Devices may not be removed from composites with existing executions.");
            }

            devices = buildDeviceList(getDevices(userId, compositeId), deviceIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (Device device : devices) {

            entityManager.remove(device);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of devices from the source list with the specified IDs.
     * 
     * @param deviceList The list of source devices.
     * @param deviceIds The list of device IDs to include.
     * @return A list of devices from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<Device> buildDeviceList(List<Device> deviceList, List<Long> deviceIds) throws WebAppException {

        HashMap<Long, Device> deviceMap = new HashMap<>();

        for (Device device : deviceList) {

            if (deviceIds.contains(device.getId())) {

                deviceMap.put(device.getId(), device);
            }
        }

        ArrayList<Device> devices = new ArrayList<>(deviceMap.values());

        for (Long id : deviceIds) {

            Device device = deviceMap.get(id);

            if (device == null) {

                String message = String.format("No device with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (deviceMap.containsKey(id)) {

                    message = "The same device ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Device List Failure", message, status);
            }

            deviceMap.put(id, null);
        }

        return devices;
    }

    /**
     * Returns the specified device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return The device with the specified attributes.
     * @throws WebAppException If no such device exists.
     */
    public Device getDevice(Long userId, Long compositeId, Long deviceId) throws WebAppException {

        String exceptionTitle = "Get Device Failure";

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Device> devices = entityManager.createNamedQuery(Query.DEVICES_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID, Device.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_ID, deviceId)
                .getResultList();

        if (devices.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No device with ID \"%d\" could be found in the composite.", deviceId), Response.Status.NOT_FOUND);
        }

        return devices.get(0);
    }

    /**
     * Adds a new fixed cellular device to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceName The string device name to set.
     * @param macAddress The long MAC address to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return The new fixed cellular device that was added.
     * @throws WebAppException If a new fixed cellular device cannot be added.
     */
    public FixedCellularDevice addFixedCellularDevice(Long userId, Long compositeId, String deviceName, Long macAddress, Double x, Double y, Double z) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Fixed Cellular Device Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "New fixed cellular devices may not be added to composites with existing executions.");
            }

            validateDeviceName(userId, compositeId, deviceName);
            validateMacAddress(userId, compositeId, macAddress);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        FixedCellularDevice fixedCellularDevice = new FixedCellularDevice();
        fixedCellularDevice.setName(deviceName);
        fixedCellularDevice.setMacAddress(macAddress);
        fixedCellularDevice.setX(x);
        fixedCellularDevice.setY(y);
        fixedCellularDevice.setZ(z);
        composite.getDevices().add(fixedCellularDevice);

        return fixedCellularDevice;
    }

    /**
     * Returns the specified fixed cellular device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return The fixed cellular device with the specified attributes.
     * @throws WebAppException If no such fixed cellular device exists.
     */
    public FixedCellularDevice getFixedCellularDevice(Long userId, Long compositeId, Long deviceId) throws WebAppException {

        Device device;
        String exceptionTitle = "Get Fixed Cellular Device Failure";

        try {

            device = getDevice(userId, compositeId, deviceId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(device instanceof FixedCellularDevice)) {

            throw new WebAppException(exceptionTitle, String.format("The device with ID \"%d\" is not a fixed cellular device.", deviceId));
        }

        return (FixedCellularDevice)device;
    }

    /**
     * Updates the specified fixed cellular device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param deviceName The string device name to set.
     * @param macAddress The long MAC address to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @throws WebAppException If the fixed cellular device cannot be updated.
     */
    public void updateFixedCellularDevice(Long userId, Long compositeId, Long deviceId, String deviceName, Long macAddress, Double x, Double y, Double z) throws WebAppException {

        FixedCellularDevice fixedCellularDevice;
        String exceptionTitle = "Update Fixed Cellular Device Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            fixedCellularDevice = getFixedCellularDevice(userId, compositeId, deviceId);
            deviceName = (deviceName != null) ? deviceName : fixedCellularDevice.getName();
            macAddress = (macAddress != null) ? macAddress : fixedCellularDevice.getMacAddress();
            x = (x != null) ? x : fixedCellularDevice.getX();
            y = (y != null) ? y : fixedCellularDevice.getY();
            z = (z != null) ? z : fixedCellularDevice.getZ();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Fixed cellular devices may not be modified for composites with existing executions.");
            }

            if (!fixedCellularDevice.getName().equalsIgnoreCase(deviceName)) {

                validateDeviceName(userId, compositeId, deviceName);
            }

            if (fixedCellularDevice.getMacAddress() != macAddress.longValue()) {

                validateMacAddress(userId, compositeId, macAddress);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        fixedCellularDevice.setName(deviceName);
        fixedCellularDevice.setMacAddress(macAddress);
        fixedCellularDevice.setX(x);
        fixedCellularDevice.setY(y);
        fixedCellularDevice.setZ(z);
    }

    /**
     * Adds a new RSE device to the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceName The string device name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return The new RSE device that was added.
     * @throws WebAppException If a new RSE device cannot be added.
     */
    public RseDevice addRseDevice(Long userId, Long compositeId, String deviceName, Double x, Double y, Double z) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add RSE Device Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "New RSE devices may not be added to composites with existing executions.");
            }

            validateDeviceName(userId, compositeId, deviceName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        RseDevice rseDevice = new RseDevice();
        rseDevice.setName(deviceName);
        rseDevice.setX(x);
        rseDevice.setY(y);
        rseDevice.setZ(z);
        composite.getDevices().add(rseDevice);

        return rseDevice;
    }

    /**
     * Returns the specified RSE device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return The RSE device with the specified attributes.
     * @throws WebAppException If no such RSE device exists.
     */
    public RseDevice getRseDevice(Long userId, Long compositeId, Long deviceId) throws WebAppException {

        Device device;
        String exceptionTitle = "Get RSE Device Failure";

        try {

            device = getDevice(userId, compositeId, deviceId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(device instanceof RseDevice)) {

            throw new WebAppException(exceptionTitle, String.format("The device with ID \"%d\" is not a RSE device.", deviceId));
        }

        return (RseDevice)device;
    }

    /**
     * Updates the specified RSE device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param deviceName The string device name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @throws WebAppException If the RSE device cannot be updated.
     */
    public void updateRseDevice(Long userId, Long compositeId, Long deviceId, String deviceName, Double x, Double y, Double z) throws WebAppException {

        RseDevice rseDevice;
        String exceptionTitle = "Update RSE Device Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            rseDevice = getRseDevice(userId, compositeId, deviceId);
            deviceName = (deviceName != null) ? deviceName : rseDevice.getName();
            x = (x != null) ? x : rseDevice.getX();
            y = (y != null) ? y : rseDevice.getY();
            z = (z != null) ? z : rseDevice.getZ();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "RSE devices may not be modified for composites with existing executions.");
            }

            if (!rseDevice.getName().equalsIgnoreCase(deviceName)) {

                validateDeviceName(userId, compositeId, deviceName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        rseDevice.setName(deviceName);
        rseDevice.setX(x);
        rseDevice.setY(y);
        rseDevice.setZ(z);
    }

    /**
     * Validates the specified device name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceName The string device name to validate.
     * @throws WebAppException If the device name is not unique.
     */
    public void validateDeviceName(Long userId, Long compositeId, String deviceName) throws WebAppException {

        List<Device> devices = entityManager.createNamedQuery(Query.DEVICES_FROM_USER_ID_COMPOSITE_ID_DEVICE_NAME, Device.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_NAME, deviceName)
                .getResultList();

        if (!devices.isEmpty()) {

            throw new WebAppException("Validate Device Name Failure", String.format("A device with the name \"%s\" already exists in the composite.", deviceName));
        }
    }

    /**
     * Validates the specified MAC address.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param macAddress The long MAC address to validate.
     * @throws WebAppException If the MAC address is not unique.
     */
    public void validateMacAddress(Long userId, Long compositeId, Long macAddress) throws WebAppException {

        List<Device> devices = entityManager.createNamedQuery(Query.DEVICES_FROM_USER_ID_COMPOSITE_ID_MAC_ADDRESS, Device.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.MAC_ADDRESS, macAddress)
                .getResultList();

        if (!devices.isEmpty()) {

            throw new WebAppException("Validate MAC Address Failure", String.format("A device with the MAC address \"%d\" already exists in the composite.", macAddress));
        }
    }

    /**
     * Returns the applications for the specified device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @return A list of applications for the specified device.
     * @throws WebAppException If the applications cannot be retrieved.
     */
    public List<Application> getDeviceApplications(Long userId, Long compositeId, Long deviceId) throws WebAppException {

        try {

            getDevice(userId, compositeId, deviceId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Device Applications Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_ID, deviceId)
                .getResultList();
    }

    /**
     * Adds a new application to the specified device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationProfileId The long ID of the application profile.
     * @return The new application that was added.
     * @throws WebAppException If a new application cannot be added.
     */
    public Application addDeviceApplication(Long userId, Long compositeId, Long deviceId, Long applicationProfileId) throws WebAppException {

        Application application;
        Device device;
        String exceptionTitle = "Add Device Application Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            device = getDevice(userId, compositeId, deviceId);
            application = applicationProfileManager.getApplicationProfile(userId, applicationProfileId).createApplication();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Applications may not be added to devices for composites with existing executions.");
            }

            validateApplicationName(userId, compositeId, deviceId, application.getName());
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!device.canHost(application)) {

            throw new WebAppException(exceptionTitle, String.format("The \"%s\" application may not be hosted on %s devices.", application.getName(), device.getType().getName()));
        }

        device.getApplications().add(application);

        return application;
    }

    /**
     * Removes the specified application from the specified device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @throws WebAppException If the application cannot be removed.
     */
    public void removeDeviceApplication(Long userId, Long compositeId, Long deviceId, Long applicationId) throws WebAppException {

        try {

            removeDeviceApplications(userId, compositeId, deviceId, Arrays.asList(applicationId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Device Application Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified applications from the specified device.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationIds The list of IDs of the applications.
     * @throws WebAppException If the applications cannot be removed.
     */
    public void removeDeviceApplications(Long userId, Long compositeId, Long deviceId, List<Long> applicationIds) throws WebAppException {

        List<Application> applications;
        String exceptionTitle = "Remove Device Applications Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Applications may not be removed from devices for composites with existing executions.");
            }

            applications = buildDeviceApplicationList(getDeviceApplications(userId, compositeId, deviceId), applicationIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (Application application : applications) {

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
    private List<Application> buildDeviceApplicationList(List<Application> applicationList, List<Long> applicationIds) throws WebAppException {

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

                String message = String.format("No application with ID \"%d\" could be found on the device.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (applicationMap.containsKey(id)) {

                    message = "The same device application ID may not be listed twice.";
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
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @return The application with the specified attributes.
     * @throws WebAppException If no such application exists.
     */
    public Application getDeviceApplication(Long userId, Long compositeId, Long deviceId, Long applicationId) throws WebAppException {

        String exceptionTitle = "Get Device Application Failure";

        try {

            getDevice(userId, compositeId, deviceId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Application> applications = entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID_APPLICATION_ID, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_ID, deviceId)
                .setParameter(QueryParameter.APPLICATION_ID, applicationId)
                .getResultList();

        if (applications.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No application with ID \"%d\" could be found on the device.", applicationId), Response.Status.NOT_FOUND);
        }

        return applications.get(0);
    }

    /**
     * Validates the specified application name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationName The string application name to validate.
     * @throws WebAppException If the application name is not unique.
     */
    public void validateApplicationName(Long userId, Long compositeId, Long deviceId, String applicationName) throws WebAppException {

        List<Application> applications = entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID_APPLICATION_NAME, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_ID, deviceId)
                .setParameter(QueryParameter.APPLICATION_NAME, applicationName)
                .getResultList();

        if (!applications.isEmpty()) {

            throw new WebAppException("Validate Application Failure", String.format("An application with the name \"%s\" is already hosted on the device.", applicationName));
        }
    }

    /**
     * Returns the specified application parameter.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @return The application parameter with the specified attributes.
     * @throws WebAppException If no such application parameter exists.
     */
    public ApplicationParameter getDeviceApplicationParameter(Long userId, Long compositeId, Long deviceId, Long applicationId, Long applicationParameterId) throws WebAppException {

        String exceptionTitle = "Get Device Application Parameter Failure";

        try {

            getDeviceApplication(userId, compositeId, deviceId, applicationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<ApplicationParameter> parameters = entityManager.createNamedQuery(
                Query.APPLICATION_PARAMETERS_FROM_USER_ID_COMPOSITE_ID_DEVICE_ID_APPLICATION_ID_APPLICATION_PARAMETER_ID, ApplicationParameter.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.DEVICE_ID, deviceId)
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
     * @param deviceId The long ID of the device.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @param value The string value to set.
     * @throws WebAppException If the application parameter cannot be updated.
     */
    public void updateDeviceApplicationParameter(Long userId, Long compositeId, Long deviceId, Long applicationId, Long applicationParameterId, String value) throws WebAppException {

        ApplicationParameter parameter;
        String exceptionTitle = "Update Device Application Parameter Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            parameter = getDeviceApplicationParameter(userId, compositeId, deviceId, applicationId, applicationParameterId);
            value = (value != null) ? value : parameter.getValue();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Device application parameters may not be modified for composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        parameter.setValue(value);
    }
}
