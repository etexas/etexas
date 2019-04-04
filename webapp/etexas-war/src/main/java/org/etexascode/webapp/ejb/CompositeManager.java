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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.Response;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.WaveSimType;
import org.etexascode.webapp.cdi.ExecutionMap;
import org.etexascode.webapp.datamodel.CellTower;
import org.etexascode.webapp.datamodel.CellularConfiguration;
import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.Execution;
import org.etexascode.webapp.datamodel.FileData;
import org.etexascode.webapp.datamodel.LaneManagerData;
import org.etexascode.webapp.datamodel.LaneMapping;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.datamodel.User;
import org.etexascode.webapp.datamodel.application.Application;
import org.etexascode.webapp.datamodel.application.ApplicationParameter;
import org.etexascode.webapp.datamodel.device.Device;
import org.etexascode.webapp.datamodel.device.ReportDevice;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The EJB to manage composite database transactions.
 * 
 * @author emyers
 * @author ttevendale
 */
@Stateless
public class CompositeManager {

    /** The application transaction manager. */
    @Inject
    private ApplicationProfileManager applicationProfileManager;

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /** The map of running executions. */
    @Inject
    private ExecutionMap executionMap;

    /** The user transaction manager. */
    @Inject
    private UserManager userManager;

    /**
     * Returns the composites for the specified user.
     * 
     * @param userId The long ID of the user.
     * @return A list of composites for the specified user.
     * @throws WebAppException If the composites cannot be retrieved.
     */
    public List<Composite> getComposites(Long userId) throws WebAppException {

        try {

            userManager.getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Composites Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.COMPOSITES_FROM_USER_ID, Composite.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .getResultList();
    }

    /**
     * Adds a new composite to the specified user.
     * 
     * @param userId The long ID of the user.
     * @param compositeName The string composite name to set.
     * @return The new composite that was added.
     * @throws WebAppException If a new compostie cannot be added.
     */
    public Composite addComposite(Long userId, String compositeName) throws WebAppException {

        User user;

        try {

            user = userManager.getUser(userId);
            validateCompositeName(userId, compositeName);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Add Composite Failure", exception.getMessage(), exception.getStatus());
        }

        Composite composite = new Composite();
        composite.setName(compositeName);
        user.getComposites().add(composite);

        return composite;
    }

    /**
     * Removes the specified composite from the specified user.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @throws WebAppException If the composite cannot be removed.
     */
    public void removeComposite(Long userId, Long compositeId) throws WebAppException {

        try {

            removeComposites(userId, Arrays.asList(compositeId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Composite Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified composites from the specified user.
     * 
     * @param userId The long ID of the user.
     * @param compositeIds The list of IDs of the composites.
     * @throws WebAppException If the composites cannot be removed.
     */
    public void removeComposites(Long userId, List<Long> compositeIds) throws WebAppException {

        List<Composite> composites;

        try {

            composites = buildCompositeList(getComposites(userId), compositeIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Composites Failure", exception.getMessage(), exception.getStatus());
        }

        List<Long> executions = new ArrayList<>();

        for (Composite composite : composites) {

            for (Execution execution : composite.getExecutions()) {

                executions.add(execution.getId());

                if (executionMap.get(execution.getId()) != null) {

                    executionMap.get(execution.getId()).deleteExecution();
                }
            }

            entityManager.remove(composite);
        }

        if (!executions.isEmpty()) {

            entityManager.createNamedQuery(Query.DELETE_APPLICATION_LOGS_FROM_EXECUTION_LIST)
                    .setParameter(QueryParameter.EXECUTION_LIST, executions)
                    .executeUpdate();

            entityManager.createNamedQuery(Query.DELETE_COMMANDS_FROM_EXECUTION_LIST)
                    .setParameter(QueryParameter.EXECUTION_LIST, executions)
                    .executeUpdate();
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of composites from the source list with the specified IDs.
     * 
     * @param compositeList The list of source composites.
     * @param compositeIds The list of composite IDs to include.
     * @return A list of composites from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<Composite> buildCompositeList(List<Composite> compositeList, List<Long> compositeIds) throws WebAppException {

        HashMap<Long, Composite> compositeMap = new HashMap<>();

        for (Composite composite : compositeList) {

            if (compositeIds.contains(composite.getId())) {

                compositeMap.put(composite.getId(), composite);
            }
        }

        ArrayList<Composite> composites = new ArrayList<>(compositeMap.values());

        for (Long id : compositeIds) {

            Composite composite = compositeMap.get(id);

            if (composite == null) {

                String message = String.format("No composite with ID \"%d\" could be found.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (compositeMap.containsKey(id)) {

                    message = "The same composite ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Composite List Failure", message, status);
            }

            compositeMap.put(id, null);
        }

        return composites;
    }

    /**
     * Returns the specified composite. The method should only be used when a user ID is not
     * available (i.e. the file service is the source of the request).
     * 
     * @param compositeId The long ID of the composite.
     * @return The composite with the specified attributes.
     * @throws WebAppException If no such composite exists.
     */
    public Composite getComposite(Long compositeId) throws WebAppException {

        Composite composite = entityManager.find(Composite.class, compositeId);

        if (composite == null) {

            throw new WebAppException("Get Composite Failure", String.format("No composite with ID \"%d\" could be found.", compositeId), Response.Status.NOT_FOUND);
        }

        return composite;
    }

    /**
     * Returns the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return The composite with the specified attributes.
     * @throws WebAppException If no such composite exists.
     */
    public Composite getComposite(Long userId, Long compositeId) throws WebAppException {

        String exceptionTitle = "Get Composite Failure";

        try {

            userManager.getUser(userId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Composite> composites = entityManager.createNamedQuery(Query.COMPOSITES_FROM_USER_ID_COMPOSITE_ID, Composite.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();

        if (composites.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No composite with ID \"%d\" could be found.", compositeId), Response.Status.NOT_FOUND);
        }

        return composites.get(0);
    }

    /**
     * Returns the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeName The string name of the composite.
     * @return The composite with the specified attributes or <code>null</code> if no such composite
     *         exists.
     */
    public Composite getComposite(Long userId, String compositeName) {

        List<Composite> composites = entityManager.createNamedQuery(Query.COMPOSITES_FROM_USER_ID_COMPOSITE_NAME, Composite.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_NAME, compositeName)
                .getResultList();

        return composites.isEmpty() ? null : composites.get(0);
    }

    /**
     * Adds a copy of the specified composite to the specified user.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param compositeName The string composite name to set.
     * @return The new composite copy that was added.
     * @throws WebAppException If a new composite copy cannot be added.
     */
    public Composite copyComposite(Long userId, Long compositeId, String compositeName) throws WebAppException {

        User user;
        Composite original;

        try {

            user = userManager.getUser(userId);
            original = getComposite(userId, compositeId);
            validateCompositeName(userId, compositeName);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Copy Composite Failure", exception.getMessage(), exception.getStatus());
        }

        Composite composite = original.copy();
        composite.setName(compositeName);
        user.getComposites().add(composite);
        entityManager.flush();

        Map<Long, Long> idMap = new HashMap<>();

        SimulationSearch: for (Simulation originalSimulation : original.getSimulations()) {
            for (Simulation simulation : composite.getSimulations()) {
                if (simulation.getName().equals(originalSimulation.getName())) {
                    idMap.put(originalSimulation.getId(), simulation.getId());
                    continue SimulationSearch;
                }
            }
        }

        List<LaneMapping> laneMappings = new ArrayList<>();
        for (LaneMapping laneMapping : original.getLaneMappings()) {
            LaneMapping copy = new LaneMapping();
            copy.setSourceSimulation(idMap.get(laneMapping.getSourceSimulation()));
            copy.setTargetSimulation(idMap.get(laneMapping.getTargetSimulation()));
            copy.setSourceLane(laneMapping.getSourceLane());
            copy.setTargetLane(laneMapping.getTargetLane());
            laneMappings.add(copy);
        }

        composite.setLaneMappings(laneMappings);

        return composite;
    }

    /**
     * Updates the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param compositeName The string composite name to set.
     * @throws WebAppException If the composite cannot be updated.
     */
    public void updateComposite(Long userId, Long compositeId, String compositeName) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Update Composite Failure";

        try {

            composite = getComposite(userId, compositeId);
            compositeName = (compositeName != null) ? compositeName : composite.getName();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Composites with existing executions may not be modified.");
            }

            if (!composite.getName().equalsIgnoreCase(compositeName)) {

                validateCompositeName(userId, compositeName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        composite.setName(compositeName);
    }

    /**
     * Validates the specified composite name.
     * 
     * @param userId The long ID of the user.
     * @param compositeName The string composite name to validate.
     * @throws WebAppException If the composite name is not unique.
     */
    public void validateCompositeName(Long userId, String compositeName) throws WebAppException {

        List<Composite> composites = entityManager.createNamedQuery(Query.COMPOSITES_FROM_USER_ID_COMPOSITE_NAME, Composite.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_NAME, compositeName)
                .getResultList();

        if (!composites.isEmpty()) {

            throw new WebAppException("Validate Composite Name Failure", String.format("A composite with the name \"%s\" already exists.", compositeName));
        }
    }

    /**
     * Returns the cell towers for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of cell towers for the specified composite.
     * @throws WebAppException If the cell towers cannot be retrieved.
     */
    public List<CellTower> getCellTowers(Long userId, Long compositeId) throws WebAppException {

        try {

            getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Cell Towers Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.CELL_TOWERS_FROM_USER_ID_COMPOSITE_ID, CellTower.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Adds a new cell tower to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param provider The string cellular provider to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @return The new cell tower that was added.
     * @throws WebAppException If a new cell tower cannot be added.
     */
    public CellTower addCellTower(Long userId, Long compositeId, String provider, Double x, Double y, Double z) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Cell Tower Failure";

        try {

            composite = getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "New cell towers may not be added to composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        CellTower cellTower = new CellTower();
        cellTower.setProvider(provider);
        cellTower.setX(x);
        cellTower.setY(y);
        cellTower.setZ(z);
        composite.getCellTowers().add(cellTower);

        return cellTower;
    }

    /**
     * Removes the specified cell tower from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param cellTowerId The long ID of the cell tower.
     * @throws WebAppException If the cell tower cannot be removed.
     */
    public void removeCellTower(Long userId, Long compositeId, Long cellTowerId) throws WebAppException {

        try {

            removeCellTowers(userId, compositeId, Arrays.asList(cellTowerId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Cell Tower Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified cell towers from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param cellTowerIds The list of IDs of the cell towers.
     * @throws WebAppException If the cell towers cannot be removed.
     */
    public void removeCellTowers(Long userId, Long compositeId, List<Long> cellTowerIds) throws WebAppException {

        List<CellTower> cellTowers;
        String exceptionTitle = "Remove Cell Towers Failure";

        try {

            Composite composite = getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Cell towers may not be removed from composites with existing executions.");
            }

            cellTowers = buildCellTowerList(getCellTowers(userId, compositeId), cellTowerIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (CellTower cellTower : cellTowers) {

            entityManager.remove(cellTower);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of cell towers from the source list with the specified IDs.
     * 
     * @param cellTowerList The list of source cell towers.
     * @param cellTowerIds The list of cell tower IDs to include.
     * @return A list of cell towers from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<CellTower> buildCellTowerList(List<CellTower> cellTowerList, List<Long> cellTowerIds) throws WebAppException {

        HashMap<Long, CellTower> cellTowerMap = new HashMap<>();

        for (CellTower cellTower : cellTowerList) {

            if (cellTowerIds.contains(cellTower.getId())) {

                cellTowerMap.put(cellTower.getId(), cellTower);
            }
        }

        ArrayList<CellTower> cellTowers = new ArrayList<>(cellTowerMap.values());

        for (Long id : cellTowerIds) {

            CellTower cellTower = cellTowerMap.get(id);

            if (cellTower == null) {

                String message = String.format("No cell tower with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (cellTowerMap.containsKey(id)) {

                    message = "The same cell tower ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Cell Tower List Failure", message, status);
            }

            cellTowerMap.put(id, null);
        }

        return cellTowers;
    }

    /**
     * Returns the specified cell tower.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param cellTowerId The long ID of the cell tower.
     * @return The cell tower with the specified attributes.
     * @throws WebAppException If no such cell tower exists.
     */
    public CellTower getCellTower(Long userId, Long compositeId, Long cellTowerId) throws WebAppException {

        String exceptionTitle = "Get Cell Tower Failure";

        try {

            getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<CellTower> cellTowers = entityManager.createNamedQuery(Query.CELL_TOWERS_FROM_USER_ID_COMPOSITE_ID_CELL_TOWER_ID, CellTower.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.CELL_TOWER_ID, cellTowerId)
                .getResultList();

        if (cellTowers.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No cell tower with ID \"%d\" could be found in the composite.", cellTowerId), Response.Status.NOT_FOUND);
        }

        return cellTowers.get(0);
    }

    /**
     * Updates the specified cell tower.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param cellTowerId The long ID of the cell tower.
     * @param provider The string cellular provider to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param z The double z coordinate (cm) to set.
     * @throws WebAppException If the cell tower cannot be updated.
     */
    public void updateCellTower(Long userId, Long compositeId, Long cellTowerId, String provider, Double x, Double y, Double z) throws WebAppException {

        CellTower cellTower;
        String exceptionTitle = "Update Cell Tower Failure";

        try {

            Composite composite = getComposite(userId, compositeId);
            cellTower = getCellTower(userId, compositeId, cellTowerId);
            provider = (provider != null) ? provider : cellTower.getProvider();
            x = (x != null) ? x : cellTower.getX();
            y = (y != null) ? y : cellTower.getY();
            z = (z != null) ? z : cellTower.getZ();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Cell towers may not be modified for composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        cellTower.setProvider(provider);
        cellTower.setX(x);
        cellTower.setY(y);
        cellTower.setZ(z);
    }

    /**
     * Returns the specified cellular configuration.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return The cellular configuration with the specified attributes.
     * @throws WebAppException If no such cellular configuration exists.
     */
    public CellularConfiguration getCellularConfiguration(Long userId, Long compositeId) throws WebAppException {

        String exceptionTitle = "Get Cellular Configuration Failure";

        try {

            getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<CellularConfiguration> cellularConfigurations = entityManager.createNamedQuery(Query.CELLULAR_CONFIGURATIONS_FROM_USER_ID_COMPOSITE_ID, CellularConfiguration.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();

        if (cellularConfigurations.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No cellular configuration for composite \"%d\" could be found.", compositeId), Response.Status.NOT_FOUND);
        }

        return cellularConfigurations.get(0);
    }

    /**
     * Updates the specified cellular configuration.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param uplinkBandwidth The uplink bandwidth (resource blocks) to set.
     * @param downlinkBandwidth The downlink bandwidth (resource blocks) to set.
     * @param uplinkCarrierFrequency The uplink carrier frequency to set.
     * @param downlinkCarrierFrequency The downlink carrier frequency to set.
     * @param cellTowerNoise The cell tower noise (dB) to set.
     * @param cellTowerPower The cell tower power (dBm) to set.
     * @param cellularDeviceNoise The cellular device noise (dB) to set.
     * @param cellularDevicePower The cellular device power (dBm) to set.
     * @throws WebAppException If the cellular configuration cannot be updated.
     */
    public void updateCellularConfiguration(Long userId, Long compositeId, Integer uplinkBandwidth, Integer downlinkBandwidth, Integer uplinkCarrierFrequency, Integer downlinkCarrierFrequency,
            Double cellTowerNoise, Double cellTowerPower, Double cellularDeviceNoise, Double cellularDevicePower) throws WebAppException {

        CellularConfiguration cellularConfiguration;
        String exceptionTitle = "Update Cellular Configuration Failure";

        try {

            Composite composite = getComposite(userId, compositeId);
            cellularConfiguration = getCellularConfiguration(userId, compositeId);
            uplinkBandwidth = (uplinkBandwidth != null) ? uplinkBandwidth : cellularConfiguration.getUplinkBandwidth();
            downlinkBandwidth = (downlinkBandwidth != null) ? downlinkBandwidth : cellularConfiguration.getDownlinkBandwidth();
            uplinkCarrierFrequency = (uplinkCarrierFrequency != null) ? uplinkCarrierFrequency : cellularConfiguration.getUplinkCarrierFrequency();
            downlinkCarrierFrequency = (downlinkCarrierFrequency != null) ? downlinkCarrierFrequency : cellularConfiguration.getDownlinkCarrierFrequency();
            cellTowerNoise = (cellTowerNoise) != null ? cellTowerNoise : cellularConfiguration.getCellTowerNoise();
            cellTowerPower = (cellTowerPower) != null ? cellTowerPower : cellularConfiguration.getCellTowerPower();
            cellularDeviceNoise = (cellularDeviceNoise != null) ? cellularDeviceNoise : cellularConfiguration.getCellularDeviceNoise();
            cellularDevicePower = (cellularDevicePower != null) ? cellularDevicePower : cellularConfiguration.getCellularDevicePower();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Cellular configurations may not be modified for composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        cellularConfiguration.setUplinkBandwidth(uplinkBandwidth);
        cellularConfiguration.setDownlinkBandwidth(downlinkBandwidth);
        cellularConfiguration.setUplinkCarrierFrequency(uplinkCarrierFrequency);
        cellularConfiguration.setDownlinkCarrierFrequency(downlinkCarrierFrequency);
        cellularConfiguration.setCellTowerNoise(cellTowerNoise);
        cellularConfiguration.setCellTowerPower(cellTowerPower);
        cellularConfiguration.setCellularDeviceNoise(cellularDeviceNoise);
        cellularConfiguration.setCellularDevicePower(cellularDevicePower);
    }

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

            getComposite(userId, compositeId);
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
     * Returns the lane mappings for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of lane mappings for the specified composite.
     * @throws WebAppException If the lane mappings cannot be retrieved.
     */
    public List<LaneMapping> getLaneMappings(Long userId, Long compositeId) throws WebAppException {

        try {

            getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Lane Mappings Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.LANE_MAPPINGS_FROM_USER_ID_COMPOSITE_ID, LaneMapping.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Adds a new lane mapping to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param sourceSimulation The long source simulation ID to set.
     * @param sourceLane The integer source lane ID to set.
     * @param targetSimulation The long target simulation ID to set.
     * @param targetLane The integer target lane ID to set.
     * @return The new lane mapping that was added.
     * @throws WebAppException If a new lane mapping cannot be added.
     */
    public LaneMapping addLaneMapping(Long userId, Long compositeId, Long sourceSimulation, Integer sourceLane, Long targetSimulation, Integer targetLane) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Lane Mapping Failure";

        try {

            composite = getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "New lane mappings may not be added to composites with existing executions.");
            }

            validateLaneMapping(getLanes(userId, compositeId), composite.getLaneMappings(), sourceSimulation, sourceLane, targetSimulation, targetLane);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        LaneMapping laneMapping = new LaneMapping();
        laneMapping.setSourceSimulation(sourceSimulation);
        laneMapping.setSourceLane(sourceLane);
        laneMapping.setTargetSimulation(targetSimulation);
        laneMapping.setTargetLane(targetLane);
        composite.getLaneMappings().add(laneMapping);

        return laneMapping;
    }

    /**
     * Removes the specified lane mapping from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param laneMappingId The long ID of the lane mapping.
     * @throws WebAppException If the lane mapping cannot be removed.
     */
    public void removeLaneMapping(Long userId, Long compositeId, Long laneMappingId) throws WebAppException {

        try {

            removeLaneMappings(userId, compositeId, Arrays.asList(laneMappingId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Lane Mapping Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified lane mappings from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param laneMappingIds The list of IDs of the lane mappings.
     * @throws WebAppException If the lane mappings cannot be removed.
     */
    public void removeLaneMappings(Long userId, Long compositeId, List<Long> laneMappingIds) throws WebAppException {

        List<LaneMapping> laneMappings;
        String exceptionTitle = "Remove Lane Mappings Failure";

        try {

            Composite composite = getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Lane mappings may not be removed from composites with existing executions.");
            }

            laneMappings = buildLaneMappingList(getLaneMappings(userId, compositeId), laneMappingIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (LaneMapping laneMapping : laneMappings) {

            entityManager.remove(laneMapping);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of lane mappings from the source list with the specified IDs.
     * 
     * @param laneMappingList The list of source lane mappings.
     * @param laneMappingIds The list of lane mapping IDs to include.
     * @return A list of lane mappings from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<LaneMapping> buildLaneMappingList(List<LaneMapping> laneMappingList, List<Long> laneMappingIds) throws WebAppException {

        HashMap<Long, LaneMapping> laneMappingMap = new HashMap<>();

        for (LaneMapping laneMapping : laneMappingList) {

            if (laneMappingIds.contains(laneMapping.getId())) {

                laneMappingMap.put(laneMapping.getId(), laneMapping);
            }
        }

        ArrayList<LaneMapping> laneMappings = new ArrayList<>(laneMappingMap.values());

        for (Long id : laneMappingIds) {

            LaneMapping laneMapping = laneMappingMap.get(id);

            if (laneMapping == null) {

                String message = String.format("No lane mapping with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (laneMappingMap.containsKey(id)) {

                    message = "The same lane mapping ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Lane Mapping List Failure", message, status);
            }

            laneMappingMap.put(id, null);
        }

        return laneMappings;
    }

    /**
     * Returns the specified lane mapping.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param laneMappingId The long ID of the lane mapping.
     * @return The lane mapping with the specified attributes.
     * @throws WebAppException If no such lane mapping exists.
     */
    public LaneMapping getLaneMapping(Long userId, Long compositeId, Long laneMappingId) throws WebAppException {

        String exceptionTitle = "Get Lane Mapping Failure";

        try {

            getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<LaneMapping> laneMappings = entityManager.createNamedQuery(Query.LANE_MAPPINGS_FROM_USER_ID_COMPOSITE_ID_LANE_MAPPING_ID, LaneMapping.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.LANE_MAPPING_ID, laneMappingId)
                .getResultList();

        if (laneMappings.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No lane mapping with ID \"%d\" could be found in the composite.", laneMappingId), Response.Status.NOT_FOUND);
        }

        return laneMappings.get(0);
    }

    /**
     * Updates the specified lane mapping.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param laneMappingId The long ID of the lane mapping.
     * @param sourceSimulation The long source simulation ID to set.
     * @param sourceLane The integer source lane ID to set.
     * @param targetSimulation The long target simulation ID to set.
     * @param targetLane The integer target lane ID to set.
     * @throws WebAppException If the lane mapping cannot be updated.
     */
    public void updateLaneMapping(Long userId, Long compositeId, Long laneMappingId, Long sourceSimulation, Integer sourceLane, Long targetSimulation, Integer targetLane) throws WebAppException {

        LaneMapping laneMapping;
        String exceptionTitle = "Update Lane Mapping Failure";

        try {

            Composite composite = getComposite(userId, compositeId);
            laneMapping = getLaneMapping(userId, compositeId, laneMappingId);
            sourceSimulation = (sourceSimulation != null) ? sourceSimulation : laneMapping.getSourceSimulation();
            sourceLane = (sourceLane != null) ? sourceLane : laneMapping.getSourceLane();
            targetSimulation = (targetSimulation != null) ? targetSimulation : laneMapping.getTargetSimulation();
            targetLane = (targetLane != null) ? targetLane : laneMapping.getTargetLane();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Lane mappings may not be modified for composites with existing executions.");
            }

            Collection<LaneMapping> validationSet = new HashSet<>(composite.getLaneMappings());
            validationSet.remove(laneMapping);
            validateLaneMapping(getLanes(userId, compositeId), validationSet, sourceSimulation, sourceLane, targetSimulation, targetLane);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        laneMapping.setSourceSimulation(sourceSimulation);
        laneMapping.setSourceLane(sourceLane);
        laneMapping.setTargetSimulation(targetSimulation);
        laneMapping.setTargetLane(targetLane);
    }

    /**
     * Validates the specified lane mapping.
     * 
     * @param lanes The existing lanes.
     * @param laneMappings The existing lane mappings.
     * @param sourceSimulation The long ID of the source simulation.
     * @param sourceLane The integer ID of the source lane.
     * @param targetSimulation The long ID of the target simulation.
     * @param targetLane The integer ID of the target lane.
     * @throws WebAppException If a mapping from the specified lane and simulation or a mapping to
     *         the specified lane and simulation already exists.
     */
    public void validateLaneMapping(Map<Long, List<ILane>> lanes, Collection<LaneMapping> laneMappings, Long sourceSimulation, Integer sourceLane, Long targetSimulation, Integer targetLane)
            throws WebAppException {

        String exceptionTitle = "Validate Lane Mapping Failure";

        try {

            validateMappingSource(lanes, sourceSimulation, sourceLane);
            validateMappingTarget(lanes, targetSimulation, targetLane);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (sourceSimulation.equals(targetSimulation)) {

            throw new WebAppException(exceptionTitle, "The source and target simulations for each lane mapping must be unique.");
        }

        for (LaneMapping laneMapping : laneMappings) {

            if (laneMapping.getSourceSimulation() == sourceSimulation.longValue() && laneMapping.getSourceLane() == sourceLane.intValue()) {

                throw new WebAppException(exceptionTitle, String.format("A lane mapping from lane \"%d\" in simulation \"%d\" already exists in the composite.", sourceLane, sourceSimulation));
            }
            else if (laneMapping.getTargetSimulation() == targetSimulation.longValue() && laneMapping.getTargetLane() == targetLane.intValue()) {

                throw new WebAppException(exceptionTitle, String.format("A lane mapping to lane \"%d\" in simulation \"%d\" already exists in the composite.", targetLane, targetSimulation));
            }
        }
    }

    /**
     * Validates the specified lane mapping source.
     * 
     * @param lanes The existing lanes.
     * @param sourceSimulation The long ID of the source simulation.
     * @param sourceLane The integer ID of the source lane.
     * @throws WebAppException If the lane mapping source is not valid.
     */
    private void validateMappingSource(Map<Long, List<ILane>> lanes, Long sourceSimulation, Integer sourceLane) throws WebAppException {

        String exceptionTitle = "Validate Mapping Source Failure";

        if (!lanes.containsKey(sourceSimulation)) {

            throw new WebAppException(exceptionTitle, String.format("No simulation with ID \"%d\" could be found in the composite.", sourceSimulation), Response.Status.NOT_FOUND);
        }

        ILane source = null;
        for (ILane lane : lanes.get(sourceSimulation)) {
            if (lane.getLaneId() == sourceLane.intValue()) {
                source = lane;
                break;
            }
        }

        if (source == null) {

            throw new WebAppException(exceptionTitle, String.format("Lane number \"%d\" could not be found in simulation \"%d\".", sourceLane, sourceSimulation), Response.Status.NOT_FOUND);
        }

        if (!source.getType().equals(Lane.OUTBOUND)) {

            throw new WebAppException(exceptionTitle, "The source lane in a lane mapping must be an outbound lane.");
        }
    }

    /**
     * Validates the specified lane mapping target.
     * 
     * @param lanes The existing lanes.
     * @param targetSimulation The long ID of the target simulation.
     * @param targetLane The integer ID of the target lane.
     * @throws WebAppException If the lane mapping target is not valid.
     */
    private void validateMappingTarget(Map<Long, List<ILane>> lanes, Long targetSimulation, Integer targetLane) throws WebAppException {

        String exceptionTitle = "Validate Mapping Target Failure";

        if (!lanes.containsKey(targetSimulation)) {

            throw new WebAppException(exceptionTitle, String.format("No simulation with ID \"%d\" could be found in the composite.", targetSimulation), Response.Status.NOT_FOUND);
        }

        ILane target = null;
        for (ILane lane : lanes.get(targetSimulation)) {
            if (lane.getLaneId() == targetLane.intValue()) {
                target = lane;
                break;
            }
        }

        if (target == null) {

            throw new WebAppException(exceptionTitle, String.format("Lane number \"%d\" could not be found in simulation \"%d\".", targetLane, targetSimulation), Response.Status.NOT_FOUND);
        }

        if (!target.getType().equals(Lane.INBOUND)) {

            throw new WebAppException(exceptionTitle, "The target lane in a lane mapping must be an inbound lane.");
        }
    }

    /**
     * Returns the lanes for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A mapping of lanes by simulation ID for the specified composite.
     * @throws WebAppException If the lanes cannot be retrieved.
     */
    public Map<Long, List<ILane>> getLanes(Long userId, Long compositeId) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Get Lanes Failure";

        try {

            composite = getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        Map<Long, List<ILane>> laneMap = new HashMap<>();

        for (Simulation simulation : composite.getSimulations()) {

            List<LaneManagerData> laneManagerData = entityManager.createNamedQuery(Query.LANE_MANAGER_DATA_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID, LaneManagerData.class)
                    .setParameter(QueryParameter.USER_ID, userId)
                    .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                    .setParameter(QueryParameter.SIMULATION_ID, simulation.getId())
                    .getResultList();

            if (laneManagerData.isEmpty()) {

                throw new WebAppException(exceptionTitle, String.format("No lane manager data for simulation \"%d\" could be found.", simulation.getId()), Response.Status.NOT_FOUND);
            }

            List<ILane> lanes = new ArrayList<>();

            for (ILane lane : laneManagerData.get(0).getLaneManager()) {

                lanes.add(lane);
            }

            laneMap.put(simulation.getId(), lanes);
        }

        return laneMap;
    }

    /**
     * Returns the file data for the specified composite. The method should only be used when a user
     * ID is not available (i.e. the file service is the source of the request).
     * 
     * @param compositeId The long ID of the composite.
     * @return The bytes of file data for the specified composite.
     * @throws WebAppException If the file data cannot be retrieved.
     */
    public byte[] getFileData(Long compositeId) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Get File Data Failure";

        try {

            composite = getComposite(compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        ByteArrayOutputStream data = new ByteArrayOutputStream();

        try {

            ZipOutputStream outputStream = new ZipOutputStream(data);

            for (Simulation simulation : composite.getSimulations()) {

                List<FileData> fileData = entityManager.createNamedQuery(Query.FILE_DATA_FROM_SIMULATION_ID, FileData.class)
                        .setParameter(QueryParameter.SIMULATION_ID, simulation.getId())
                        .getResultList();

                if (fileData.isEmpty()) {

                    throw new WebAppException(exceptionTitle, String.format("No file data for composite \"%d\" could be found.", compositeId), Response.Status.NOT_FOUND);
                }

                ZipEntry entry = new ZipEntry(String.format("%s.zip", simulation.getName()));
                entry.setSize(fileData.get(0).getData().length);
                outputStream.putNextEntry(entry);
                outputStream.write(fileData.get(0).getData());
                outputStream.closeEntry();
            }

            outputStream.close();
        }
        catch (IOException exception) {

            throw new WebAppException(exceptionTitle, String.format("The file data for composite \"%d\" could not be read.", compositeId), Response.Status.INTERNAL_SERVER_ERROR);
        }

        return data.toByteArray();
    }

    /**
     * Returns the option properties for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A map of the option properties for the specified composite.
     * @throws WebAppException If the option properties cannot be retrieved.
     */
    public Map<String, Object> getCompositeOptions(Long userId, Long compositeId) throws WebAppException {

        Composite composite;

        try {

            composite = getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Composite Options Failure", exception.getMessage(), exception.getStatus());
        }

        Map<String, Object> optionsMap = new HashMap<>();
        optionsMap.put("latitude", composite.getLatitude());
        optionsMap.put("longitude", composite.getLongitude());
        optionsMap.put("geographicCalculator", UtilsLatLongConversion.convertCalculatorType(composite.getGeographicCalculator()));
        optionsMap.put("propagationLossModel", composite.getPropagationLossModel().getName());
        optionsMap.put("communicationsModel", composite.getCommunicationsModel().getName());

        return optionsMap;
    }

    /**
     * Updates the option properties for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param latitude The double latitude (DD) to set.
     * @param longitude The double longitude (DD) to set.
     * @param geographicCalculator The string geographic calculator to set.
     * @param propagationLossModel The string propagation loss model to set.
     * @param communicationsModel The string communications model to set.
     * @throws WebAppException If the option properties cannot be updated.
     */
    public void updateCompositeOptions(Long userId, Long compositeId, Double latitude, Double longitude, String geographicCalculator, String propagationLossModel,
            String communicationsModel)
            throws WebAppException {

        Composite composite;
        String exceptionTitle = "Update Composite Options Failure";

        try {

            composite = getComposite(userId, compositeId);
            latitude = (latitude != null) ? latitude : composite.getLatitude();
            longitude = (longitude != null) ? longitude : composite.getLongitude();
            geographicCalculator = (geographicCalculator != null) ? geographicCalculator : UtilsLatLongConversion.convertCalculatorType(composite.getGeographicCalculator());
            propagationLossModel = (propagationLossModel != null) ? propagationLossModel : composite.getPropagationLossModel().getName();
            communicationsModel = (communicationsModel != null) ? communicationsModel : composite.getCommunicationsModel().getName();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Composite options may not be modified for composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        composite.setLatitude(latitude);
        composite.setLongitude(longitude);
        composite.setGeographicCalculator(UtilsLatLongConversion.convertCalculatorType(geographicCalculator));
        composite.setPropagationLossModel(PropagationLossModel.valueOfName(propagationLossModel));
        composite.setCommunicationsModel(WaveSimType.valueOfName(communicationsModel));
    }

    /**
     * Returns the report applications for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of report applications for the specified composite.
     * @throws WebAppException If the report applications cannot be retrieved.
     */
    public List<Application> getReportApplications(Long userId, Long compositeId) throws WebAppException {

        try {

            getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Report Applications Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Adds a new report application to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param applicationProfileId The long ID of the application profile.
     * @return The new report application that was added.
     * @throws WebAppException If a new a report application cannot be added.
     */
    public Application addReportApplication(Long userId, Long compositeId, Long applicationProfileId) throws WebAppException {

        Application application;
        ReportDevice reportDevice;
        String exceptionTitle = "Add Report Application Failure";

        try {

            Composite composite = getComposite(userId, compositeId);
            reportDevice = composite.getReportDevice();
            application = applicationProfileManager.getApplicationProfile(userId, applicationProfileId).createApplication();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Report applications may not be added to composites with existing executions.");
            }

            validateApplicationName(userId, compositeId, application.getName());
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!reportDevice.canHost(application)) {

            throw new WebAppException(exceptionTitle, String.format("The \"%s\" application is not a report application.", application.getName()));
        }

        reportDevice.getApplications().add(application);

        return application;
    }

    /**
     * Removes the specified report application from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param applicationId The long ID of the application.
     * @throws WebAppException If the report application cannot be removed.
     */
    public void removeReportApplication(Long userId, Long compositeId, Long applicationId) throws WebAppException {

        String exceptionTitle = "Remove Report Application Failure";

        try {

            Composite composite = getComposite(userId, compositeId);
            Application application = getReportApplication(userId, compositeId, applicationId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Report applications may not be removed from composites with existing executions.");
            }

            entityManager.remove(application);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified report applications from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param applicationIds The list of IDs of the report applications.
     * @throws WebAppException If the report applications cannot be removed.
     */
    public void removeReportApplications(Long userId, Long compositeId, List<Long> applicationIds) throws WebAppException {

        List<Application> applications;
        String exceptionTitle = "Remove Report Applications Failure";

        try {

            Composite composite = getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Report applications may not be removed from composites with existing executions.");
            }

            applications = buildReportApplicationList(getReportApplications(userId, compositeId), applicationIds);
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
    private List<Application> buildReportApplicationList(List<Application> applicationList, List<Long> applicationIds) throws WebAppException {

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

                String message = String.format("No report application with ID \"%d\" could be found.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (applicationMap.containsKey(id)) {

                    message = "The same report application ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Report Application List Failure", message, status);
            }

            applicationMap.put(id, null);
        }

        return applications;
    }

    /**
     * Returns the specified report application.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param applicationId The long ID of the application.
     * @return The report application with the specified attributes.
     * @throws WebAppException If no such report application exists.
     */
    public Application getReportApplication(Long userId, Long compositeId, Long applicationId) throws WebAppException {

        String exceptionTitle = "Get Report Application Failure";

        try {

            getComposite(userId, compositeId).getReportDevice();
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Application> applications = entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_APPLICATION_ID, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.APPLICATION_ID, applicationId)
                .getResultList();

        if (applications.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No report application with ID \"%d\" could be found.", applicationId), Response.Status.NOT_FOUND);
        }

        return applications.get(0);
    }

    /**
     * Validates the specified application name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param applicationName The string application name to validate.
     * @throws WebAppException If the application name is not unique.
     */
    public void validateApplicationName(Long userId, Long compositeId, String applicationName) throws WebAppException {

        List<Application> applications = entityManager.createNamedQuery(Query.APPLICATIONS_FROM_USER_ID_COMPOSITE_ID_APPLICATION_NAME, Application.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.APPLICATION_NAME, applicationName)
                .getResultList();

        if (!applications.isEmpty()) {

            throw new WebAppException("Validate Application Failure", String.format("A report application with the name \"%s\" is already registered.", applicationName));
        }
    }

    /**
     * Returns the specified application parameter.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @return The application parameter with the specified attributes.
     * @throws WebAppException If no such application parameter exists.
     */
    public ApplicationParameter getReportApplicationParameter(Long userId, Long compositeId, Long applicationId, Long applicationParameterId) throws WebAppException {

        String exceptionTitle = "Get Report Application Parameter Failure";

        try {

            getReportApplication(userId, compositeId, applicationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<ApplicationParameter> parameters = entityManager.createNamedQuery(
                Query.APPLICATION_PARAMETERS_FROM_USER_ID_COMPOSITE_ID_APPLICATION_ID_APPLICATION_PARAMETER_ID, ApplicationParameter.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
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
     * @param applicationId The long ID of the application.
     * @param applicationParameterId The long ID of the application parameter.
     * @param value The string value to set.
     * @throws WebAppException If the application parameter cannot be updated.
     */
    public void updateReportApplicationParameter(Long userId, Long compositeId, Long applicationId, Long applicationParameterId, String value) throws WebAppException {

        ApplicationParameter parameter;
        String exceptionTitle = "Update Report Application Parameter Failure";

        try {

            Composite composite = getComposite(userId, compositeId);
            parameter = getReportApplicationParameter(userId, compositeId, applicationId, applicationParameterId);
            value = (value != null) ? value : parameter.getValue();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Report application parameters may not be modified for composites with existing executions.");
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        parameter.setValue(value);
    }
}
