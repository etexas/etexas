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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Path;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.Response;

import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.Detector;
import org.etexascode.webapp.datamodel.FileData;
import org.etexascode.webapp.datamodel.LaneManagerData;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.datamodel.SimulationType;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.ra.api.SimFactory;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException.SimResourceErrorType;
import org.slf4j.LoggerFactory;

/**
 * The EJB to manage simulation database transactions.
 * 
 * @author bbadillo
 * @author dranker
 * @author bmauldon
 * @author ttevendale
 * @author emyers
 */
@Stateless
public class SimulationManager {

    /** The composite transaction manager. */
    @Inject
    private CompositeManager compositeManager;

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /** The simulation factory. */
    @Resource(name = "genericAdapter")
    private SimFactory genericFactory;

    /**
     * Returns the simulations for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of simulations for the specified composite.
     * @throws WebAppException If the simulations cannot be retrieved.
     */
    public List<Simulation> getSimulations(Long userId, Long compositeId) throws WebAppException {

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Simulations Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.SIMULATIONS_FROM_USER_ID_COMPOSITE_ID, Simulation.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Adds a new simulation to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @param templatePath The simulation template path to use.
     * @return The new simulation that was added.
     * @throws WebAppException If a new simulation cannot be added.
     */
    public Simulation addSimulation(Long userId, Long compositeId, String simulationName, Double x, Double y, Path templatePath) throws WebAppException {

        Path fileName = templatePath.getFileName();
        String exceptionTitle = "Add Simulation Failure";

        if (fileName == null) {

            throw new WebAppException(exceptionTitle, "File does not exist");
        }
        String templateName = fileName.toString();
        templateName = templateName.substring(0, templateName.lastIndexOf('.'));

        byte[] simulationData;

        try (RandomAccessFile templateFile = new RandomAccessFile(templatePath.toString(), "r")) {

            // NOTE: ablatt - this throws a null pointer exception when the file does not exist
            simulationData = new byte[(int)templateFile.length()];

            if (templateFile.read(simulationData) < simulationData.length) {

                LoggerFactory.getLogger(SimulationManager.class).error("The template file was not properly read.");
                throw new IOException();
            }
        }
        catch (IOException exception) {

            String message = (exception instanceof FileNotFoundException)
                    ? String.format("The \"%s\" template file could not be loaded.", templateName)
                    : String.format("An unknown error occurred while loading the \"%s\" template file.", templateName);

            throw new WebAppException(exceptionTitle, message);
        }

        return addSimulation(userId, compositeId, simulationName, x, y, SimulationType.TEXAS, String.format("Template: %s", templateName), simulationData);
    }

    /**
     * Adds a new simulation to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @param type The simulation type to set.
     * @param source The string simulation source to set.
     * @param data The bytes of simulation file data to set.
     * @return The new simulation that was added.
     * @throws WebAppException If a new simulation cannot be added.
     */
    public Simulation addSimulation(Long userId, Long compositeId, String simulationName, Double x, Double y, SimulationType type, String source, byte[] data) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Simulation Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "New simulations may not be added to composites with existing executions.");
            }

            validateSimulationName(userId, compositeId, simulationName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        Simulation simulation = new Simulation();
        simulation.setName(simulationName);
        simulation.setType(type);
        simulation.setSource(source);
        simulation.getFileData().setData(data);
        composite.getSimulations().add(simulation);
        entityManager.flush();

        try {

            buildSimulation(simulation);
            simulation.setLocation(x, y);
        }
        catch (WebAppException exception) {

            composite.getSimulations().remove(simulation);
            entityManager.remove(simulation);

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        return simulation;
    }

    /**
     * Builds the specified simulation.
     * 
     * @param simulation The simulation to build.
     * @throws WebAppException If the simulation cannot be built.
     */
    private void buildSimulation(Simulation simulation) throws WebAppException {

        try {

            SimulatorInterface simulator = SimulationBuilder.buildSimulator(genericFactory, simulation);

            try {

                StaticData staticData = simulator.getStaticData();
                simulation.getLaneManagerData().setLaneManager(staticData.getLaneManager());

                SimMetaData metaData = staticData.getMetaData();
                simulation.setWidth(metaData.getSimWidth());
                simulation.setHeight(metaData.getSimHeight());
                simulation.setMaximumSteps(metaData.getMaxSteps());
                simulation.setStepSize(metaData.getStepSize());

                simulator.close();
            }
            catch (RemoteException exception) {

                LoggerFactory.getLogger(SimulationManager.class).error("", exception);
                throw new SimResourceException("A remote exception occurred while getting the metadata or closing the simulator.", SimResourceErrorType.REMOTE);
            }
        }
        catch (SimResourceException exception) {

            entityManager.remove(simulation);
            entityManager.flush();

            LoggerFactory.getLogger(SimulationManager.class).error("", exception);
            SimResourceErrorType errorType = exception.getErrorType();
            String message = (errorType == SimResourceErrorType.ALLOCATE)
                    ? "A connection could not be allocated for the new simulation."
                    : "The simulation could not be built.";

            throw new WebAppException("Build Simulation Failure", message);
        }
    }

    /**
     * Removes the specified simulation from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @throws WebAppException If the simulation cannot be removed.
     */
    public void removeSimulation(Long userId, Long compositeId, Long simulationId) throws WebAppException {

        try {

            removeSimulations(userId, compositeId, Arrays.asList(simulationId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Simulation Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified simulations from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationIds The list of IDs of the simulations.
     * @throws WebAppException If the simulations cannot be removed.
     */
    public void removeSimulations(Long userId, Long compositeId, List<Long> simulationIds) throws WebAppException {

        List<Simulation> simulations;
        String exceptionTitle = "Remove Simulations Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Simulations may not be removed from composites with existing executions.");
            }

            simulations = buildSimulationList(getSimulations(userId, compositeId), simulationIds);
            composite.getSimulations().removeAll(simulations);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (Simulation simulation : simulations) {

            entityManager.remove(simulation);
        }

        entityManager.createNamedQuery(Query.DELETE_LANE_MAPPINGS_FROM_SIMULATION_LIST)
                .setParameter(QueryParameter.SIMULATION_LIST, simulationIds)
                .executeUpdate();

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of simulations from the source list with the specified IDs.
     * 
     * @param simulationList The list of source simulations.
     * @param simulationIds The list of simulation IDs to include.
     * @return A list of simulations from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<Simulation> buildSimulationList(List<Simulation> simulationList, List<Long> simulationIds) throws WebAppException {

        HashMap<Long, Simulation> simulationMap = new HashMap<>();

        for (Simulation simulation : simulationList) {

            if (simulationIds.contains(simulation.getId())) {

                simulationMap.put(simulation.getId(), simulation);
            }
        }

        ArrayList<Simulation> simulations = new ArrayList<>(simulationMap.values());

        for (Long id : simulationIds) {

            Simulation simulation = simulationMap.get(id);

            if (simulation == null) {

                String message = String.format("No simulation with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (simulationMap.containsKey(id)) {

                    message = "The same simulation ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Simulation List Failure", message, status);
            }

            simulationMap.put(id, null);
        }

        return simulations;
    }

    /**
     * Returns the specified simulation. The method should only be used when user and composite IDs
     * are not available (i.e. the file service is the source of the request).
     * 
     * @param simulationId The long ID of the simulation.
     * @return The simulation with the specified attributes.
     * @throws WebAppException If no such simulation exists.
     */
    public Simulation getSimulation(Long simulationId) throws WebAppException {

        Simulation simulation = entityManager.find(Simulation.class, simulationId);

        if (simulation == null) {

            throw new WebAppException("Get Simulation Failure", String.format("No simulation with ID \"%d\" could be found.", simulationId), Response.Status.NOT_FOUND);
        }

        return simulation;
    }

    /**
     * Returns the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return The simulation with the specified attributes.
     * @throws WebAppException If no such simulation exists.
     */
    public Simulation getSimulation(Long userId, Long compositeId, Long simulationId) throws WebAppException {

        String exceptionTitle = "Get Simulation Failure";

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Simulation> simulations = entityManager.createNamedQuery(Query.SIMULATIONS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID, Simulation.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.SIMULATION_ID, simulationId)
                .getResultList();

        if (simulations.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No simulation with ID \"%d\" could be found in the composite.", simulationId), Response.Status.NOT_FOUND);
        }

        return simulations.get(0);
    }

    /**
     * Adds a copy of the specified simulation to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param targetCompositeId The long ID of the target composite.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @return The new simulation copy that was added.
     * @throws WebAppException If a new simulation copy cannot be added.
     */
    public Simulation copySimulation(Long userId, Long compositeId, Long simulationId, Long targetCompositeId, String simulationName, Double x, Double y) throws WebAppException {

        Composite composite;
        Simulation original;
        String exceptionTitle = "Copy Simulation Failure";

        try {

            original = getSimulation(userId, compositeId, simulationId);
            composite = compositeManager.getComposite(userId, targetCompositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Simulations may not be copied to composites with existing executions.");
            }

            validateSimulationName(userId, targetCompositeId, simulationName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        Simulation simulation = original.copy();
        simulation.setName(simulationName);
        simulation.setLocation(x, y);
        composite.getSimulations().add(simulation);

        return simulation;
    }

    /**
     * Updates the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param simulationName The string simulation name to set.
     * @param x The x coordinate of the simulation to set.
     * @param y The y coordinate of the simulation to set.
     * @throws WebAppException If the simulation cannot be updated.
     */
    public void updateSimulation(Long userId, Long compositeId, Long simulationId, String simulationName, Double x, Double y) throws WebAppException {

        Simulation simulation;
        String exceptionTitle = "Update Simulation Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            simulation = getSimulation(userId, compositeId, simulationId);
            simulationName = (simulationName != null) ? simulationName : simulation.getName();
            x = (x != null) ? x : simulation.getX();
            y = (y != null) ? y : simulation.getY();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Simulations may not be modified in composites with existing executions.");
            }

            if (!simulation.getName().equalsIgnoreCase(simulationName)) {

                validateSimulationName(userId, compositeId, simulationName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        simulation.setName(simulationName);
        simulation.setLocation(x, y);
    }

    /**
     * Validates the specified simulation name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationName The string simulation name to validate.
     * @throws WebAppException If the simulation name is not unique.
     */
    public void validateSimulationName(Long userId, Long compositeId, String simulationName) throws WebAppException {

        List<Simulation> simulations = entityManager.createNamedQuery(Query.SIMULATIONS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_NAME, Simulation.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.SIMULATION_NAME, simulationName)
                .getResultList();

        if (!simulations.isEmpty()) {

            throw new WebAppException("Validate Simulation Name Failure", String.format("A simulation with the name \"%s\" already exists in the composite.", simulationName));
        }
    }

    /**
     * Returns the detectors for the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return A list of detectors for the specified simulation.
     * @throws WebAppException If the detectors cannot be retrieved.
     */
    public List<Detector> getDetectors(Long userId, Long compositeId, Long simulationId) throws WebAppException {

        try {

            getSimulation(userId, compositeId, simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Detectors Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.DETECTORS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID, Detector.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.SIMULATION_ID, simulationId)
                .getResultList();
    }

    /**
     * Adds a new detector to the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param laneId The integer lane number to set.
     * @param width The double detector width (cm) to set.
     * @param height The double detector height (cm) to set.
     * @param distance The double distance from the stop line (cm) to set.
     * @return The new detector that was added.
     * @throws WebAppException If a new detector cannot be added.
     */
    public Detector addDetector(Long userId, Long compositeId, Long simulationId, Integer laneId, Double width, Double height, Double distance) throws WebAppException {

        Simulation simulation;
        String exceptionTitle = "Add Detector Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            simulation = getSimulation(userId, compositeId, simulationId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "New detectors may not be added to simulations in composites with existing executions.");
            }

            ILane lane = validateLane(userId, compositeId, simulationId, laneId);
            validateDetectorDistance(lane, distance);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        Detector detector = new Detector();
        detector.setLane(laneId);
        detector.setWidth(width);
        detector.setHeight(height);
        detector.setDistance(distance);
        simulation.getDetectors().add(detector);

        return detector;
    }

    /**
     * Removes the specified detector from the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorId The long ID of the detector.
     * @throws WebAppException If the detector cannot be removed.
     */
    public void removeDetector(Long userId, Long compositeId, Long simulationId, Long detectorId) throws WebAppException {

        try {

            removeDetectors(userId, compositeId, simulationId, Arrays.asList(detectorId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Detector Failure", exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Removes the specified detectors from the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorIds The list of IDs of the detectors.
     * @throws WebAppException If the detectors cannot be removed.
     */
    public void removeDetectors(Long userId, Long compositeId, Long simulationId, List<Long> detectorIds) throws WebAppException {

        List<Detector> detectors;
        String exceptionTitle = "Remove Detectors Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Detectors may not be removed from simulations in composites with existing executions.");
            }

            detectors = buildDetectorList(getDetectors(userId, compositeId, simulationId), detectorIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (Detector detector : detectors) {

            entityManager.remove(detector);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of detectors from the source list with the specified IDs.
     * 
     * @param detectorList The list of source detectors.
     * @param detectorIds The list of detector IDs to include.
     * @return A list of detectors from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<Detector> buildDetectorList(List<Detector> detectorList, List<Long> detectorIds) throws WebAppException {

        HashMap<Long, Detector> detectorMap = new HashMap<>();

        for (Detector detector : detectorList) {

            if (detectorIds.contains(detector.getId())) {

                detectorMap.put(detector.getId(), detector);
            }
        }

        ArrayList<Detector> detectors = new ArrayList<>(detectorMap.values());

        for (Long id : detectorIds) {

            Detector detector = detectorMap.get(id);

            if (detector == null) {

                String message = String.format("No detector with ID \"%d\" could be found in the simulation.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (detectorMap.containsKey(id)) {

                    message = "The same detector ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Detector List Failure", message, status);
            }

            detectorMap.put(id, null);
        }

        return detectors;
    }

    /**
     * Returns the specified detector.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorId The long ID of the detector.
     * @return The detector with the specified attributes.
     * @throws WebAppException If no such detector exists.
     */
    public Detector getDetector(Long userId, Long compositeId, Long simulationId, Long detectorId) throws WebAppException {

        String exceptionTitle = "Get Detector Failure";

        try {

            getSimulation(userId, compositeId, simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<Detector> detectors = entityManager.createNamedQuery(Query.DETECTORS_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID_DETECTOR_ID, Detector.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.SIMULATION_ID, simulationId)
                .setParameter(QueryParameter.DETECTOR_ID, detectorId)
                .getResultList();

        if (detectors.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No detector with ID \"%d\" could be found in the simulation.", detectorId), Response.Status.NOT_FOUND);
        }

        return detectors.get(0);
    }

    /**
     * Updates the specified detector.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param detectorId The long ID of the detector.
     * @param laneId The integer lane number to set.
     * @param width The double detector width (cm) to set.
     * @param height The double detector height (cm) to set.
     * @param distance The double distance from the stop line (cm) to set.
     * @throws WebAppException If the detector cannot be updated.
     */
    public void updateDetector(Long userId, Long compositeId, Long simulationId, Long detectorId, Integer laneId, Double width, Double height, Double distance) throws WebAppException {

        Detector detector;
        String exceptionTitle = "Update Detector Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            detector = getDetector(userId, compositeId, simulationId, detectorId);
            laneId = (laneId != null) ? laneId : detector.getLane();
            width = (width != null) ? width : detector.getWidth();
            height = (height != null) ? height : detector.getHeight();
            distance = (distance != null) ? distance : detector.getDistance();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Detectors may not be modified for simulations in composites with existing executions.");
            }

            ILane lane = validateLane(userId, compositeId, simulationId, laneId);
            validateDetectorDistance(lane, distance);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        detector.setLane(laneId);
        detector.setWidth(width);
        detector.setHeight(height);
        detector.setDistance(distance);
    }

    /**
     * Validates the detector distance.
     * 
     * @param lane The lane to check the detector distance against.
     * @param distance The detector's distance from the lane's stop line.
     * @throws WebAppException If the detector distance is longer than the lane's length.
     */
    public void validateDetectorDistance(ILane lane, Double distance) throws WebAppException {

        ILaneNode previousNode = null;
        double totalDistance = 0;

        for (ILaneNode laneNode : lane.getLaneGeomList()) {

            if (previousNode == null) {

                previousNode = laneNode;
            }
            else {

                totalDistance += Math
                        .sqrt(Math.pow(laneNode.getX() - previousNode.getX(), 2) + Math.pow(laneNode.getY() - previousNode.getY(), 2) + Math.pow(laneNode.getZ() - previousNode.getZ(), 2));
            }
        }

        totalDistance = Math.floor(totalDistance * 100) / 100;

        if (totalDistance < distance) {

            throw new WebAppException("Validate Detector Distance Failure",
                    "The detector's distance from the current lane's stop line is longer than the lane's total length (" + totalDistance + " cm).");
        }
    }

    /**
     * Validates the specified lane number.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @param lane The integer lane number to validate.
     * @return The validated lane.
     * @throws WebAppException If no such lane exists.
     */
    public ILane validateLane(Long userId, Long compositeId, Long simulationId, Integer lane) throws WebAppException {

        List<ILane> lanes;
        String exceptionTitle = "Validate Lane Failure";

        try {

            lanes = getLanes(userId, compositeId, simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (int i = 0; i < lanes.size(); i++) {

            if (lanes.get(i).getLaneId() == lane) {

                return lanes.get(i);
            }
        }

        throw new WebAppException(exceptionTitle, String.format("Lane number \"%d\" is not a valid lane.", lane));
    }

    /**
     * Returns the file data for the specified simulation. The method should only be used when user
     * and composite IDs are not available (i.e. the file service is the source of the request).
     * 
     * @param simulationId The long ID of the simulation.
     * @return The bytes of file data for the specified simulation.
     * @throws WebAppException If the file data cannot be retrieved.
     */
    public byte[] getFileData(Long simulationId) throws WebAppException {

        String exceptionTitle = "Get File Data Failure";

        try {

            getSimulation(simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<FileData> fileData = entityManager.createNamedQuery(Query.FILE_DATA_FROM_SIMULATION_ID, FileData.class)
                .setParameter(QueryParameter.SIMULATION_ID, simulationId)
                .getResultList();

        if (fileData.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No file data for simulation \"%d\" could be found.", simulationId), Response.Status.NOT_FOUND);
        }

        return fileData.get(0).getData();
    }

    /**
     * Updates the file data for the specified simulation. The method should only be used when user
     * and composite IDs are not available (i.e. the file service is the source of the request).
     * 
     * @param simulationId The long ID of the simulation.
     * @param data The bytes of simulation file data to set.
     * @throws WebAppException If the simulation cannot be updated.
     */
    public void updateFileData(Long simulationId, byte[] data) throws WebAppException {

        String exceptionTitle = "Update File Data Failure";
        Simulation simulation = entityManager.find(Simulation.class, simulationId);

        if (simulation == null) {

            throw new WebAppException(exceptionTitle, String.format("No simulation with ID \"%d\" could be found.", simulationId), Response.Status.NOT_FOUND);
        }

        simulation.getFileData().setData(data);

        try {

            buildSimulation(simulation);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }
    }

    /**
     * Returns the lanes for the specified simulation.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return A list of lanes for the specified simulation.
     * @throws WebAppException If the lanes cannot be retrieved.
     */
    public List<ILane> getLanes(Long userId, Long compositeId, Long simulationId) throws WebAppException {

        LaneManagerData laneManagerData;

        try {

            laneManagerData = getLaneManagerData(userId, compositeId, simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Lanes Failure", exception.getMessage(), exception.getStatus());
        }

        List<ILane> lanes = new ArrayList<>();

        for (ILane lane : laneManagerData.getLaneManager()) {

            lanes.add(lane);
        }

        return lanes;
    }

    /**
     * Returns the specified lane manager data.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param simulationId The long ID of the simulation.
     * @return The lane manager data with the specified attributes.
     * @throws WebAppException If no such lane manager data exists.
     */
    public LaneManagerData getLaneManagerData(Long userId, Long compositeId, Long simulationId) throws WebAppException {

        String exceptionTitle = "Get Lane Manager Data Failure";

        try {

            getSimulation(userId, compositeId, simulationId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<LaneManagerData> laneManagerData = entityManager.createNamedQuery(Query.LANE_MANAGER_DATA_FROM_USER_ID_COMPOSITE_ID_SIMULATION_ID, LaneManagerData.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.SIMULATION_ID, simulationId)
                .getResultList();

        if (laneManagerData.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No lane manager data for simulation \"%d\" could be found.", simulationId), Response.Status.NOT_FOUND);
        }

        return laneManagerData.get(0);
    }
}