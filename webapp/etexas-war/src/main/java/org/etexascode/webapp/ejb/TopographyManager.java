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
import org.etexascode.webapp.datamodel.topography.Building;
import org.etexascode.webapp.datamodel.topography.TopographyFeature;
import org.etexascode.webapp.exception.WebAppException;

/**
 * The EJB to manage topography transactions.
 * 
 * @author ttevendale
 */
@Stateless
public class TopographyManager {

    /** The database entity manager. */
    @PersistenceContext(unitName = "etexas-pu")
    private EntityManager entityManager;

    /** The composite transaction manager. */
    @Inject
    private CompositeManager compositeManager;

    /**
     * Returns the topography features for the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @return A list of topography features for the specified simulation.
     * @throws WebAppException If the topography features cannot be retrieved.
     */
    public List<TopographyFeature> getTopographyFeatures(Long userId, Long compositeId) throws WebAppException {

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException("Get Topography Features Failure", exception.getMessage(), exception.getStatus());
        }

        return entityManager.createNamedQuery(Query.TOPOGRAPHY_FEATURES_FROM_USER_ID_COMPOSITE_ID, TopographyFeature.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .getResultList();
    }

    /**
     * Returns the specified topography feature.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the topography feature.
     * @return The topography feature with the specified attributes.
     * @throws WebAppException If no such topography feature exists.
     */
    public TopographyFeature getTopographyFeature(Long userId, Long compositeId, Long featureId) throws WebAppException {

        String exceptionTitle = "Get Topography Feature Failure";

        try {

            compositeManager.getComposite(userId, compositeId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        List<TopographyFeature> topographyFeatures = entityManager.createNamedQuery(Query.TOPOGRAPHY_FEATURES_FROM_USER_ID_COMPOSITE_ID_FEATURE_ID, TopographyFeature.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.FEATURE_ID, featureId)
                .getResultList();

        if (topographyFeatures.isEmpty()) {

            throw new WebAppException(exceptionTitle, String.format("No topography feature with ID \"%d\" could be found in the composite.", featureId), Response.Status.NOT_FOUND);
        }

        return topographyFeatures.get(0);
    }

    /**
     * Adds a new building to the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureName The string feature name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param width The double width (cm) to set.
     * @param length The double length (cm) to set.
     * @param height The double height (cm) to set.
     * @return The new building that was added.
     * @throws WebAppException If a new building cannot be added.
     */
    public Building addBuilding(Long userId, Long compositeId, String featureName, Double x, Double y, Double width, Double length, Double height) throws WebAppException {

        Composite composite;
        String exceptionTitle = "Add Building Failure";

        try {

            composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Buildings may not be added to composites with existing executions.");
            }

            validateFeatureName(userId, compositeId, featureName);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        Building building = new Building();
        building.setName(featureName);
        building.setX(x);
        building.setY(y);
        building.setWidth(width);
        building.setLength(length);
        building.setHeight(height);

        composite.getTopographyFeatures().add(building);

        return building;
    }

    /**
     * Returns the specified building.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the topography feature.
     * @return The building with the specified attributes.
     * @throws WebAppException If no such building exists.
     */
    public Building getBuilding(Long userId, Long compositeId, Long featureId) throws WebAppException {

        TopographyFeature feature;
        String exceptionTitle = "Get Building Failure";

        try {

            feature = getTopographyFeature(userId, compositeId, featureId);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        if (!(feature instanceof Building)) {

            throw new WebAppException(exceptionTitle, String.format("The topography feature with ID \"%d\" is not a building.", featureId));
        }

        return (Building)feature;
    }

    /**
     * Updates the specified building.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the topography feature.
     * @param featureName The string feature name to set.
     * @param x The double x coordinate (cm) to set.
     * @param y The double y coordinate (cm) to set.
     * @param width The double width (cm) to set.
     * @param length The double length (cm) to set.
     * @param height The double height (cm) to set.
     * @throws WebAppException If the building cannot be updated.
     */
    public void updateBuilding(Long userId, Long compositeId, Long featureId, String featureName, Double x, Double y, Double width, Double length, Double height) throws WebAppException {

        Building building;
        String exceptionTitle = "Update Building Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);
            building = getBuilding(userId, compositeId, featureId);
            featureName = (featureName != null) ? featureName : building.getName();
            x = (x != null) ? x : building.getX();
            y = (y != null) ? y : building.getY();
            width = (width != null) ? width : building.getWidth();
            length = (length != null) ? length : building.getLength();
            height = (height != null) ? height : building.getHeight();

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Buildings may not be modified for composites with existing executions.");
            }

            if (!building.getName().equalsIgnoreCase(featureName)) {

                validateFeatureName(userId, compositeId, featureName);
            }
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        building.setName(featureName);
        building.setX(x);
        building.setY(y);
        building.setWidth(width);
        building.setLength(length);
        building.setHeight(height);

    }

    /**
     * Removes the specified topography feature from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureId The long ID of the topography feature.
     * @throws WebAppException If the topography feature cannot be removed.
     */
    public void removeTopographyFeature(Long userId, Long compositeId, Long featureId) throws WebAppException {

        try {

            removeTopographyFeatures(userId, compositeId, Arrays.asList(featureId));
        }
        catch (WebAppException exception) {

            throw new WebAppException("Remove Topography Feature Failure", exception.getMessage(), exception.getStatus());
        }

    }

    /**
     * Removes the specified topography features from the specified composite.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureIds The list of IDs of the topography features.
     * @throws WebAppException If the topography features cannot be removed.
     */
    public void removeTopographyFeatures(Long userId, Long compositeId, List<Long> featureIds) throws WebAppException {

        List<TopographyFeature> features;
        String exceptionTitle = "Remove Topography Features Failure";

        try {

            Composite composite = compositeManager.getComposite(userId, compositeId);

            if (!composite.getExecutions().isEmpty()) {

                throw new WebAppException(exceptionTitle, "Topography features may not be removed from composites with existing executions.");
            }

            features = buildTopographyFeatureList(getTopographyFeatures(userId, compositeId), featureIds);
        }
        catch (WebAppException exception) {

            throw new WebAppException(exceptionTitle, exception.getMessage(), exception.getStatus());
        }

        for (TopographyFeature feature : features) {

            entityManager.remove(feature);
        }

        entityManager.flush();
        entityManager.clear();
    }

    /**
     * Returns a list of topography features from the source list with the specified IDs.
     * 
     * @param featureList The list of source topography features.
     * @param featureIds The list of topography feature IDs to include.
     * @return A list of topography features from the source list with the specified IDs.
     * @throws WebAppException If nonexistent or duplicate IDs are provided.
     */
    private List<TopographyFeature> buildTopographyFeatureList(List<TopographyFeature> featureList, List<Long> featureIds) throws WebAppException {

        HashMap<Long, TopographyFeature> featureMap = new HashMap<>();

        for (TopographyFeature feature : featureList) {

            if (featureIds.contains(feature.getId())) {

                featureMap.put(feature.getId(), feature);
            }
        }

        ArrayList<TopographyFeature> features = new ArrayList<>(featureMap.values());

        for (Long id : featureIds) {

            TopographyFeature feature = featureMap.get(id);

            if (feature == null) {

                String message = String.format("No topography feature with ID \"%d\" could be found in the composite.", id);
                Response.Status status = Response.Status.NOT_FOUND;

                if (featureMap.containsKey(id)) {

                    message = "The same topography feature ID may not be listed twice.";
                    status = Response.Status.CONFLICT;
                }

                throw new WebAppException("Build Topography Feature List Failure", message, status);
            }

            featureMap.put(id, null);
        }

        return features;
    }

    /**
     * Validates the specified feature name.
     * 
     * @param userId The long ID of the user.
     * @param compositeId The long ID of the composite.
     * @param featureName The string feature name to validate.
     * @throws WebAppException If the feature name is not unique.
     */
    private void validateFeatureName(Long userId, Long compositeId, String featureName) throws WebAppException {

        List<TopographyFeature> features = entityManager.createNamedQuery(Query.TOPOGRAPHY_FEATURES_FROM_USER_ID_COMPOSITE_ID_FEATURE_NAME, TopographyFeature.class)
                .setParameter(QueryParameter.USER_ID, userId)
                .setParameter(QueryParameter.COMPOSITE_ID, compositeId)
                .setParameter(QueryParameter.FEATURE_NAME, featureName)
                .getResultList();

        if (!features.isEmpty()) {

            throw new WebAppException("Validate Topography Feature Name Failure", String.format("A topography feature with the name \"%s\" already exists in the composite.", featureName));
        }
    }

}
