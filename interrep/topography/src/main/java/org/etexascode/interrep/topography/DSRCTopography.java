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
package org.etexascode.interrep.topography;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * The DSRC Topography class that will handle DSRC messages (other messages will be ignored).
 * 
 * @author ttevendale
 */
public class DSRCTopography implements ITopography {

    /**
     * The topography features map.
     */
    private Map<Long, ITopographyFeature> features = new HashMap<Long, ITopographyFeature>();

    /**
     * Constructor
     * 
     * @param features The topography features to check against.
     */
    public DSRCTopography(Collection<ITopographyFeature> features) {

        for (ITopographyFeature feature : features) {

            this.features.put(feature.getFeatureID(), feature);
        }
        this.features = Collections.unmodifiableMap(this.features);
    }

    /**
     * Gets the obstruction between the two nodes if one exists.
     * 
     * @param source The source node.
     * @param destination The destination node.
     * @param messageType The message type of the two nodes.
     * @return The obstruction between the two nodes or null if one doesn't exist.
     */
    public ITopographyFeature getObstruction(Vector3 source, Vector3 destination, TopographyMessageType messageType) {

        ITopographyFeature obstruction = null;

        if (messageType.equals(TopographyMessageType.DSRC)) {

            obstruction = handleDSRC(source, destination);
        }

        return obstruction;
    }

    /**
     * Handles the DSRC messages.
     * 
     * @param source The source node to handle.
     * @param destination The destination node to handle.
     * @return The topography feature that is obstructing the message or null.
     */
    private ITopographyFeature handleDSRC(Vector3 source, Vector3 destination) {

        Map<Long, Vector3> obstructionMap = new HashMap<Long, Vector3>();

        for (ITopographyFeature feature : features.values()) {

            Vector3 intersectVector = feature.lineIntersection(source, destination);
            if (intersectVector != null) {

                obstructionMap.put(feature.getFeatureID(), intersectVector);
            }
        }
        ITopographyFeature obstruction = this.getClosestObstruction(obstructionMap, source);

        return obstruction;

    }

    /**
     * Gets the closest obstruction to the source node.
     * 
     * @param intersectsMap The map of intersected obstructions to check the distance of.
     * @param source The source to check the distance from.
     * @return The closest topography obstruction from the source or null if there aren't any.
     */
    private ITopographyFeature getClosestObstruction(Map<Long, Vector3> obstructionMap, Vector3 source) {

        Double distance = null;
        Long id = null;
        for (Entry<Long, Vector3> entry : obstructionMap.entrySet()) {

            double tempDistance = source.distanceTo(entry.getValue());
            if (distance == null || tempDistance < distance) {

                distance = tempDistance;
                id = entry.getKey();
            }
        }
        ITopographyFeature obstruction = features.get(id);

        return obstruction;
    }
}
