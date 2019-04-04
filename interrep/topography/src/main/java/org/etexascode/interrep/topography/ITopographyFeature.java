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

/**
 * Interface for topography features.
 * 
 * @author ttevendale
 */
public interface ITopographyFeature {

    /**
     * Getter for the feature ID.
     * 
     * @return The feature ID.
     */
    public long getFeatureID();

    /**
     * Finds the intersection point from the source to destination with this feature if it exists.
     * 
     * @param source The source node.
     * @param destination The destination node.
     * @return The intersection vector or null if it doesn't exist.
     */
    public Vector3 lineIntersection(Vector3 source, Vector3 destination);

    /**
     * Checks to see if the point is contained in this feature.
     * 
     * @param point The point to check.
     * @return True if the point is within the box, false otherwise.
     */
    public boolean contains(Vector3 point);

    /**
     * Checks to see if the point is contained in this feature.
     * 
     * @param x The x coordinate to check.
     * @param y The y coordinate to check.
     * @param z The z coordinate to check.
     * @return True if the point is within the box, false otherwise.
     */
    public boolean contains(double x, double y, double z);
}
