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
package org.etexascode.devicedata;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * Class which allows you to identify something via 2 integers Commonly used for identifying a
 * specific lane within the simulation Supports a default to allow different parts of the system to
 * not need to coordinate default values
 * 
 * @author ablatt
 */
public class DualIntIdentifier {

    /**
     * The first identifier. Commonly the Intersection Id
     */
    public final int id1;

    /**
     * The second identifier. Commonly the Lane Id
     */
    public final int id2;

    /**
     * Specifies if this identifier should be treated as a default value.
     */
    public final boolean isDefault;

    /**
     * Constuctor
     * 
     * @param id1 The first identifier to use (typically the Intersection Id)
     * @param id2 The second identifier to use (typically the Lane Id)
     * @param isDefault Is this identifier the default? (in which case id1 and id2 don't matter)
     */
    public DualIntIdentifier(int id1, int id2, boolean isDefault) {
        this.id1 = id1;
        this.id2 = id2;
        this.isDefault = isDefault;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    public int hashCode() {
        if (isDefault) {
            return 0;
        }
        return new HashCodeBuilder(1729, 831).append(id1).append(id2).append(isDefault).toHashCode();
    }

    /**
     * Equality check
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof DualIntIdentifier) {
            DualIntIdentifier dii = (DualIntIdentifier)o;
            if (isDefault) {
                return dii.isDefault;
            }
            else {
                return (isDefault == dii.isDefault) && (id1 == dii.id1) && (id2 == dii.id2);
            }
        }
        else {
            return false;
        }
    }
}
