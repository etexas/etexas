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
package org.etexascode.interrep.datamodel.interfaces;

/**
 * An interface for comparing an entity based solely on its Id or getting an Id for the entity This
 * is necessary because the Id is the only aspect of an object which is guaranteed to be the same
 * over time.
 * 
 * @author ablatt
 */
public interface IDable {

    /**
     * Tests if the 2 entities are the same. This tests for equality against only the id of the
     * object
     * 
     * @param entity The entity to test against
     * @return Does this have the same id as entity?
     */
    public boolean equalsId(IDable entity);

    /**
     * Gets a globally unique id of the entity qualified by its type.
     * 
     * @return The id of this entity
     */
    public String getProperId();
}
