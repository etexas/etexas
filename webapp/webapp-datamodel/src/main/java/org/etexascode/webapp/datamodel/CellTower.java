/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
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
package org.etexascode.webapp.datamodel;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A cellular communications tower.
 * 
 * @author ttevendale
 * @author emyers
 */
@Entity
@Table(name = "cell_towers")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class CellTower extends AbstractEntity implements ICopyable<CellTower> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The cellular provider for this cell tower. */
    private String provider;

    /** The x coordinate of this cell tower. */
    private double x;

    /** The y coordinate of this cell tower. */
    private double y;

    /** The z coordinate of this cell tower. */
    private double z;

    /**
     * Returns the cellular provider for this cell tower.
     * 
     * @return The string cellular provider for this cell tower.
     */
    public String getProvider() {

        return provider;
    }

    /**
     * Sets the cellular provider for this cell tower.
     * 
     * @param provider The string cellular provider to set.
     */
    public void setProvider(String provider) {

        this.provider = provider;
    }

    /**
     * Returns the x coordinate (cm) of this cell tower.
     * 
     * @return The double x coordinate (cm) of this cell tower.
     */
    public double getX() {

        return x;
    }

    /**
     * Sets the x coordinate (cm) of this cell tower.
     * 
     * @param x The double x coordinate (cm) to set.
     */
    public void setX(double x) {

        this.x = x;
    }

    /**
     * Returns the y coordinate (cm) of this cell tower.
     * 
     * @return The double y coordinate (cm) of this cell tower.
     */
    public double getY() {

        return y;
    }

    /**
     * Sets the y coordinate (cm) of this cell tower.
     * 
     * @param y The double y coordinate (cm) to set.
     */
    public void setY(double y) {

        this.y = y;
    }

    /**
     * Returns the z coordinate (cm) of this cell tower.
     * 
     * @return The double z coordinate (cm) of this cell tower.
     */
    public double getZ() {

        return z;
    }

    /**
     * Sets the z coordinate (cm) of this cell tower.
     * 
     * @param z The double z coordinate (cm) to set.
     */
    public void setZ(double z) {

        this.z = z;
    }

    /**
     * The method to copy this cell tower
     * 
     * @return The copied cell tower
     */
    @Override
    public CellTower copy() {
        CellTower cellTower = new CellTower();
        cellTower.setProvider(this.getProvider());
        cellTower.setX(this.getX());
        cellTower.setY(this.getY());
        cellTower.setZ(this.getZ());

        return cellTower;
    }
}