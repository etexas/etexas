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
package org.etexascode.webapp.datamodel;

import java.io.Serializable;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

/**
 * A database entity with a unique ID number.
 * 
 * @author bbadillo
 * @author emyers
 */
@MappedSuperclass
@XmlSeeAlso({ Composite.class, User.class })
public abstract class AbstractEntity implements Serializable {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The ID number for this database entity. */
    @Id
    @GeneratedValue(strategy = GenerationType.TABLE, generator = "hilo_sequence_generator")
    @GenericGenerator(name = "hilo_sequence_generator", strategy = "org.hibernate.id.enhanced.TableGenerator", parameters = {
            @Parameter(name = "table_name", value = "hilo_sequence"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "25"),
            @Parameter(name = "optimizer", value = "hilo") })
    private Long id;

    /* needed for serialization */
    public AbstractEntity() {}

    /**
     * Returns the ID number for this database entity.
     * 
     * @return The long ID number for this database entity.
     */
    public Long getId() {

        return id;
    }

    /**
     * Sets the ID number for this database entity.
     * 
     * @param id The long ID number to set.
     */
    public void setId(Long id) {

        this.id = id;
    }

    @Override
    public int hashCode() {

        return (id != null) ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object object) {

        if (!(object instanceof AbstractEntity)) {

            return false;
        }

        AbstractEntity entity = (AbstractEntity)object;
        return (this.id != null) ? this.id.equals(entity.id) : entity.id == null;
    }

    @Override
    public String toString() {

        return String.format("%s[ id=%s ]", this.getClass().getName(), id);
    }
}