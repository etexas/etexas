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
package org.etexascode.webapp.datamodel;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A mapping between an outbound and an inbound lane to define the flow of traffic among constituent
 * simulations in a composite. Vehicles that would normally exit a simulation on an outbound lane
 * will instead be injected into the corresponding inbound lane when a lane mapping exists.
 * 
 * @author emyers
 */
@Entity
@Table(name = "lane_mappings")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class LaneMapping extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The source simulation for this lane mapping. */
    @Column(name = "source_simulation")
    private long sourceSimulation;

    /** The source lane for this lane mapping. */
    @Column(name = "source_lane")
    private int sourceLane;

    /** The target simulation for this lane mapping. */
    @Column(name = "target_simulation")
    private long targetSimulation;

    /** The target lane for this lane mapping. */
    @Column(name = "target_lane")
    private int targetLane;

    /**
     * Returns the ID of the source simulation for this lane mapping.
     * 
     * @return The long ID of the source simulation for this lane mapping.
     */
    public long getSourceSimulation() {

        return sourceSimulation;
    }

    /**
     * Sets the ID of the source simulation for this lane mapping.
     * 
     * @param sourceSimulation The long ID of the source simulation to set.
     */
    public void setSourceSimulation(long sourceSimulation) {

        this.sourceSimulation = sourceSimulation;
    }

    /**
     * Returns the ID of the source lane for this lane mapping.
     * 
     * @return The integer ID of the source lane for this lane mapping.
     */
    public int getSourceLane() {

        return sourceLane;
    }

    /**
     * Sets the ID of the source lane for this lane mapping.
     * 
     * @param sourceLane The integer ID of the source lane to set.
     */
    public void setSourceLane(int sourceLane) {

        this.sourceLane = sourceLane;
    }

    /**
     * Returns the ID of the target simulation for this lane mapping.
     * 
     * @return The long ID of the target simulation for this lane mapping.
     */
    public long getTargetSimulation() {

        return targetSimulation;
    }

    /**
     * Sets the ID of the target simulation for this lane mapping.
     * 
     * @param targetSimulation The long ID of the target simulation to set.
     */
    public void setTargetSimulation(long targetSimulation) {

        this.targetSimulation = targetSimulation;
    }

    /**
     * Returns the ID of the target lane for this lane mapping.
     * 
     * @return The integer ID of the target lane for this lane mapping.
     */
    public int getTargetLane() {

        return targetLane;
    }

    /**
     * Sets the ID of the target lane for this lane mapping.
     * 
     * @param targetLane The integer ID of the target lane to set.
     */
    public void setTargetLane(int targetLane) {

        this.targetLane = targetLane;
    }
}
