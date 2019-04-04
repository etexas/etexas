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
package org.etexascode.xmlclasses;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A predefined simulation template.
 * 
 * @author ablatt
 * @author emyers
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class SimulationTemplate implements Serializable {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /**
     * The predefined simulation template values.
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    private static final List<SimulationTemplate> VALUES = Collections.unmodifiableList(Arrays.asList(new SimulationTemplate[] {

            new SimulationTemplate("exam_01", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_02", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_03", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_04", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_05", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_06", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_07", "4X4  LIGHT TRAFFIC"),
            new SimulationTemplate("exam_08", "5X4  HEAVY TRAFFIC"),
            new SimulationTemplate("exam_09", "5X4  HEAVY TRAFFIC"),
            new SimulationTemplate("exam_10", "5X4  HEAVY TRAFFIC"),
            new SimulationTemplate("exam_11", "COMPACT DIAMOND"),
            new SimulationTemplate("exam_12", "COMPACT DIAMOND"),
            new SimulationTemplate("exam_13", "EX1  STANDARD DIAMOND"),
            new SimulationTemplate("exam_14", "EX1  STANDARD DIAMOND"),
            new SimulationTemplate("exam_15", "EX1  STANDARD DIAMOND"),
            new SimulationTemplate("exam_16", "EX1  STANDARD DIAMOND"),
            new SimulationTemplate("exam_17", "EX1  STANDARD DIAMOND"),
            new SimulationTemplate("exam_18", "STANDARD DIAMOND FREE U-TURNS"),
            new SimulationTemplate("exam_19", "5X5  HEAVY TRAFFIC"),
            new SimulationTemplate("exam_20", "5X5  HEAVY TRAFFIC")
    }));

    /** The name of this template. */
    private String name;

    /** The description of this template. */
    private String description;

    /* required constructor */
    public SimulationTemplate() {}

    /**
     * Constructs a new <code>SimulationTemplate</code> with the specified name and description.
     * 
     * @param name The string name to set.
     * @param description The string description to set.
     */
    public SimulationTemplate(String name, String description) {

        this.name = name;
        this.description = description;
    }

    /**
     * Returns the name of this template.
     * 
     * @return The string name of this template.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this template.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the description of this template.
     * 
     * @return The string description of this template.
     */
    public String getDescription() {

        return description;
    }

    /**
     * Sets the description of this template.
     * 
     * @param description The string description to set.
     */
    public void setDescription(String description) {

        this.description = description;
    }

    /**
     * Returns a list of the predefined simulation template values. Attempts to modify the list will
     * result in an <code>UnsupportedOperationException</code>.
     * 
     * @return The list of predefined simulation templates.
     */
    public static List<SimulationTemplate> getValues() {

        return SimulationTemplate.VALUES;
    }
}