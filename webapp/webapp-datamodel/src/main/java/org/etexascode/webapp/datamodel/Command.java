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
 * An execution command.
 * 
 * @author emyers
 */
@Entity
@Table(name = "commands")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Command extends AbstractEntity {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The ID of the parent execution. */
    @Column(name = "execution")
    private long executionId;

    /** The intersection for this command. */
    @Column(name = "intersection")
    private int intersectionId;

    /** The submission time (s) for this command. */
    @Column(name = "time")
    private double submissionTime;

    /** The source of this command. */
    private String source;

    /** The description for this command. */
    private String description;

    /**
     * Returns the ID of the parent execution.
     * 
     * @return The long ID of the parent execution.
     */
    public long getExecutionId() {

        return executionId;
    }

    /**
     * Sets the ID of the parent execution.
     * 
     * @param executionId The long parent execution ID to set.
     */
    public void setExecutionId(long executionId) {

        this.executionId = executionId;
    }

    /**
     * Returns the intersection ID for this command.
     * 
     * @return The integer intersection ID for this command.
     */
    public int getIntersectionId() {

        return intersectionId;
    }

    /**
     * Sets the intersection ID for this command.
     * 
     * @param intersectionId The integer intersection ID to set.
     */
    public void setIntersectionId(int intersectionId) {

        this.intersectionId = intersectionId;
    }

    /**
     * Returns the submission time (s) for this command.
     * 
     * @return The double submission time (s) for this command.
     */
    public double getSubmissionTime() {

        return submissionTime;
    }

    /**
     * Sets the submission time (s) for this command.
     * 
     * @param submissionTime The double submission time (s) to set.
     */
    public void setSubmissionTime(double submissionTime) {

        this.submissionTime = submissionTime;
    }

    /**
     * Returns the source of this command.
     * 
     * @return The string source of this command.
     */
    public String getSource() {

        return source;
    }

    /**
     * Sets the source of this command.
     * 
     * @param source The string source to set.
     */
    public void setSource(String source) {

        this.source = source;
    }

    /**
     * Returns the description for this command.
     * 
     * @return The string description for this command.
     */
    public String getDescription() {

        return description;
    }

    /**
     * Sets the description for this command.
     * 
     * @param description The string description to set.
     */
    public void setDescription(String description) {

        this.description = description;
    }
}
